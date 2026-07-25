# Missing values and value labels --------------------------------------------

# Aliased variableMetadata calls, in chunks, so that a 700-variable file costs
# a handful of requests rather than 700.
ess_variable_codes_raw <- function(vars, cache = TRUE, quiet = FALSE, chunk = 100L) {
  n <- nrow(vars)
  if (n == 0L) {
    return(list())
  }

  selection <- "
    name { en }
    label { en }
    codeList(first: 5000) {
      edges { node { value label { en } isMissing } }
    }"

  starts <- seq(1L, n, by = chunk)
  out <- vector("list", length(starts))

  show_progress <- !quiet && length(starts) > 1L
  if (show_progress) {
    cli::cli_progress_bar(
      "Reading variable metadata", total = length(starts), clear = TRUE
    )
  }

  for (k in seq_along(starts)) {
    idx <- seq(starts[[k]], min(starts[[k]] + chunk - 1L, n))
    items <- lapply(idx, function(i) {
      list(id = vars$variable_id[[i]], version = vars$variable_version[[i]])
    })
    out[[k]] <- ess_alias_perform("variableMetadata", items, selection, cache = cache)
    if (show_progress) {
      cli::cli_progress_update()
    }
  }

  if (show_progress) {
    cli::cli_progress_done()
  }

  unlist(out, recursive = FALSE)
}

# One row per (variable, code). missing_only keeps just the codes the ESS flags
# as missing.
ess_codes_dt <- function(round,
                         variables = NULL,
                         kind = "integrated",
                         missing_only = FALSE,
                         cache = TRUE,
                         quiet = FALSE) {
  vars <- ess_variables(round = round, kind = kind, cache = cache)

  if (!is.null(variables)) {
    if (!is.character(variables)) {
      cli::cli_abort("{.arg variables} must be a character vector of names.")
    }
    keep <- tolower(vars$variable_name) %in% tolower(variables)
    unknown <- setdiff(tolower(variables), tolower(vars$variable_name))
    if (length(unknown) > 0L && !quiet) {
      cli::cli_warn(
        paste0(
          "{cli::qty(length(unknown))}Variable{?s} {.val {unknown}} ",
          "{cli::qty(length(unknown))}{?is/are} not in ESS round {round}; ignored."
        )
      )
    }
    vars <- vars[keep]
  }

  proto <- data.table::data.table(
    variable_name = character(), value = character(),
    label = character(), is_missing = logical()
  )

  if (nrow(vars) == 0L) {
    return(proto)
  }

  results <- ess_variable_codes_raw(vars, cache = cache, quiet = quiet)

  rows <- list()
  for (i in seq_along(results)) {
    v <- results[[i]]
    if (is.null(v)) {
      next
    }
    name <- ess_en(v$name)
    edges <- v$codeList$edges
    if (length(edges) == 0L) {
      next
    }
    for (e in edges) {
      is_missing <- isTRUE(ess_lgl(e$node$isMissing))
      if (missing_only && !is_missing) {
        next
      }
      rows[[length(rows) + 1L]] <- data.table::data.table(
        variable_name = name,
        value = ess_chr(e$node$value),
        label = ess_en(e$node$label),
        is_missing = is_missing
      )
    }
  }

  if (length(rows) == 0L) {
    return(proto)
  }

  data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
}

#' Codes the ESS treats as missing
#'
#' Reports, per variable, which response codes stand for "Refusal", "Don't
#' know", "No answer", "Not applicable" and similar. The codes differ between
#' variables — `ppltrst` uses 77, 88 and 99 while `agea` uses 999 — which is why
#' they are read from the metadata rather than assumed.
#'
#' These are the codes that `recode_missings = TRUE` converts to `NA` on the
#' server, and that [ess_recode_missings()] acts on locally.
#'
#' @param round An ESS round number.
#' @param variables Optionally, restrict to these variable names. Strongly
#'   recommended: without it, metadata for every variable in the file is read.
#' @param kind Which data file of the round, as named by [ess_file_kinds()].
#' @param cache If `TRUE` (the default), reuse catalogue answers for the rest of
#'   the session.
#' @param quiet If `TRUE`, suppress the progress bar.
#'
#' @return A `data.table` with columns `variable_name`, `value` and `label`.
#'
#' @examples
#' \dontrun{
#' ess_missing_codes(11, variables = c("ppltrst", "agea", "gndr"))
#' }
#'
#' @export
ess_missing_codes <- function(round,
                              variables = NULL,
                              kind = "integrated",
                              cache = TRUE,
                              quiet = FALSE) {
  out <- ess_codes_dt(
    round = round, variables = variables, kind = kind,
    missing_only = TRUE, cache = cache, quiet = quiet
  )
  out[, c("variable_name", "value", "label"), with = FALSE][]
}

#' Value labels for ESS variables
#'
#' Returns the response categories of each variable: the codes that appear in
#' the data and what they mean.
#'
#' Parquet files hold codes only — a `gndr` of 1 or 2, not "Male" or "Female" —
#' so this is where the labels come from. That is a real difference from the
#' earlier `essurvey` package, which read SPSS files carrying their own labels.
#'
#' @inheritParams ess_missing_codes
#'
#' @return A `data.table` with columns `variable_name`, `value`, `label` and
#'   `is_missing`.
#'
#' @examples
#' \dontrun{
#' labs <- ess_value_labels(11, variables = "gndr")
#' labs
#' }
#'
#' @export
ess_value_labels <- function(round,
                             variables = NULL,
                             kind = "integrated",
                             cache = TRUE,
                             quiet = FALSE) {
  ess_codes_dt(
    round = round, variables = variables, kind = kind,
    missing_only = FALSE, cache = cache, quiet = quiet
  )[]
}

#' Recode ESS missing values to NA
#'
#' Converts the codes the ESS flags as missing into `NA`, one variable at a
#' time, using each variable's own missing codes from the metadata catalogue.
#'
#' Asking the API to do this instead — the default `recode_missings = TRUE` of
#' [ess_round()] and [ess_data()] — is faster and needs no metadata requests.
#' Use this function for data fetched with `recode_missings = FALSE`, when you
#' want the original codes for some variables and `NA` for others, or when you
#' need to see which codes were affected.
#'
#' The table is modified **by reference**, in keeping with `data.table`
#' semantics: the object passed in is changed, and returned invisibly so that
#' calls can be chained. Pass `data.table::copy(dt)` to leave the original
#' alone.
#'
#' @param dt A `data.table`, normally from [ess_round()].
#' @param round The ESS round the data came from. When `NULL` (the default) it
#'   is taken from the `essround` column.
#' @param variables Optionally, restrict recoding to these columns. Defaults to
#'   every column of `dt`.
#' @param kind Which data file of the round, as named by [ess_file_kinds()].
#' @param cache If `TRUE` (the default), reuse catalogue answers for the rest of
#'   the session.
#' @param quiet If `TRUE`, suppress the progress bar and the summary.
#'
#' @return `dt`, invisibly and modified in place.
#'
#' @examples
#' \dontrun{
#' # Fetch the original codes, then recode a few variables locally.
#' dt <- ess_round(11, select = c("essround", "cntry", "agea", "ppltrst"),
#'                 recode_missings = FALSE)
#' dt[, .(max_age = max(agea))]         # 999, the "not available" code
#'
#' ess_recode_missings(dt)
#' dt[, .(max_age = max(agea, na.rm = TRUE))]
#' }
#'
#' @export
ess_recode_missings <- function(dt,
                                round = NULL,
                                variables = NULL,
                                kind = "integrated",
                                cache = TRUE,
                                quiet = FALSE) {
  if (!data.table::is.data.table(dt)) {
    cli::cli_abort(c(
      "{.arg dt} must be a {.cls data.table}.",
      "i" = "Got {.cls {class(dt)}}. {.fn data.table::setDT} converts in place."
    ))
  }

  if (is.null(round)) {
    if (!"essround" %in% names(dt)) {
      cli::cli_abort(c(
        "Cannot tell which ESS round {.arg dt} is from.",
        "i" = "Pass {.arg round}, or keep the {.field essround} column when selecting."
      ))
    }
    rounds <- sort(unique(stats::na.omit(dt$essround)))
    if (length(rounds) != 1L) {
      cli::cli_abort(c(
        "{.arg dt} holds {length(rounds)} ESS round{?s}, whose missing codes may differ.",
        "i" = "Recode each round separately, then stack them."
      ))
    }
    round <- as.integer(rounds[[1L]])
  }

  cols <- if (is.null(variables)) names(dt) else intersect(variables, names(dt))

  if (length(cols) == 0L) {
    if (!quiet) {
      cli::cli_alert_info("No matching columns; nothing recoded.")
    }
    return(invisible(dt))
  }

  codes <- ess_missing_codes(
    round = round, variables = cols, kind = kind, cache = cache, quiet = quiet
  )

  if (nrow(codes) == 0L) {
    if (!quiet) {
      cli::cli_alert_info(
        "{cli::qty(length(cols))}No missing codes are defined for {?this/these} {length(cols)} column{?s}."
      )
    }
    return(invisible(dt))
  }

  changed <- 0L
  touched <- character()

  for (col in intersect(cols, unique(codes$variable_name))) {
    vals <- codes$value[codes$variable_name == col]
    x <- dt[[col]]

    # The catalogue reports codes as strings; match them in the column's own
    # type so that 77 matches an integer column and "77" a character one.
    hits <- if (is.character(x)) {
      x %in% vals
    } else {
      num <- suppressWarnings(as.numeric(vals))
      num <- num[!is.na(num)]
      if (length(num) == 0L) next
      x %in% num
    }

    n <- sum(hits, na.rm = TRUE)
    if (n == 0L) {
      next
    }

    data.table::set(dt, i = which(hits), j = col, value = NA)
    changed <- changed + n
    touched <- c(touched, col)
  }

  if (!quiet) {
    if (changed == 0L) {
      cli::cli_alert_info("No missing codes found in the data; nothing changed.")
    } else {
      cli::cli_alert_success(
        "Recoded {changed} value{?s} to {.code NA} across {length(touched)} column{?s}."
      )
    }
  }

  invisible(dt)
}

#' Turn coded ESS columns into factors
#'
#' Replaces the numeric or string codes of categorical variables with labelled
#' factors, using the value labels from the metadata catalogue.
#'
#' Only variables with a code list are converted; continuous ones such as `agea`
#' and identifiers such as `idno` are left alone. Codes flagged as missing
#' become `NA` rather than a factor level, unless `keep_missing = TRUE`.
#'
#' Like [ess_recode_missings()], this modifies `dt` **by reference**. Pass
#' `data.table::copy(dt)` if you want to keep the codes.
#'
#' @inheritParams ess_recode_missings
#' @param keep_missing If `TRUE`, missing codes become factor levels — "Refusal"
#'   and so on — instead of `NA`.
#' @param max_levels Columns with more distinct codes than this are left as they
#'   are. Guards against turning a near-continuous variable into a factor with
#'   hundreds of levels.
#'
#' @return `dt`, invisibly and modified in place.
#'
#' @examples
#' \dontrun{
#' dt <- ess_round(11, select = c("essround", "cntry", "gndr", "ppltrst"))
#' ess_as_factor(dt, variables = "gndr")
#' dt[, .N, by = gndr]
#' }
#'
#' @export
ess_as_factor <- function(dt,
                          round = NULL,
                          variables = NULL,
                          kind = "integrated",
                          keep_missing = FALSE,
                          max_levels = 50L,
                          cache = TRUE,
                          quiet = FALSE) {
  if (!data.table::is.data.table(dt)) {
    cli::cli_abort("{.arg dt} must be a {.cls data.table}.")
  }
  ess_check_bool(keep_missing, "keep_missing")

  if (is.null(round)) {
    if (!"essround" %in% names(dt)) {
      cli::cli_abort(c(
        "Cannot tell which ESS round {.arg dt} is from.",
        "i" = "Pass {.arg round}, or keep the {.field essround} column when selecting."
      ))
    }
    rounds <- sort(unique(stats::na.omit(dt$essround)))
    if (length(rounds) != 1L) {
      cli::cli_abort(
        "{.arg dt} holds {length(rounds)} ESS rounds, whose labels may differ; convert each separately."
      )
    }
    round <- as.integer(rounds[[1L]])
  }

  cols <- if (is.null(variables)) names(dt) else intersect(variables, names(dt))
  if (length(cols) == 0L) {
    return(invisible(dt))
  }

  labs <- ess_value_labels(
    round = round, variables = cols, kind = kind, cache = cache, quiet = quiet
  )

  if (nrow(labs) == 0L) {
    if (!quiet) {
      cli::cli_alert_info("No value labels available; nothing converted.")
    }
    return(invisible(dt))
  }

  converted <- character()
  skipped <- character()

  for (col in intersect(cols, unique(labs$variable_name))) {
    this <- labs[labs$variable_name == col]
    if (!keep_missing) {
      this <- this[!this$is_missing]
    }
    if (nrow(this) == 0L) {
      next
    }
    if (nrow(this) > max_levels) {
      skipped <- c(skipped, col)
      next
    }

    x <- dt[[col]]
    key <- if (is.character(x)) this$value else suppressWarnings(as.numeric(this$value))
    if (!is.character(x) && anyNA(key)) {
      skipped <- c(skipped, col)
      next
    }

    data.table::set(
      dt, j = col,
      value = factor(x, levels = key, labels = this$label)
    )
    converted <- c(converted, col)
  }

  if (!quiet) {
    if (length(converted) > 0L) {
      cli::cli_alert_success("Converted {length(converted)} column{?s} to factor{?s}.")
    }
    if (length(skipped) > 0L) {
      cli::cli_alert_info(
        "Left {length(skipped)} column{?s} unchanged (over {max_levels} levels, or non-numeric codes): {.val {skipped}}."
      )
    }
  }

  invisible(dt)
}
