# High-level download verbs --------------------------------------------------

#' Download ESS rounds
#'
#' Downloads one or more rounds of the European Social Survey and returns them
#' as a single `data.table`. The DOI of each round's data file is resolved from
#' the live metadata catalogue, so a newly published round or a new file edition
#' is picked up without updating this package.
#'
#' Downloads are cached on disk, so calling this again — now or in a later
#' session — costs nothing. See [ess_cache_dir()].
#'
#' @param rounds One or more round numbers. [ess_rounds()] lists what is
#'   published.
#' @param kind Which data file of each round to download, as named by
#'   [ess_file_kinds()]. Defaults to the main integrated file.
#' @param select Optionally, a character vector of variables to read. Only
#'   these columns are decoded from the parquet file, which is considerably
#'   faster than reading all of them. [ess_variables()] lists the names.
#' @param recode_missings If `TRUE` (the default), values flagged as missing in
#'   the ESS metadata — "Not applicable", "Refusal", "Don't know" and so on —
#'   arrive as `NA`. Set to `FALSE` to keep the original codes.
#' @param format The transfer format, `"parquet"` by default. See
#'   [ess_data()] for the alternatives.
#' @param user_id Your ESS API user ID. Defaults to [ess_user_id()].
#' @param use_cache If `TRUE` (the default), reuse downloads already on disk.
#' @param quiet If `TRUE`, suppress progress and status messages.
#'
#' @return A [data.table::data.table]. When several rounds are requested they
#'   are stacked with [data.table::rbindlist()] and `fill = TRUE`, so a variable
#'   absent from a round is `NA` for that round's rows. The `essround` column
#'   distinguishes them.
#'
#' @examples
#' \dontrun{
#' # One round, all variables.
#' dt <- ess_round(11)
#' dim(dt)
#'
#' # Two rounds, a handful of variables.
#' dt <- ess_round(10:11, select = c("essround", "cntry", "agea", "ppltrst"))
#' dt[, .(mean_trust = mean(ppltrst, na.rm = TRUE)), by = .(essround, cntry)]
#'
#' # The self-completion file of round 11.
#' sc <- ess_round(11, kind = "self_completion")
#' }
#'
#' @export
ess_round <- function(rounds,
                      kind = "integrated",
                      select = NULL,
                      recode_missings = TRUE,
                      format = "parquet",
                      user_id = ess_user_id(),
                      use_cache = TRUE,
                      quiet = FALSE) {
  ess_check_bool(quiet, "quiet")

  published <- ess_rounds()$round
  rounds <- ess_check_rounds(rounds, available = published)

  files <- ess_data_files(rounds = rounds, kind = kind, doi = TRUE)

  if (nrow(files) == 0L) {
    cli::cli_abort(
      c(
        "No {.val {kind}} data file for ESS round{cli::qty(length(rounds))}{?s} {.val {rounds}}.",
        "i" = "{.run essurvey2::ess_data_files({deparse(rounds)})} shows what exists."
      ),
      class = "essurvey2_error_data_file"
    )
  }

  missing_rounds <- setdiff(rounds, files$round)
  if (length(missing_rounds) > 0L) {
    cli::cli_warn(c(
      "{cli::qty(length(missing_rounds))}ESS round{?s} {.val {missing_rounds}} {cli::qty(length(missing_rounds))}{?has/have} no {.val {kind}} data file and {cli::qty(length(missing_rounds))}{?was/were} skipped.",
      "i" = "{.run essurvey2::ess_data_files()} shows which kinds each round has."
    ))
  }

  if (anyNA(files$doi)) {
    bad <- files$file_label[is.na(files$doi)]
    cli::cli_abort(
      c(
        "The catalogue has no DOI for {.val {bad}}, so {cli::qty(length(bad))}{?it/they} cannot be downloaded.",
        "i" = "Contact {.email essdatasupport@sikt.no} if this persists."
      ),
      class = "essurvey2_error_catalogue"
    )
  }

  if (!quiet && nrow(files) > 1L) {
    cli::cli_alert_info(
      "Fetching {nrow(files)} data file{?s}: {.val {files$file_label}}."
    )
  }

  parts <- vector("list", nrow(files))

  for (i in seq_len(nrow(files))) {
    parts[[i]] <- ess_data(
      doi = files$doi[[i]],
      format = format,
      recode_missings = recode_missings,
      select = select,
      user_id = user_id,
      use_cache = use_cache,
      quiet = quiet
    )
  }

  if (length(parts) == 1L) {
    return(parts[[1L]])
  }

  out <- data.table::rbindlist(parts, use.names = TRUE, fill = TRUE)
  data.table::setDT(out)[]
}

#' Download every published ESS round
#'
#' Downloads all rounds listed by [ess_rounds()] and stacks them into one
#' `data.table`.
#'
#' This transfers a lot of data — the integrated files come to several hundred
#' megabytes — so it warns before starting unless `quiet = TRUE`. Passing
#' `select` cuts the transfer down substantially, and the cache means the cost
#' is paid only once.
#'
#' @inheritParams ess_round
#'
#' @return A [data.table::data.table] with every round stacked, distinguished by
#'   the `essround` column.
#'
#' @examples
#' \dontrun{
#' # A few variables across the whole series.
#' trust <- ess_all_rounds(select = c("essround", "cntry", "ppltrst", "pspwght"))
#' trust[, .N, by = essround]
#'
#' # Everything, which is a large download.
#' all <- ess_all_rounds()
#' }
#'
#' @export
ess_all_rounds <- function(kind = "integrated",
                           select = NULL,
                           recode_missings = TRUE,
                           format = "parquet",
                           user_id = ess_user_id(),
                           use_cache = TRUE,
                           quiet = FALSE) {
  rounds <- ess_rounds()$round

  if (!quiet) {
    cli::cli_alert_warning(
      "About to fetch {length(rounds)} ESS round{?s}. This is a large download."
    )

    n_cached <- nrow(ess_cache_list())
    if (n_cached > 0L) {
      cli::cli_alert_info("{n_cached} file{?s} already cached, and will be reused.")
    }

    if (is.null(select)) {
      cli::cli_alert_info(
        "Passing {.arg select} would transfer far less. {.run essurvey2::ess_variables(11)} lists the variables."
      )
    }
  }

  ess_round(
    rounds = rounds,
    kind = kind,
    select = select,
    recode_missings = recode_missings,
    format = format,
    user_id = user_id,
    use_cache = use_cache,
    quiet = quiet
  )
}

#' Download one country's ESS data
#'
#' Returns the respondents from a single country across the requested rounds.
#'
#' The current ESS API publishes integrated files only — there are no
#' per-country downloads any more, unlike the endpoints the earlier `essurvey`
#' package used. Each requested round is therefore downloaded in full and then
#' filtered on its `cntry` column. The cache makes this cheap on repeat calls,
#' and `select` keeps the transfer small.
#'
#' @param country An ISO 3166-1 alpha-2 country code, such as `"NL"`. Case is
#'   ignored. [ess_countries()] lists the valid codes.
#' @param rounds Round numbers. When `NULL` (the default) every round the
#'   country took part in is used, as reported by [ess_country_rounds()].
#' @inheritParams ess_round
#'
#' @return A [data.table::data.table] holding only that country's rows. When
#'   `select` is given, `cntry` is read regardless, since it is needed to
#'   filter, and it is kept in the result.
#'
#' @examples
#' \dontrun{
#' # Every Dutch respondent, all rounds.
#' nl <- ess_country("NL")
#' nl[, .N, by = essround]
#'
#' # Two rounds, a few variables.
#' nl <- ess_country("NL", rounds = 10:11,
#'                   select = c("essround", "agea", "ppltrst"))
#' }
#'
#' @export
ess_country <- function(country,
                        rounds = NULL,
                        select = NULL,
                        recode_missings = TRUE,
                        format = "parquet",
                        user_id = ess_user_id(),
                        use_cache = TRUE,
                        quiet = FALSE) {
  ess_check_string(country, "country")
  code <- toupper(trimws(country))

  participated <- ess_country_rounds(code)

  if (is.null(rounds)) {
    rounds <- participated$round
    if (length(rounds) == 0L) {
      cli::cli_abort(
        "The catalogue reports no ESS rounds for {.val {code}}.",
        class = "essurvey2_error_country"
      )
    }
  } else {
    rounds <- ess_check_rounds(rounds, available = ess_rounds()$round)
    absent <- setdiff(rounds, participated$round)
    if (length(absent) > 0L) {
      cli::cli_warn(c(
        "{.val {code}} did not take part in ESS round{cli::qty(length(absent))}{?s} {.val {absent}}.",
        "i" = "{cli::qty(length(absent))}{?That round/Those rounds} will contribute no rows.",
        "i" = "{.run essurvey2::ess_country_rounds(\"{code}\")} lists the rounds it is in."
      ))
    }
  }

  # cntry is needed to filter, so read it even if the caller did not ask for it,
  # and keep it: a country subset that cannot say which country it is would be
  # a strange thing to hand back.
  if (!is.null(select)) {
    select <- unique(c("cntry", select))
  }

  dt <- ess_round(
    rounds = rounds,
    kind = "integrated",
    select = select,
    recode_missings = recode_missings,
    format = format,
    user_id = user_id,
    use_cache = use_cache,
    quiet = quiet
  )

  if (!"cntry" %in% names(dt)) {
    cli::cli_abort(c(
      "The data file has no {.field cntry} column, so it cannot be filtered by country.",
      "i" = "Use {.fn essurvey2::ess_round} and filter it yourself."
    ))
  }

  out <- dt[dt$cntry == code]

  if (nrow(out) == 0L) {
    cli::cli_warn(c(
      "No rows for {.val {code}} in ESS round{cli::qty(length(rounds))}{?s} {.val {rounds}}.",
      "i" = "Country codes present: {.val {sort(unique(dt$cntry))}}."
    ))
  } else if (!quiet) {
    cli::cli_alert_success(
      "{nrow(out)} respondent{?s} from {.val {code}} across {length(unique(out$essround))} round{?s}."
    )
  }

  out[]
}
