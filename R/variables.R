# Variable-level metadata ----------------------------------------------------

# Resolve a round (plus kind) or an explicit data file to one id/version pair.
ess_resolve_data_file <- function(round = NULL,
                                 kind = "integrated",
                                 datafile_id = NULL,
                                 datafile_version = NULL,
                                 cache = TRUE,
                                 call = rlang::caller_env()) {
  if (!is.null(datafile_id)) {
    ess_check_string(datafile_id, "datafile_id", call = call)
    if (is.null(datafile_version)) {
      cli::cli_abort(
        c(
          "{.arg datafile_version} is required alongside {.arg datafile_id}.",
          "i" = "The catalogue does not serve metadata without a version.",
          "i" = "{.run essurvey2::ess_data_files()} returns both."
        ),
        call = call
      )
    }
    return(list(
      id = datafile_id,
      version = as.integer(datafile_version),
      round = NA_integer_,
      file_label = NA_character_
    ))
  }

  if (is.null(round)) {
    cli::cli_abort(
      "Supply either {.arg round} or both {.arg datafile_id} and {.arg datafile_version}.",
      call = call
    )
  }

  if (length(round) != 1L) {
    cli::cli_abort(
      c(
        "{.arg round} must be a single round number here.",
        "x" = "Got {length(round)}."
      ),
      call = call
    )
  }

  files <- ess_data_files(rounds = round, kind = kind, doi = FALSE, cache = cache)

  if (nrow(files) == 0L) {
    available <- ess_data_files(rounds = round, doi = FALSE, cache = cache)
    cli::cli_abort(
      c(
        "ESS round {round} has no {.val {kind}} data file.",
        "i" = "It has: {.val {available$file_kind}}."
      ),
      class = "essurvey2_error_data_file",
      call = call
    )
  }

  if (nrow(files) > 1L) {
    cli::cli_abort(
      c(
        "ESS round {round} has {nrow(files)} {.val {kind}} data files.",
        "x" = "{.val {files$file_label}}",
        "i" = "Pass {.arg datafile_id} and {.arg datafile_version} to pick one."
      ),
      class = "essurvey2_error_data_file",
      call = call
    )
  }

  list(
    id = files$datafile_id[[1L]],
    version = files$datafile_version[[1L]],
    round = files$round[[1L]],
    file_label = files$file_label[[1L]]
  )
}

#' Variables in an ESS data file
#'
#' Lists a data file's variables with their labels and the variable group they
#' belong to, taken from the metadata catalogue. No survey data is downloaded.
#'
#' Use this to find the variable names to pass to the `select` argument of
#' [ess_round()] or [ess_data()], which is much faster than reading all
#' 600-plus columns of an integrated file.
#'
#' The list is a superset of the data file's columns. A few entries are templates
#' whose name ends in `xx`, standing for a country code — `prtvtxx` documents the
#' party-voted-for question, which appears in the data as `prtvtcnl` for the
#' Netherlands, `prtvtdde` for Germany and so on. Others, such as the higher
#' household-grid slots, exist in the questionnaire but have no column in a given
#' round. Passing such a name to `select` is an error naming the column, since it
#' is checked against the file itself.
#'
#' @param round An ESS round number.
#' @param kind Which data file of that round, as named by [ess_file_kinds()].
#'   Defaults to the main integrated file.
#' @param datafile_id,datafile_version Alternatively, identify the data file
#'   directly. Both are returned by [ess_data_files()].
#' @param cache If `TRUE` (the default), reuse catalogue answers for the rest
#'   of the session.
#'
#' @return A `data.table` with columns `variable_name`, `variable_label`,
#'   `group_name`, `group_label`, `role`, `variable_id` and
#'   `variable_version`.
#'
#' @examples
#' \dontrun{
#' vars <- ess_variables(11)
#' nrow(vars)
#'
#' # Find the trust variables.
#' vars[grepl("trust", variable_label, ignore.case = TRUE)]
#'
#' # Then read only those columns.
#' dt <- ess_round(11, select = c("cntry", vars[1:5]$variable_name))
#' }
#'
#' @export
ess_variables <- function(round = NULL,
                          kind = "integrated",
                          datafile_id = NULL,
                          datafile_version = NULL,
                          cache = TRUE) {
  target <- ess_resolve_data_file(
    round = round, kind = kind,
    datafile_id = datafile_id, datafile_version = datafile_version,
    cache = cache
  )

  query <- "
    query($id: ID!, $version: Int) {
      search {
        dataFileMetadata(id: $id, version: $version, instance: PUBLISHED, agencyId: INT_ESSERIC) {
          variableGroups {
            id
            name { en }
            label { en }
            variables { id version name { en } label { en } role { en } }
            variableGroups {
              id
              name { en }
              label { en }
              variables { id version name { en } label { en } role { en } }
            }
          }
        }
      }
    }"

  data <- ess_gql(
    query,
    variables = list(id = target$id, version = target$version),
    cache = cache
  )

  groups <- data$search$dataFileMetadata$variableGroups

  rows <- list()

  collect <- function(group) {
    group_name <- ess_en(group$name)
    group_label <- ess_en(group$label)

    for (v in group$variables) {
      rows[[length(rows) + 1L]] <<- data.table::data.table(
        variable_name = ess_en(v$name),
        variable_label = ess_en(v$label),
        group_name = group_name,
        group_label = group_label,
        role = ess_en(v$role),
        variable_id = ess_chr(v$id),
        variable_version = ess_int(v$version)
      )
    }

    # Variable groups nest one level in the catalogue's own query; follow
    # whatever depth is returned rather than assuming.
    for (sub in group$variableGroups) {
      collect(sub)
    }
  }

  for (g in groups) {
    collect(g)
  }

  proto <- data.table::data.table(
    variable_name = character(), variable_label = character(),
    group_name = character(), group_label = character(), role = character(),
    variable_id = character(), variable_version = integer()
  )

  if (length(rows) == 0L) {
    return(proto)
  }

  out <- data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
  unique(out, by = "variable_name")[]
}

#' Value labels and question wording for a variable
#'
#' Returns a variable's response categories — the codes that appear in the data
#' and what they mean — along with the question as it was put to respondents.
#'
#' Parquet files carry codes, not labels, so this is where the labels live. The
#' `is_missing` column flags the codes the ESS treats as missing, which is what
#' `recode_missings = TRUE` acts on and what [ess_recode_missings()] uses.
#'
#' @param variable A variable name such as `"ppltrst"`, or a catalogue
#'   variable ID.
#' @param round An ESS round number, used to locate the variable when a name is
#'   given.
#' @param kind Which data file of that round, as named by [ess_file_kinds()].
#' @param variable_version The version to fetch, when `variable` is an ID.
#' @param cache If `TRUE` (the default), reuse catalogue answers for the rest
#'   of the session.
#'
#' @return A list with elements `name`, `label`, `question`, `description` and
#'   `codes`, the last being a `data.table` of `value`, `label` and
#'   `is_missing`.
#'
#' @examples
#' \dontrun{
#' info <- ess_variable_info("ppltrst", round = 11)
#' info$question
#' info$codes
#'
#' # The codes the ESS counts as missing.
#' info$codes[is_missing == TRUE]
#' }
#'
#' @export
ess_variable_info <- function(variable,
                              round = NULL,
                              kind = "integrated",
                              variable_version = NULL,
                              cache = TRUE) {
  ess_check_string(variable, "variable")

  id <- variable
  version <- variable_version

  # A name has to be looked up in the round's variable list first.
  if (!grepl("^[0-9a-fA-F-]{20,}$", variable)) {
    if (is.null(round)) {
      cli::cli_abort(c(
        "{.arg round} is required when {.arg variable} is a name.",
        "i" = "Or pass a catalogue variable ID from {.run essurvey2::ess_variables()}."
      ))
    }

    vars <- ess_variables(round = round, kind = kind, cache = cache)
    hit <- vars[tolower(vars$variable_name) == tolower(variable)]

    if (nrow(hit) == 0L) {
      cli::cli_abort(
        c(
          "No variable named {.val {variable}} in ESS round {round}.",
          "i" = "{.run essurvey2::ess_variables({round})} lists all {nrow(vars)} of them."
        ),
        class = "essurvey2_error_variable"
      )
    }

    id <- hit$variable_id[[1L]]
    version <- hit$variable_version[[1L]]
  }

  if (is.null(version)) {
    cli::cli_abort(c(
      "{.arg variable_version} is required when {.arg variable} is an ID.",
      "i" = "{.run essurvey2::ess_variables()} returns it."
    ))
  }

  query <- "
    query($id: ID!, $version: Int) {
      search {
        variableMetadata(id: $id, version: $version, instance: PUBLISHED, agencyId: INT_ESSERIC) {
          name { en }
          label { en }
          description { en }
          question
          preQuestionStr
          postQuestionStr
          codeList(first: 10000) {
            edges { node { value label { en } isMissing } }
          }
        }
      }
    }"

  data <- ess_gql(
    query,
    variables = list(id = id, version = as.integer(version)),
    cache = cache
  )

  v <- data$search$variableMetadata

  codes <- ess_rows_to_dt(
    v$codeList$edges,
    function(e) {
      data.table::data.table(
        value = ess_chr(e$node$value),
        label = ess_en(e$node$label),
        is_missing = isTRUE(ess_lgl(e$node$isMissing))
      )
    },
    data.table::data.table(
      value = character(), label = character(), is_missing = logical()
    )
  )

  list(
    name = ess_en(v$name),
    label = ess_en(v$label),
    question = ess_chr(v$question),
    description = ess_en(v$description),
    pre_question = ess_chr(v$preQuestionStr),
    post_question = ess_chr(v$postQuestionStr),
    codes = codes
  )
}
