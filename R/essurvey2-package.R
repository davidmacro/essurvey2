#' essurvey2: download data from the European Social Survey API
#'
#' `essurvey2` is a client for the European Social Survey (ESS) data API at
#' <https://api.ess.sikt.no>. It discovers what is published through the ESS
#' metadata catalogue, downloads data files as Apache Parquet, and returns
#' [data.table::data.table] objects.
#'
#' @section Configuration:
#' Every request to the data API must carry a user ID, which the ESS uses to
#' record download statistics. Obtain one by logging in at
#' <https://ess.sikt.no/en/api>, then store it as the `ESS_USER_ID`
#' environment variable. See [ess_user_id()] and [ess_set_user_id()].
#'
#' @section Finding data:
#' Nothing in this group downloads survey data, so these are cheap to call.
#'
#' * [ess_rounds()] — published ESS rounds and their main data file
#' * [ess_data_files()] — every data file in a round
#' * [ess_data_file_info()] — a data file's DOI and coverage
#' * [ess_countries()], [ess_country_rounds()] — country coverage
#' * [ess_themes()], [ess_theme_rounds()] — questionnaire themes
#' * [ess_variables()] — variable-level metadata
#' * [ess_series()] — ESS and related survey series
#'
#' @section Downloading data:
#' * [ess_round()] — one or more rounds as a `data.table`
#' * [ess_all_rounds()] — every published round, stacked
#' * [ess_country()] — one country across rounds
#' * [ess_data()] — any data file, by DOI
#' * [ess_download_file()] — a data file to disk, unparsed
#'
#' @section Cache:
#' Downloads are cached on disk between sessions. See [ess_cache_dir()],
#' [ess_cache_list()] and [ess_cache_clear()].
#'
#' @section Differences from the essurvey package:
#' The earlier `essurvey` package scraped the ESS website and parsed SPSS or
#' Stata files with `haven`. Those endpoints no longer exist. Two consequences
#' are visible in this package's interface:
#'
#' * A round's respondents come as one integrated file, so [ess_country()]
#'   downloads a whole round and filters it locally rather than fetching a
#'   country file. (A handful of countries held out of an integrated file do have
#'   their own file; see [ess_data_files()] with `kind = "country"`.)
#' * Parquet columns arrive as plain integers and strings. Value labels live in
#'   the metadata catalogue ([ess_variables()]), not in the data file.
#'
#' @keywords internal
"_PACKAGE"

#' @importFrom data.table := .N .SD data.table setDT setnames rbindlist
#' @importFrom data.table as.data.table setcolorder setattr fifelse
#' @importFrom utils packageVersion URLencode
NULL

# Columns assigned or referenced by bare name inside data.table expressions.
# Declared so that R CMD check does not report them as undefined globals.
utils::globalVariables(c(
  "country_name", "doi", "edition", "file_country", "file_kind", "file_label",
  "n_variables", "round"
))
