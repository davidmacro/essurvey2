# The data API: request construction and error handling ----------------------

ess_formats <- c("parquet", "csv", "sav", "dta")

ess_format_ext <- c(
  parquet = "parquet",
  csv = "csv",
  sav = "sav",
  dta = "dta"
)

#' Split a DOI into its prefix and suffix
#'
#' The ESS data API takes the two halves of a data file's DOI as separate path
#' segments, so they have to be separated before a request can be built.
#'
#' Several ways of writing a DOI are accepted: bare (`10.21338/ess11e04_2`),
#' prefixed (`doi:10.21338/ess11e04_2`), or as a resolver URL
#' (`https://doi.org/10.21338/ess11e04_2`).
#'
#' @param doi A DOI as a length-one character vector.
#'
#' @return A list with elements `prefix` and `suffix`.
#'
#' @examples
#' ess_parse_doi("10.21338/ess11e04_2")
#' ess_parse_doi("https://doi.org/10.21338/ess6e02_6")
#'
#' @export
ess_parse_doi <- function(doi) {
  ess_check_string(doi, "doi")

  x <- trimws(doi)
  x <- sub("^doi:", "", x, ignore.case = TRUE)
  x <- sub("^https?://(dx\\.)?doi\\.org/", "", x, ignore.case = TRUE)

  if (!grepl("^10\\.[0-9]+/.+", x)) {
    cli::cli_abort(
      c(
        "{.arg doi} is not a DOI.",
        "x" = "Got {.val {doi}}.",
        "i" = "A DOI looks like {.val 10.21338/ess11e04_2}.",
        "i" = "{.run essurvey2::ess_data_files()} lists the DOIs of ESS data files."
      ),
      class = "essurvey2_error_doi"
    )
  }

  list(
    prefix = sub("/.*$", "", x),
    suffix = sub("^[^/]+/", "", x)
  )
}

ess_check_format <- function(format, call = rlang::caller_env()) {
  ess_check_string(format, "format", call = call)
  format <- tolower(format)
  if (!format %in% ess_formats) {
    cli::cli_abort(
      c(
        "{.arg format} must be one of {.val {ess_formats}}.",
        "x" = "Got {.val {format}}."
      ),
      call = call
    )
  }
  format
}

#' Build the URL of an ESS data file
#'
#' Returns the URL that [ess_download_file()] fetches. Exported so that a
#' download can be handed to another tool, or inspected when a request is
#' behaving unexpectedly.
#'
#' @param doi The DOI of a *data file*. Study-level DOIs such as
#'   `10.21338/NSD-ESS10-2020` are rejected by the API; see
#'   [ess_data_files()] for data file DOIs.
#' @param user_id Your ESS API user ID. Defaults to [ess_user_id()].
#' @param format One of `"parquet"` (the default), `"csv"`, `"sav"` or
#'   `"dta"`.
#' @param recode_missings If `TRUE` (the default), ask the API to convert
#'   values flagged as missing in the metadata — "Not applicable", "No answer"
#'   and so on — into system missing values.
#'
#'   The API treats its `recodeMissingValues` parameter as a flag, so any value
#'   at all switches recoding *on*. This package therefore omits the parameter
#'   entirely when `recode_missings = FALSE`, which is the only way to turn it
#'   off.
#'
#' @return The URL as a length-one character vector.
#'
#' @examples
#' ess_data_file_url(
#'   "10.21338/ess11e04_2",
#'   user_id = "a1b2c3d4-e5f6-7890-abcd-ef1234567890"
#' )
#'
#' @export
ess_data_file_url <- function(doi,
                              user_id = ess_user_id(),
                              format = "parquet",
                              recode_missings = TRUE) {
  parts <- ess_parse_doi(doi)
  format <- ess_check_format(format)
  ess_check_bool(recode_missings, "recode_missings")
  ess_check_string(user_id, "user_id")
  ess_check_user_id(user_id)

  query <- paste0(
    "userId=", utils::URLencode(user_id, reserved = TRUE),
    "&fileFormat=", format
  )

  # A valueless flag: see the note in the documentation above.
  if (recode_missings) {
    query <- paste0(query, "&recodeMissingValues")
  }

  paste0(
    ess_api_url(), "/v1/data/dataFile/",
    utils::URLencode(parts$prefix, reserved = TRUE), "/",
    utils::URLencode(parts$suffix, reserved = TRUE),
    "?", query
  )
}

ess_data_file_request <- function(doi,
                                  user_id = ess_user_id(),
                                  format = "parquet",
                                  recode_missings = TRUE,
                                  timeout = 600) {
  url <- ess_data_file_url(
    doi = doi,
    user_id = user_id,
    format = format,
    recode_missings = recode_missings
  )

  req <- httr2::request(url)
  req <- httr2::req_user_agent(req, ess_user_agent())
  req <- httr2::req_timeout(req, timeout)
  # Errors are inspected by ess_api_abort(), which needs the response body to
  # build a useful message, so httr2 must not throw first.
  req <- httr2::req_error(req, is_error = function(resp) FALSE)
  httr2::req_retry(
    req,
    max_tries = 3L,
    is_transient = function(resp) {
      httr2::resp_status(resp) %in% c(408L, 429L, 500L, 502L, 503L, 504L)
    }
  )
}

# Read an error payload, whether it landed in memory or in a file, and raise a
# classed condition. The API answers failures with
# {"code": <int>, "message": <chr>, "requestId": <chr>}.
ess_api_abort <- function(resp, doi = NULL, path = NULL, call = rlang::caller_env()) {
  status <- httr2::resp_status(resp)

  body <- NULL
  txt <- NULL

  if (!is.null(path) && file.exists(path)) {
    txt <- tryCatch(
      paste(readLines(path, warn = FALSE), collapse = "\n"),
      error = function(e) NULL
    )
  } else {
    txt <- tryCatch(
      httr2::resp_body_string(resp),
      error = function(e) NULL
    )
  }

  if (!is.null(txt) && nzchar(txt)) {
    body <- tryCatch(
      jsonlite::fromJSON(txt, simplifyVector = TRUE),
      error = function(e) NULL
    )
  }

  code <- ess_int(body$code)
  message <- ess_chr(body$message)
  request_id <- ess_chr(body$requestId)

  if (is.na(message)) {
    message <- httr2::resp_status_desc(resp)
  }

  # Codes observed against the live API:
  #   101  userId query parameter missing
  #   201  DOI URL resolution error   (no such data file)
  #   202  DOI ID resolution error    (a study DOI was passed)
  #   205  Error registering download. Is the user ID valid?
  #   901  Unexpected error
  # The API is in beta and documents only some of these, so unknown codes fall
  # back to their leading digit: 1xx and 2xx are the caller's problem, 9xx ours.
  cls <- if (!is.na(code)) {
    switch(as.character(code),
      "101" = ,
      "205" = "essurvey2_error_user_id",
      "201" = ,
      "202" = "essurvey2_error_doi",
      if (code >= 900L) "essurvey2_error_server" else "essurvey2_error_api"
    )
  } else if (status >= 500L) {
    "essurvey2_error_server"
  } else {
    "essurvey2_error_api"
  }

  bullets <- c("The ESS data API rejected the request.")
  bullets <- c(bullets, "x" = paste0(
    "HTTP ", status,
    if (!is.na(code)) paste0(" (code ", code, ")") else "",
    ": ", message
  ))

  if (!is.null(doi)) {
    bullets <- c(bullets, "i" = "DOI: {.val {doi}}")
  }

  # Explain the failures a user can actually act on.
  hint <- switch(cls,
    essurvey2_error_user_id = c(
      "i" = "The API would not accept the user ID. Check it against {.url https://ess.sikt.no/en/api}.",
      "i" = "{.run essurvey2::ess_config()} shows which ID this session is using."
    ),
    essurvey2_error_doi = c(
      "i" = "The API only serves {.emph data file} DOIs, not study-level ones such as {.val 10.21338/NSD-ESS10-2020}.",
      "i" = "{.run essurvey2::ess_data_files()} lists valid data file DOIs."
    ),
    essurvey2_error_server = c(
      "i" = "This is a server-side failure. Larger data files are not available through the API at all.",
      "i" = "If it persists, contact {.email essdatasupport@sikt.no}."
    ),
    NULL
  )
  bullets <- c(bullets, hint)

  if (!is.na(request_id)) {
    bullets <- c(bullets, "i" = "Quote request ID {.val {request_id}} when contacting ESS support.")
  }

  cli::cli_abort(
    bullets,
    class = unique(c(cls, "essurvey2_error_api")),
    status = status,
    code = code,
    request_id = request_id,
    doi = doi,
    call = call
  )
}
