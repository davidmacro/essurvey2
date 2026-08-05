# Downloading and reading data files -----------------------------------------

#' Download an ESS data file to disk
#'
#' Fetches one data file from the ESS data API and writes it to disk without
#' parsing it. Use [ess_data()] instead to get a `data.table` straight away, or
#' this function when you want the file itself.
#'
#' @param doi The DOI of a data file, as listed by [ess_data_files()]. Study
#'   DOIs are not accepted by the API.
#' @param path Where to write the file. When `NULL` (the default) the cache
#'   location is used if caching is on, otherwise a temporary file.
#' @param format One of `"parquet"` (the default), `"csv"`, `"sav"` or
#'   `"dta"`.
#' @param recode_missings If `TRUE` (the default), the API converts values
#'   flagged as missing in the metadata into system missing values. See
#'   [ess_data_file_url()] for why this cannot be switched off with a value.
#' @param user_id Your ESS API user ID. Defaults to [ess_user_id()].
#' @param use_cache If `TRUE` (the default) reuse a previously downloaded copy,
#'   and keep the download for next time. Whether it can actually be kept
#'   depends on the cache being available: see [ess_cache_dir()], which asks
#'   before first writing to the default location and falls back to a temporary
#'   file if it may not.
#' @param overwrite If `TRUE`, download again even if the file is already
#'   cached at `path`.
#' @param quiet If `TRUE`, suppress progress and status messages.
#'
#' @return The path to the downloaded file, invisibly.
#'
#' @examples
#' \dontrun{
#' # Needs a configured user ID; see ?ess_user_id.
#' path <- ess_download_file("10.21338/ess6e02_6")
#' file.size(path)
#'
#' # Keep the SPSS file instead, in a directory of your choosing. Anywhere
#' # writable will do; tempdir() is used here so the example leaves nothing
#' # behind.
#' ess_download_file(
#'   "10.21338/ess6e02_6",
#'   path = file.path(tempdir(), "ess6.sav"),
#'   format = "sav"
#' )
#' }
#'
#' @export
ess_download_file <- function(doi,
                              path = NULL,
                              format = "parquet",
                              recode_missings = TRUE,
                              user_id = ess_user_id(),
                              use_cache = TRUE,
                              overwrite = FALSE,
                              quiet = FALSE) {
  format <- ess_check_format(format)
  ess_check_bool(recode_missings, "recode_missings")
  ess_check_bool(use_cache, "use_cache")
  ess_check_bool(overwrite, "overwrite")
  ess_check_bool(quiet, "quiet")

  # Normalise the DOI early so the cache key and the request agree.
  parts <- ess_parse_doi(doi)
  doi <- paste0(parts$prefix, "/", parts$suffix)

  cached <- isTRUE(use_cache)

  # Whether the file will land in the managed cache, as opposed to a temporary
  # file or a path the caller named. Only that case gets pruned afterwards.
  in_cache <- FALSE

  if (is.null(path)) {
    path <- if (cached) {
      ess_cache_path(doi, format, recode_missings, create = TRUE)
    } else {
      NULL
    }
    if (is.null(path)) {
      cached <- FALSE
      path <- tempfile(
        pattern = "essurvey2-",
        fileext = paste0(".", ess_format_ext[[format]])
      )
    } else {
      in_cache <- TRUE
    }
  } else {
    ess_check_string(path, "path")
    parent <- dirname(path)
    if (!dir.exists(parent)) {
      dir.create(parent, recursive = TRUE, showWarnings = FALSE)
    }
  }

  if (!overwrite && file.exists(path) && file.size(path) > 0L) {
    if (!quiet) {
      cli::cli_alert_info(
        "Using cached {.val {doi}} ({ess_format_bytes(file.size(path))})."
      )
    }
    return(invisible(path))
  }

  req <- ess_data_file_request(
    doi = doi,
    user_id = user_id,
    format = format,
    recode_missings = recode_missings
  )

  if (!quiet) {
    cli::cli_alert_info("Downloading {.val {doi}} as {format}{if (recode_missings) ', missing values recoded' else ''}.")
    if (interactive()) {
      req <- httr2::req_progress(req, type = "down")
    }
  }

  # Download to a scratch file first: a failed request writes its JSON error
  # body to the destination, which must not be mistaken for a cached download.
  tmp <- paste0(path, ".part")
  on.exit(unlink(tmp), add = TRUE)

  # A refused connection or a DNS failure never reaches ess_api_abort(), which
  # needs a response to read. Name the endpoint instead of letting curl's own
  # wording surface, matching how ess_gql() reports the same condition.
  resp <- tryCatch(
    httr2::req_perform(req, path = tmp),
    httr2_failure = function(cnd) {
      cli::cli_abort(
        c(
          "Could not reach the ESS data API.",
          "x" = conditionMessage(cnd),
          "i" = "{.url {ess_api_url()}}"
        ),
        class = "essurvey2_error_offline"
      )
    }
  )

  if (httr2::resp_status(resp) >= 400L) {
    ess_api_abort(resp, doi = doi, path = tmp)
  }

  size <- file.size(tmp)
  if (is.na(size) || size == 0L) {
    cli::cli_abort(c(
      "The API returned an empty file for {.val {doi}}.",
      "i" = "Try again, and contact {.email essdatasupport@sikt.no} if it persists."
    ), class = "essurvey2_error_api")
  }

  if (!file.rename(tmp, path)) {
    # Renaming can fail across file systems; fall back to a copy.
    if (!file.copy(tmp, path, overwrite = TRUE)) {
      cli::cli_abort(c(
        "Downloaded {.val {doi}} but could not write it to its destination.",
        "x" = "{.path {path}}"
      ))
    }
  }

  if (!quiet) {
    cli::cli_alert_success("Downloaded {ess_format_bytes(size)} to {.path {path}}.")
  }

  # Keep the cache within its cap now that something has been added to it. A
  # temporary file or a path the caller named is theirs, not the cache's, so
  # neither triggers a trim.
  if (in_cache) {
    ess_cache_prune(dirname(path), quiet = quiet)
  }

  invisible(path)
}

#' Read an ESS data file into a data.table
#'
#' Downloads a data file by DOI and reads it into memory. Parquet — the
#' default, and the format the API is fastest at serving — is read with
#' \pkg{arrow}; `csv` with [data.table::fread()]; `sav` and `dta` with
#' \pkg{haven}, which must be installed for those.
#'
#' [ess_round()] and [ess_country()] are usually more convenient: they find the
#' DOI for you. Use `ess_data()` for files those do not cover, such as the
#' contact forms, or for data outside the main ESS series.
#'
#' @inheritParams ess_download_file
#' @param select Optionally, a character vector of column names to read. For
#'   parquet only the requested columns are decoded, which is much faster than
#'   reading all 600-plus variables of an integrated file. Unknown names are an
#'   error.
#'
#' @return A [data.table::data.table].
#'
#' @examples
#' \dontrun{
#' # The whole ESS round 6 integrated file.
#' dt <- ess_data("10.21338/ess6e02_6")
#' dim(dt)
#'
#' # Just a few variables, which is far quicker.
#' dt <- ess_data("10.21338/ess6e02_6", select = c("cntry", "agea", "gndr"))
#' dt[, .N, by = cntry]
#' }
#'
#' @export
ess_data <- function(doi,
                     format = "parquet",
                     recode_missings = TRUE,
                     select = NULL,
                     user_id = ess_user_id(),
                     use_cache = TRUE,
                     quiet = FALSE) {
  format <- ess_check_format(format)

  path <- ess_download_file(
    doi = doi,
    format = format,
    recode_missings = recode_missings,
    user_id = user_id,
    use_cache = use_cache,
    quiet = quiet
  )

  ess_read_file(path, format = format, select = select)
}

#' Read a downloaded ESS data file
#'
#' Reads a file already on disk — normally one produced by
#' [ess_download_file()] — into a `data.table`. Separated from the download so
#' that a cached or manually fetched file can be re-read without touching the
#' network.
#'
#' @param path Path to the file.
#' @param format The file's format. When `NULL` (the default) it is inferred
#'   from the extension.
#' @param select Optionally, a character vector of columns to read.
#'
#' @return A [data.table::data.table].
#'
#' @examples
#' \dontrun{
#' path <- ess_download_file("10.21338/ess6e02_6")
#' ess_read_file(path, select = c("cntry", "essround"))
#' }
#'
#' @export
ess_read_file <- function(path, format = NULL, select = NULL) {
  ess_check_string(path, "path")

  if (!file.exists(path)) {
    cli::cli_abort(c("No such file.", "x" = "{.path {path}}"))
  }

  if (is.null(format)) {
    format <- tolower(tools::file_ext(path))
    if (!format %in% ess_formats) {
      cli::cli_abort(c(
        "Cannot tell what format {.path {path}} is.",
        "i" = "Pass {.arg format} explicitly, one of {.val {ess_formats}}."
      ))
    }
  }
  format <- ess_check_format(format)

  if (!is.null(select)) {
    if (!is.character(select) || anyNA(select)) {
      cli::cli_abort("{.arg select} must be a character vector of column names.")
    }
    select <- unique(select)
  }

  switch(format,
    parquet = ess_read_parquet(path, select),
    csv = ess_read_csv(path, select),
    ess_read_haven(path, format, select)
  )
}

ess_read_parquet <- function(path, select = NULL) {
  # as_data_frame = FALSE keeps this an arrow Table, so columns can be dropped
  # before anything is materialised in R.
  tbl <- arrow::read_parquet(path, as_data_frame = FALSE)

  if (!is.null(select)) {
    have <- names(tbl)
    missing <- setdiff(select, have)
    if (length(missing) > 0L) {
      cli::cli_abort(
        c(
          "{cli::qty(length(missing))}Column{?s} {.val {missing}} {cli::qty(length(missing))}{?is/are} not in this data file.",
          "i" = "It has {length(have)} column{?s}; the first few are {.val {utils::head(have, 8)}}."
        ),
        class = "essurvey2_error_select"
      )
    }
    tbl <- tbl$SelectColumns(match(select, have) - 1L)
  }

  data.table::setDT(as.data.frame(tbl))[]
}

ess_read_csv <- function(path, select = NULL) {
  dt <- if (is.null(select)) {
    data.table::fread(path, showProgress = FALSE)
  } else {
    data.table::fread(path, select = select, showProgress = FALSE)
  }
  data.table::setDT(dt)[]
}

ess_read_haven <- function(path, format, select = NULL) {
  rlang::check_installed(
    "haven",
    reason = paste0("to read ", format, " files. Parquet needs no extra package.")
  )

  df <- if (format == "sav") {
    haven::read_spss(path)
  } else {
    haven::read_dta(path)
  }

  dt <- data.table::as.data.table(df)

  if (!is.null(select)) {
    missing <- setdiff(select, names(dt))
    if (length(missing) > 0L) {
      cli::cli_abort(
        "{cli::qty(length(missing))}Column{?s} {.val {missing}} {cli::qty(length(missing))}{?is/are} not in this data file.",
        class = "essurvey2_error_select"
      )
    }
    dt <- dt[, select, with = FALSE]
  }

  dt[]
}
