# On-disk cache for downloaded data files -------------------------------------

#' Where essurvey2 caches downloads
#'
#' Data files are cached on disk so that repeating a call — in the same session
#' or a later one — does not re-download several megabytes. The default location
#' is `tools::R_user_dir("essurvey2", "cache")`.
#'
#' Override it with an option, for instance to put the cache on a larger disk
#' or to share it between projects:
#'
#' ```r
#' options(essurvey2.cache_dir = "D:/data/ess-cache")
#' ```
#'
#' Set `options(essurvey2.cache_dir = FALSE)` to disable caching everywhere,
#' which is equivalent to passing `use_cache = FALSE` to every call.
#'
#' @param create If `TRUE`, create the directory when it does not exist.
#'
#' @return The cache directory as a length-one character vector, or `NULL` when
#'   caching is disabled.
#'
#' @examples
#' ess_cache_dir()
#'
#' @export
ess_cache_dir <- function(create = FALSE) {
  opt <- getOption("essurvey2.cache_dir", default = NULL)

  if (isFALSE(opt)) {
    return(NULL)
  }

  dir <- if (is.null(opt)) {
    tools::R_user_dir("essurvey2", which = "cache")
  } else {
    ess_check_string(opt, "options(essurvey2.cache_dir)")
    opt
  }

  if (create && !dir.exists(dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    if (!dir.exists(dir)) {
      cli::cli_abort(c(
        "Could not create the cache directory.",
        "x" = "{.path {dir}}",
        "i" = "Point {.code options(essurvey2.cache_dir)} somewhere writable, or pass {.code use_cache = FALSE}."
      ))
    }
  }

  dir
}

# A cache file name identifies exactly what was downloaded: the same DOI in a
# different format, or with a different missing-value treatment, is a different
# file.
ess_cache_key <- function(doi, format = "parquet", recode_missings = TRUE) {
  parts <- ess_parse_doi(doi)
  format <- ess_check_format(format)
  ess_check_bool(recode_missings, "recode_missings")

  stem <- gsub("[^A-Za-z0-9._-]+", "-", paste0(parts$prefix, "_", parts$suffix))

  paste0(
    stem,
    if (recode_missings) "__recoded" else "__raw",
    ".", ess_format_ext[[format]]
  )
}

ess_cache_path <- function(doi, format = "parquet", recode_missings = TRUE, create = FALSE) {
  dir <- ess_cache_dir(create = create)
  if (is.null(dir)) {
    return(NULL)
  }
  file.path(dir, ess_cache_key(doi, format, recode_missings))
}

#' List and clear cached ESS downloads
#'
#' `ess_cache_list()` reports what is currently cached. `ess_cache_clear()`
#' deletes cached files, either all of them or just those for one DOI.
#'
#' @param doi Optionally, restrict `ess_cache_clear()` to one DOI. When
#'   `NULL` (the default) the whole cache is cleared.
#' @param confirm If `TRUE`, ask before deleting. Defaults to `TRUE` in an
#'   interactive session.
#'
#' @return `ess_cache_list()` returns a `data.table` with one row per cached
#'   file and columns `file`, `format`, `recoded`, `size`, `size_bytes`,
#'   `modified` and `path`. `ess_cache_clear()` invisibly returns the number of
#'   files deleted.
#'
#' @examples
#' ess_cache_list()
#'
#' \dontrun{
#' # Remove one round's file.
#' ess_cache_clear("10.21338/ess6e02_6", confirm = FALSE)
#'
#' # Remove everything.
#' ess_cache_clear(confirm = FALSE)
#' }
#'
#' @name ess_cache
NULL

#' @rdname ess_cache
#' @export
ess_cache_list <- function() {
  empty <- data.table::data.table(
    file = character(),
    format = character(),
    recoded = logical(),
    size = character(),
    size_bytes = numeric(),
    modified = as.POSIXct(character()),
    path = character()
  )

  dir <- ess_cache_dir(create = FALSE)
  if (is.null(dir) || !dir.exists(dir)) {
    return(empty)
  }

  paths <- list.files(dir, full.names = TRUE, all.files = FALSE, no.. = TRUE)
  paths <- paths[!dir.exists(paths)]
  if (length(paths) == 0L) {
    return(empty)
  }

  info <- file.info(paths, extra_cols = FALSE)
  files <- basename(paths)

  out <- data.table::data.table(
    file = files,
    format = tools::file_ext(files),
    recoded = grepl("__recoded\\.", files),
    size = ess_format_bytes(info$size),
    size_bytes = as.numeric(info$size),
    modified = info$mtime,
    path = paths
  )

  out[order(-out$size_bytes)]
}

#' @rdname ess_cache
#' @export
ess_cache_clear <- function(doi = NULL, confirm = interactive()) {
  dir <- ess_cache_dir(create = FALSE)
  if (is.null(dir) || !dir.exists(dir)) {
    cli::cli_alert_info("Nothing cached.")
    return(invisible(0L))
  }

  cached <- ess_cache_list()

  if (!is.null(doi)) {
    parts <- ess_parse_doi(doi)
    stem <- gsub("[^A-Za-z0-9._-]+", "-", paste0(parts$prefix, "_", parts$suffix))
    cached <- cached[startsWith(cached$file, stem)]
  }

  if (nrow(cached) == 0L) {
    cli::cli_alert_info("Nothing to delete.")
    return(invisible(0L))
  }

  total <- ess_format_bytes(sum(cached$size_bytes))

  if (isTRUE(confirm)) {
    cli::cli_alert_warning(
      "About to delete {nrow(cached)} cached file{?s} ({total}) from {.path {dir}}."
    )
    ans <- readline("Proceed? [y/N] ")
    if (!tolower(trimws(ans)) %in% c("y", "yes")) {
      cli::cli_alert_info("Cancelled; nothing was deleted.")
      return(invisible(0L))
    }
  }

  ok <- file.remove(cached$path)
  n <- sum(ok)
  cli::cli_alert_success("Deleted {n} cached file{?s} ({total}).")
  invisible(n)
}
