# On-disk cache for downloaded data files -------------------------------------

# Default ceiling on the whole cache. ESS parquet files run from a few hundred
# kilobytes to some tens of megabytes, so a gigabyte holds a good many rounds
# while still being a bound rather than an invitation.
ess_cache_default_max_size <- 1024^3

# Consent is recorded by a file inside the cache directory, so it survives
# between sessions without the package writing anywhere else. The leading dot
# also keeps it out of ess_cache_list(), which passes all.files = FALSE.
ess_cache_marker <- ".essurvey2-cache-consent"

# A refusal is remembered for the session, so declining does not mean being
# asked again on the next download.
ess_cache_state <- new.env(parent = emptyenv())

#' Where essurvey2 caches downloads
#'
#' Data files are cached on disk so that repeating a call — in the same session
#' or a later one — does not re-download several megabytes. The default location
#' is `tools::R_user_dir("essurvey2", "cache")`.
#'
#' @section Asking first:
#' That default lives in your home filespace, so essurvey2 does not write there
#' until you say it may. The first time a download would be cached in an
#' interactive session, the package names the directory and asks. Answer once
#' and the answer is remembered, by a marker file inside the cache directory
#' itself.
#'
#' Outside an interactive session — `Rscript`, a knitted document, a CI job —
#' there is nobody to ask, so nothing is written to the home filespace and
#' downloads go to the session's temporary directory instead. Two ways to turn
#' persistent caching on without a prompt:
#'
#' ```r
#' options(essurvey2.cache_consent = TRUE)   # or the ESSURVEY2_CACHE_CONSENT
#' Sys.setenv(ESSURVEY2_CACHE_CONSENT = "true")   # environment variable
#' ```
#'
#' Naming a directory yourself counts as consent in its own right, and is the
#' clearest option in a pipeline:
#'
#' ```r
#' options(essurvey2.cache_dir = "D:/data/ess-cache")
#' ```
#'
#' Set `options(essurvey2.cache_dir = FALSE)` to disable caching everywhere,
#' which is equivalent to passing `use_cache = FALSE` to every call.
#'
#' @section Keeping it bounded:
#' The cache is capped, and trimmed on every write: once it would exceed the
#' cap, the least recently modified files are deleted until it fits. The
#' default cap is 1 GB. Change it, in bytes, with
#'
#' ```r
#' options(essurvey2.cache_max_size = 250 * 1024^2)   # 250 MB
#' options(essurvey2.cache_max_size = FALSE)          # no cap
#' ```
#'
#' [ess_cache_list()] shows what is held and [ess_cache_clear()] empties it.
#'
#' @param create If `TRUE`, create the directory when it does not exist, asking
#'   for consent first if the default location has not been agreed to yet.
#'
#' @return The cache directory as a length-one character vector, or `NULL` when
#'   caching is disabled or consent for the default location is absent.
#'
#' @examples
#' # Where downloads would be cached. Never prompts and never creates anything.
#' ess_cache_dir()
#'
#' @export
ess_cache_dir <- function(create = FALSE) {
  opt <- getOption("essurvey2.cache_dir", default = NULL)

  if (isFALSE(opt)) {
    return(NULL)
  }

  # A directory the user named is a directory the user chose, so there is
  # nothing to ask about.
  if (!is.null(opt)) {
    ess_check_string(opt, "options(essurvey2.cache_dir)")
    return(if (create) ess_cache_create(opt) else opt)
  }

  dir <- ess_cache_default_dir()

  # Reporting the location is not writing to it, so a query never prompts.
  # This is what keeps ess_cache_dir() and ess_config() safe to run anywhere.
  if (!create) {
    return(dir)
  }

  if (!ess_cache_consented(dir)) {
    return(NULL)
  }

  ess_cache_create(dir)
}

# The default location, behind its own function so that tests can point it
# somewhere harmless instead of at the real home filespace.
ess_cache_default_dir <- function() {
  tools::R_user_dir("essurvey2", which = "cache")
}

# Whether a download would actually be cached in `dir` as things stand: either
# the user named it, or the default location has been agreed to. Asks nothing,
# so it is safe to call from ess_config().
ess_cache_writable <- function(dir) {
  if (!is.null(getOption("essurvey2.cache_dir", default = NULL))) {
    return(TRUE)
  }
  file.exists(file.path(dir, ess_cache_marker)) || isTRUE(ess_cache_consent_setting())
}

ess_cache_create <- function(dir) {
  if (!dir.exists(dir)) {
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

# TRUE, FALSE, or NULL when the user has expressed no preference either way.
ess_cache_consent_setting <- function() {
  opt <- getOption("essurvey2.cache_consent", default = NULL)
  if (isTRUE(opt)) {
    return(TRUE)
  }
  if (isFALSE(opt)) {
    return(FALSE)
  }

  env <- trimws(Sys.getenv("ESSURVEY2_CACHE_CONSENT", unset = ""))
  if (!nzchar(env)) {
    return(NULL)
  }
  if (tolower(env) %in% c("true", "yes", "1")) {
    return(TRUE)
  }
  if (tolower(env) %in% c("false", "no", "0")) {
    return(FALSE)
  }
  NULL
}

ess_cache_consented <- function(dir) {
  if (file.exists(file.path(dir, ess_cache_marker))) {
    return(TRUE)
  }

  setting <- ess_cache_consent_setting()
  if (isTRUE(setting)) {
    ess_cache_record_consent(dir)
    return(TRUE)
  }
  if (isFALSE(setting)) {
    return(FALSE)
  }

  if (isTRUE(ess_cache_state$declined)) {
    return(FALSE)
  }

  # Nobody to ask: fall back to the session's temporary directory rather than
  # writing to the home filespace unasked.
  if (!interactive()) {
    return(FALSE)
  }

  cli::cli_alert_info(
    "essurvey2 can keep downloads between sessions, in {.path {dir}}."
  )
  cap <- ess_cache_max_size()
  cli::cli_bullets(c(
    " " = "This is your own filespace, so it needs your say-so.",
    " " = "Declining is not fatal: downloads then go to this session's temporary directory.",
    " " = if (is.finite(cap)) {
      "The cache is capped at {ess_format_bytes(cap)} and trimmed oldest-first."
    } else {
      "The cap is switched off, so the cache will grow until you clear it."
    }
  ))
  ans <- readline("Cache downloads there? [y/N] ")

  if (!tolower(trimws(ans)) %in% c("y", "yes")) {
    ess_cache_state$declined <- TRUE
    cli::cli_alert_info(
      "Not caching. {.code options(essurvey2.cache_dir = \"...\")} picks a location instead."
    )
    return(FALSE)
  }

  ess_cache_record_consent(dir)
  TRUE
}

ess_cache_record_consent <- function(dir) {
  ess_cache_create(dir)
  marker <- file.path(dir, ess_cache_marker)
  if (!file.exists(marker)) {
    writeLines(
      c(
        "essurvey2 may cache downloaded ESS data files in this directory.",
        "Delete this file to withdraw that consent.",
        "See ?ess_cache_dir."
      ),
      marker
    )
  }
  invisible(dir)
}

# The cap on the whole cache, in bytes. Inf means no cap.
ess_cache_max_size <- function() {
  opt <- getOption("essurvey2.cache_max_size", default = NULL)

  if (is.null(opt)) {
    return(ess_cache_default_max_size)
  }
  if (isFALSE(opt) || (is.numeric(opt) && length(opt) == 1L && is.infinite(opt))) {
    return(Inf)
  }
  if (!is.numeric(opt) || length(opt) != 1L || is.na(opt) || opt <= 0) {
    cli::cli_abort(c(
      "{.code options(essurvey2.cache_max_size)} must be a single positive number of bytes.",
      "i" = "Use {.code FALSE} for no cap at all."
    ))
  }
  as.numeric(opt)
}

# Trim the cache to its cap, deleting least-recently-modified files first.
# Called after a download lands, so the cache is bounded without the user
# having to remember ess_cache_clear().
ess_cache_prune <- function(dir = NULL, quiet = FALSE) {
  cap <- ess_cache_max_size()
  if (is.null(dir) || !is.finite(cap) || !dir.exists(dir)) {
    return(invisible(0L))
  }

  cached <- ess_cache_list()
  if (nrow(cached) == 0L) {
    return(invisible(0L))
  }

  # Newest first, so the cumulative total says which files fall past the cap.
  cached <- cached[order(-as.numeric(cached$modified))]
  over <- cumsum(cached$size_bytes) > cap

  if (!any(over)) {
    return(invisible(0L))
  }

  doomed <- cached[over]
  freed <- ess_format_bytes(sum(doomed$size_bytes))
  n <- sum(file.remove(doomed$path))

  if (!quiet && n > 0L) {
    cli::cli_alert_info(
      "Cache was over {ess_format_bytes(cap)}; removed {n} least recently used file{?s} ({freed})."
    )
  }

  invisible(n)
}

# The part of a cache file name that identifies the data file, with anything
# awkward in a path replaced. Kept separate from ess_cache_key() because
# ess_cache_clear() matches on it.
ess_cache_stem <- function(doi) {
  parts <- ess_parse_doi(doi)
  gsub("[^A-Za-z0-9._-]+", "-", paste0(parts$prefix, "_", parts$suffix))
}

# A cache file name identifies exactly what was downloaded: the same DOI in a
# different format, or with a different missing-value treatment, is a different
# file.
ess_cache_key <- function(doi, format = "parquet", recode_missings = TRUE) {
  format <- ess_check_format(format)
  ess_check_bool(recode_missings, "recode_missings")

  paste0(
    ess_cache_stem(doi),
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
    # Match the whole stem, up to the marker that separates it from the format
    # and missing-value suffix. Without the marker, clearing ess6e02_6 would
    # also delete ess6e02_60.
    cached <- cached[startsWith(cached$file, paste0(ess_cache_stem(doi), "__"))]
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
