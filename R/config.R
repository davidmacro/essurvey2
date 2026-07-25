# Configuration: endpoints and the ESS user ID -------------------------------

ess_default_api_url <- "https://api.ess.sikt.no"
ess_default_gql_url <- "https://api.nsd.no/graphql"

#' Endpoints used by essurvey2
#'
#' `ess_api_url()` returns the base URL of the ESS data API, which serves the
#' data files. `ess_gql_url()` returns the URL of the metadata catalogue's
#' GraphQL endpoint, which serves everything else.
#'
#' Both can be overridden with an option, which is mainly useful for pointing
#' the package at a staging deployment:
#'
#' ```r
#' options(essurvey2.api_url = "https://api.ess.stage.sikt.no")
#' ```
#'
#' @return A length-one character vector holding a URL, without a trailing
#'   slash.
#'
#' @examples
#' ess_api_url()
#' ess_gql_url()
#'
#' @name ess_endpoints
NULL

#' @rdname ess_endpoints
#' @export
ess_api_url <- function() {
  url <- getOption("essurvey2.api_url", default = ess_default_api_url)
  ess_check_string(url, "options(essurvey2.api_url)")
  sub("/+$", "", url)
}

#' @rdname ess_endpoints
#' @export
ess_gql_url <- function() {
  url <- getOption("essurvey2.gql_url", default = ess_default_gql_url)
  ess_check_string(url, "options(essurvey2.gql_url)")
  sub("/+$", "", url)
}

# Canonical UUID shape. The API documents its own example as
# "12345678-1234-1234-1234-12345678900", which is not a canonical UUID, so this
# pattern is used only to decide whether to nudge the user -- never to reject.
ess_uuid_pattern <- paste0(
  "^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-",
  "[0-9a-fA-F]{4}-[0-9a-fA-F]{12}$"
)

#' Your ESS API user ID
#'
#' Every request to the ESS data API must carry a user ID. It is not a secret
#' and it does not authenticate you: the ESS uses it to record download
#' statistics. Get yours by logging in at <https://ess.sikt.no/en/api>.
#'
#' `ess_user_id()` resolves the ID in this order:
#'
#' 1. the `essurvey2.user_id` option, as set by `ess_set_user_id()`;
#' 2. the `ESS_USER_ID` environment variable.
#'
#' `ess_set_user_id()` sets the option for the current session only. To
#' configure the ID permanently, add a line to your user `.Renviron` file —
#' `usethis::edit_r_environ()` opens it:
#'
#' ```
#' ESS_USER_ID=your-user-id-here
#' ```
#'
#' Restart R afterwards, since `.Renviron` is only read at startup. A system
#' environment variable of the same name works equally well and is the better
#' choice on a shared or containerised machine.
#'
#' @param error If `TRUE` (the default) an unset ID raises an error. If
#'   `FALSE`, `ess_user_id()` returns `NULL` instead, which is useful when
#'   probing whether the package is configured.
#' @param user_id A user ID as a length-one character vector.
#'
#' @return `ess_user_id()` returns the ID as a length-one character vector, or
#'   `NULL` when it is unset and `error = FALSE`. `ess_set_user_id()` returns
#'   the previous value invisibly.
#'
#' @examples
#' # Is the package configured?
#' is.null(ess_user_id(error = FALSE))
#'
#' # Set an ID for this session only.
#' old <- ess_set_user_id("a1b2c3d4-e5f6-7890-abcd-ef1234567890")
#' ess_user_id()
#'
#' # Restore whatever was configured before.
#' ess_set_user_id(old)
#'
#' @name ess_user_id
NULL

#' @rdname ess_user_id
#' @export
ess_user_id <- function(error = TRUE) {
  id <- getOption("essurvey2.user_id", default = NULL)

  if (is.null(id) || !is.character(id) || length(id) != 1L || !nzchar(id)) {
    id <- Sys.getenv("ESS_USER_ID", unset = "")
  }

  id <- trimws(as.character(id))

  if (!nzchar(id) || is.na(id)) {
    if (!error) {
      return(NULL)
    }
    cli::cli_abort(
      c(
        "No ESS API user ID is configured.",
        "i" = "Get one by logging in at {.url https://ess.sikt.no/en/api}.",
        "*" = "For this session only: {.run essurvey2::ess_set_user_id(\"your-id\")}.",
        "*" = "Permanently: add {.code ESS_USER_ID=your-id} to your {.file .Renviron}, then restart R.",
        "i" = "{.run usethis::edit_r_environ()} opens that file."
      ),
      class = "essurvey2_error_user_id"
    )
  }

  ess_check_user_id(id)
  id
}

#' @rdname ess_user_id
#' @export
ess_set_user_id <- function(user_id) {
  old <- getOption("essurvey2.user_id", default = NULL)

  if (is.null(user_id)) {
    options(essurvey2.user_id = NULL)
    return(invisible(old))
  }

  ess_check_string(user_id, "user_id")
  user_id <- trimws(user_id)
  ess_check_user_id(user_id)

  if (!grepl(ess_uuid_pattern, user_id)) {
    cli::cli_warn(c(
      "{.arg user_id} does not look like a UUID.",
      "i" = "ESS user IDs normally look like {.val a1b2c3d4-e5f6-7890-abcd-ef1234567890}.",
      "i" = "Accepting it anyway; the API will reject it if it is wrong."
    ))
  }

  options(essurvey2.user_id = user_id)
  invisible(old)
}

# Reject input that cannot possibly be a user ID, without being so strict that
# a legitimate but non-canonical ID is refused.
ess_check_user_id <- function(user_id, arg = "user_id", call = rlang::caller_env()) {
  if (!nzchar(user_id)) {
    cli::cli_abort(
      "{.arg {arg}} must not be empty.",
      class = "essurvey2_error_user_id",
      call = call
    )
  }

  if (grepl("[^0-9a-fA-F-]", user_id)) {
    bad <- unique(strsplit(gsub("[0-9a-fA-F-]", "", user_id), "")[[1]])
    cli::cli_abort(
      c(
        "{.arg {arg}} contains characters that cannot appear in an ESS user ID.",
        "x" = "Unexpected: {.val {bad}}.",
        "i" = "A user ID consists of hexadecimal digits and dashes only.",
        "i" = "Find yours at {.url https://ess.sikt.no/en/api}."
      ),
      class = "essurvey2_error_user_id",
      call = call
    )
  }

  if (nchar(user_id) < 20L) {
    cli::cli_abort(
      c(
        "{.arg {arg}} is too short to be an ESS user ID.",
        "x" = "Got {nchar(user_id)} character{?s}; expected around 36.",
        "i" = "Find yours at {.url https://ess.sikt.no/en/api}."
      ),
      class = "essurvey2_error_user_id",
      call = call
    )
  }

  invisible(user_id)
}

#' Report the current essurvey2 configuration
#'
#' Prints the endpoints in use, whether a user ID is configured, and where
#' downloads are cached. Useful when a call fails and you want to see what the
#' package thinks its settings are.
#'
#' The user ID is shown truncated, so the output is safe to paste into a bug
#' report.
#'
#' @return A named list with the resolved settings, invisibly.
#'
#' @examples
#' ess_config()
#'
#' @export
ess_config <- function() {
  id <- ess_user_id(error = FALSE)

  dir <- ess_cache_dir(create = FALSE)

  cfg <- list(
    api_url = ess_api_url(),
    gql_url = ess_gql_url(),
    user_id = id,
    user_id_source = ess_user_id_source(),
    cache_dir = dir,
    cache_writable = !is.null(dir) && ess_cache_writable(dir)
  )

  cli::cli_h3("essurvey2 configuration")
  cli::cli_dl(c(
    "data API" = "{.url {cfg$api_url}}",
    "catalogue" = "{.url {cfg$gql_url}}",
    "user ID" = if (is.null(id)) {
      "{.strong not configured} - see {.help essurvey2::ess_user_id}"
    } else {
      paste0("{.val ", ess_mask_id(id), "} (from ", cfg$user_id_source, ")")
    },
    "cache" = if (is.null(dir)) {
      "{.strong off}"
    } else if (cfg$cache_writable) {
      "{.path {dir}}"
    } else {
      paste0("{.path ", dir, "} ({.strong not yet agreed to} - see {.help essurvey2::ess_cache_dir})")
    }
  ))

  invisible(cfg)
}

ess_user_id_source <- function() {
  opt <- getOption("essurvey2.user_id", default = NULL)
  if (!is.null(opt) && is.character(opt) && length(opt) == 1L && nzchar(opt)) {
    return("options(essurvey2.user_id)")
  }
  if (nzchar(Sys.getenv("ESS_USER_ID", unset = ""))) {
    return("ESS_USER_ID")
  }
  NA_character_
}

ess_mask_id <- function(id) {
  if (nchar(id) <= 8L) {
    return(strrep("*", nchar(id)))
  }
  paste0(substr(id, 1L, 8L), strrep("*", nchar(id) - 8L))
}
