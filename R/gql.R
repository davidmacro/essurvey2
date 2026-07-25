# The metadata catalogue: GraphQL client -------------------------------------

# Every catalogue query is scoped to the published ESS instance.
ess_gql_instance <- "PUBLISHED"
ess_gql_agency <- "INT_ESSERIC"

# Catalogue answers are stable within a session, and the high-level functions
# call them repeatedly while resolving a round to a DOI, so memoise them here.
ess_memo <- new.env(parent = emptyenv())

#' Query the ESS metadata catalogue
#'
#' Sends a GraphQL query to the catalogue behind the ESS data portal and returns
#' the parsed `data` payload. The catalogue needs no user ID.
#'
#' The `ess_*` functions in this package cover the queries most users need.
#' `ess_gql()` is exported for the rest: the schema exposes considerably more
#' than is wrapped here, and it is introspectable.
#'
#' @param query A GraphQL query document.
#' @param variables A named list of query variables.
#' @param cache If `TRUE` (the default), reuse the answer to an identical query
#'   for the rest of the session. Pass `FALSE` to force a fresh request.
#'
#' @return The `data` element of the response, as a nested list.
#'
#' @examples
#' \dontrun{
#' ess_gql("
#'   query($id: ID!) {
#'     search {
#'       seriesMetadata(id: $id, instance: PUBLISHED, agencyId: INT_ESSERIC) {
#'         title { en }
#'       }
#'     }
#'   }",
#'   variables = list(id = ess_series_id())
#' )
#' }
#'
#' @export
ess_gql <- function(query, variables = list(), cache = TRUE) {
  ess_check_string(query, "query")
  if (!is.list(variables)) {
    cli::cli_abort("{.arg variables} must be a list.")
  }
  ess_check_bool(cache, "cache")

  key <- rlang::hash(list(query = query, variables = variables, url = ess_gql_url()))

  if (cache && !is.null(ess_memo[[key]])) {
    return(ess_memo[[key]])
  }

  # An empty R list serialises to [], which the server rejects because the
  # variables field must be an object. Omit it instead.
  body <- if (length(variables) == 0L) {
    list(query = query)
  } else {
    list(query = query, variables = variables)
  }

  req <- httr2::request(ess_gql_url())
  req <- httr2::req_user_agent(req, ess_user_agent())
  req <- httr2::req_timeout(req, 120)
  req <- httr2::req_body_json(req, body)
  req <- httr2::req_error(req, is_error = function(resp) FALSE)
  req <- httr2::req_retry(
    req,
    max_tries = 3L,
    is_transient = function(resp) {
      httr2::resp_status(resp) %in% c(408L, 429L, 500L, 502L, 503L, 504L)
    }
  )

  resp <- tryCatch(
    httr2::req_perform(req),
    httr2_failure = function(cnd) {
      cli::cli_abort(
        c(
          "Could not reach the ESS metadata catalogue.",
          "x" = conditionMessage(cnd),
          "i" = "{.url {ess_gql_url()}}"
        ),
        class = "essurvey2_error_offline"
      )
    }
  )

  if (httr2::resp_status(resp) >= 400L) {
    cli::cli_abort(
      c(
        "The ESS metadata catalogue rejected the request.",
        "x" = "HTTP {httr2::resp_status(resp)}: {httr2::resp_status_desc(resp)}",
        "i" = "{.url {ess_gql_url()}}"
      ),
      class = "essurvey2_error_catalogue"
    )
  }

  out <- httr2::resp_body_json(resp)

  if (length(out$errors) > 0L) {
    msgs <- vapply(out$errors, function(e) ess_chr(e$message), character(1))
    cli::cli_abort(
      c(
        "The ESS metadata catalogue returned {length(msgs)} error{?s}.",
        stats::setNames(msgs, rep("x", length(msgs)))
      ),
      class = "essurvey2_error_catalogue"
    )
  }

  if (cache) {
    assign(key, out$data, envir = ess_memo)
  }

  out$data
}

#' Forget cached catalogue answers
#'
#' Catalogue lookups are remembered for the duration of the session. Call this
#' to discard them, for instance after a new ESS round has been published while
#' your session was open.
#'
#' This does not touch downloaded data files; for those see [ess_cache_clear()].
#'
#' @return Invisibly, the number of cached answers discarded.
#'
#' @examples
#' ess_catalogue_refresh()
#'
#' @export
ess_catalogue_refresh <- function() {
  n <- length(ls(ess_memo, all.names = TRUE))
  rm(list = ls(ess_memo, all.names = TRUE), envir = ess_memo)
  cli::cli_alert_success("Discarded {n} cached catalogue answer{?s}.")
  invisible(n)
}
