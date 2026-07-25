# Small internal helpers -----------------------------------------------------

ess_check_string <- function(x, arg, call = rlang::caller_env()) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    cli::cli_abort(
      "{.arg {arg}} must be a single non-empty string.",
      call = call
    )
  }
  invisible(x)
}

ess_check_bool <- function(x, arg, call = rlang::caller_env()) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    cli::cli_abort(
      "{.arg {arg}} must be {.code TRUE} or {.code FALSE}.",
      call = call
    )
  }
  invisible(x)
}

# Pull the English string out of a {en: "..."} localised-text node.
ess_en <- function(x) {
  if (is.null(x)) {
    return(NA_character_)
  }
  if (is.list(x)) {
    v <- x[["en"]]
    if (is.null(v) || length(v) == 0L) {
      return(NA_character_)
    }
    return(as.character(v)[[1L]])
  }
  as.character(x)[[1L]]
}

# NULL, empty and zero-length values all become a single NA of the right type.
ess_chr <- function(x) {
  if (is.null(x) || length(x) == 0L) NA_character_ else as.character(x)[[1L]]
}

ess_int <- function(x) {
  if (is.null(x) || length(x) == 0L) NA_integer_ else as.integer(x)[[1L]]
}

ess_lgl <- function(x) {
  if (is.null(x) || length(x) == 0L) NA else as.logical(x)[[1L]]
}

ess_user_agent <- function() {
  paste0(
    "essurvey2/", as.character(utils::packageVersion("essurvey2")),
    " (R ", getRversion(), "; +https://api.ess.sikt.no)"
  )
}

# Format a byte count the way a download progress message should read.
#
# sprintf() per unit rather than format(): format() pads a vector to a common
# width, so a mixed batch would come back as "  2.0 KB" alongside "512.5 KB".
ess_format_bytes <- function(bytes) {
  bytes <- as.numeric(bytes)
  out <- rep(NA_character_, length(bytes))

  ok <- which(!is.na(bytes))
  if (length(ok) == 0L) {
    return(out)
  }

  b <- bytes[ok]
  tier <- findInterval(b, c(1024, 1024^2, 1024^3)) + 1L

  out[ok] <- data.table::fcase(
    tier == 1L, paste0(b, " B"),
    tier == 2L, sprintf("%.1f KB", b / 1024),
    tier == 3L, sprintf("%.1f MB", b / 1024^2),
    tier == 4L, sprintf("%.2f GB", b / 1024^3)
  )

  out
}

# An ESS data file label such as "ESS10SCe03_2" encodes the round, which
# questionnaire the file belongs to, and the edition. Decompose it so that
# callers can select a file by round and kind rather than by label.
#
# The main integrated file carries no tag, and `c("" = x)` is a parse error in
# R, so the two halves of the mapping are kept as parallel vectors and looked up
# with match().
ess_file_tags <- c(
  "", "SC", "INT", "CF", "SCCF", "MTMM", "ALC", "TIME", "MC", "SDDF"
)

ess_file_kinds_chr <- c(
  "integrated",
  "self_completion",
  "interviewer",
  "contact_forms",
  "self_completion_contact_forms",
  "mtmm",
  "alcohol",
  "interview_time",
  "media_claims",
  "sample_design"
)

# Map label tags to kinds, falling back to the lower-cased tag for a
# questionnaire this package has not seen before. Vectorised over `tag`.
# A tag that is not a known questionnaire abbreviation, but is exactly two
# upper-case letters, is an ISO country code: the ESS publishes supplementary
# files for countries held out of an integrated file. Deciding by exclusion
# rather than against a country list means no maintenance when a new country
# appears; the cost is that a future two-letter questionnaire abbreviation would
# have to be added to ess_file_tags to avoid being read as a country.
ess_tag_to_kind <- function(tag) {
  out <- ess_file_kinds_chr[match(tag, ess_file_tags)]

  unknown <- !is.na(tag) & is.na(out)
  out[unknown] <- data.table::fifelse(
    grepl("^[A-Z]{2}$", tag[unknown]), "country", tolower(tag[unknown])
  )

  out
}

# Real ESS labels come in three shapes, and the edition marker is written
# inconsistently across rounds:
#   ESS11e04_2      ESS10SCe03_2    ESS7SDDFe1_2     standard
#   ESS3CF_ed1_1    ESS4CF_e02_1                     underscore before edition
#   ESS2IT          ESS3LV                           country file, no edition
# A few one-offs, such as ESS3e03F32, match none of these; they yield a round
# where one can be read and NA elsewhere, rather than a wrong guess.
ess_parse_file_label <- function(label) {
  label <- as.character(label)
  n <- length(label)

  edition_re <- "^ESS([0-9]+)([A-Z]*)_?ed?([0-9]+)(?:_([0-9]+))?$"
  country_re <- "^ESS([0-9]+)([A-Z]{2})$"

  # Stack the per-label match vectors into a character matrix, so each capture
  # group can be read off as a column and the whole table built in one pass.
  # A trailing group that did not participate may be absent rather than empty,
  # hence `least` (how much of a match counts) alongside `width`.
  groups <- function(re, least, width) {
    m <- regmatches(label, regexec(re, label))
    hit <- lengths(m) >= least
    out <- matrix(NA_character_, nrow = n, ncol = width)
    if (any(hit)) {
      out[hit, ] <- do.call(rbind, lapply(m[hit], function(g) {
        length(g) <- width
        g
      }))
    }
    out
  }

  ed <- groups(edition_re, 4L, 5L) # label, round, tag, major, minor
  ct <- groups(country_re, 3L, 3L) # label, round, tag

  # A label with an edition wins; the country shape is only consulted for the
  # labels the first regex could not read at all.
  has_ed <- !is.na(ed[, 1L])
  has_ct <- !has_ed & !is.na(ct[, 1L])

  round <- rep(NA_integer_, n)
  round[has_ed] <- as.integer(ed[has_ed, 2L])
  round[has_ct] <- as.integer(ct[has_ct, 2L])

  tag <- rep(NA_character_, n)
  tag[has_ed] <- ed[has_ed, 3L]
  tag[has_ct] <- ct[has_ct, 3L]

  # An absent or empty minor marker means edition x.0.
  minor <- ed[, 5L]
  minor[is.na(minor) | !nzchar(minor)] <- "0"

  edition <- rep(NA_character_, n)
  edition[has_ed] <- paste0(
    as.integer(ed[has_ed, 4L]), ".", as.integer(minor[has_ed])
  )

  kind <- ess_tag_to_kind(tag)

  data.table::data.table(
    file_label = label,
    round = round,
    file_tag = tag,
    file_kind = kind,
    file_country = data.table::fifelse(
      !is.na(kind) & kind == "country", tag, NA_character_
    ),
    edition = edition
  )
}

#' Data file kinds
#'
#' The ESS publishes several data files per round. `ess_file_kinds()` lists the
#' kinds this package recognises, as accepted by the `kind` argument of
#' [ess_round()] and [ess_data_files()].
#'
#' Not every kind exists for every round — most exist for only a few.
#' [ess_data_files()] shows what a given round actually has.
#'
#' Alongside these there is a `"country"` kind, covering the supplementary files
#' the ESS publishes for countries held out of an integrated file, such as
#' `ESS4AT` (Austria, whose fieldwork ran late) or `ESS3LV` (Latvia, which has no
#' design weights). Those files are identified by an ISO country code rather than
#' a questionnaire abbreviation, so they share one kind; the `file_country`
#' column of [ess_data_files()] says which country each one is for.
#'
#' @return A `data.table` with columns `file_kind`, `file_tag` and
#'   `description`.
#'
#' @examples
#' ess_file_kinds()
#'
#' @export
ess_file_kinds <- function() {
  tagged <- data.table::data.table(
    file_kind = ess_file_kinds_chr,
    file_tag = ess_file_tags,
    description = c(
      "Main integrated file: the survey responses. This is the default.",
      "Integrated file for the self-completion mode (round 10 onwards).",
      "Responses to the interviewer's questionnaire.",
      "Contact forms, describing fieldwork attempts and outcomes.",
      "Contact forms for the self-completion mode.",
      "Test variables from a supplementary multitrait-multimethod questionnaire.",
      "Alcohol consumption variables.",
      "Interview timing data.",
      "Media claims data.",
      "Sample design data (SDDF): design weights and clustering information."
    )
  )

  # "country" has no fixed tag: the tag is whichever ISO code the file is for.
  data.table::rbindlist(list(
    tagged,
    data.table::data.table(
      file_kind = "country",
      file_tag = NA_character_,
      description = paste(
        "A country held out of the integrated file, named by ISO country code.",
        "See the file_country column of ess_data_files()."
      )
    )
  ))
}

# Turn a round argument into a validated integer vector.
ess_check_rounds <- function(rounds, available = NULL, call = rlang::caller_env()) {
  if (is.null(rounds)) {
    cli::cli_abort("{.arg rounds} must not be {.code NULL}.", call = call)
  }

  num <- suppressWarnings(as.numeric(rounds))
  if (anyNA(num)) {
    bad <- rounds[is.na(num)]
    cli::cli_abort(
      c(
        "{.arg rounds} must be whole numbers.",
        "x" = "Cannot interpret {.val {bad}} as a round number."
      ),
      call = call
    )
  }

  if (any(num != trunc(num)) || any(num < 1)) {
    cli::cli_abort(
      "{.arg rounds} must be positive whole numbers.",
      call = call
    )
  }

  out <- unique(as.integer(num))

  if (!is.null(available)) {
    missing <- setdiff(out, available)
    if (length(missing) > 0L) {
      cli::cli_abort(
        c(
          paste0(
            "{cli::qty(length(missing))}ESS round{?s} {.val {missing}} ",
            "{cli::qty(length(missing))}{?is/are} not published."
          ),
          "i" = "Available: {.val {sort(available)}}.",
          "i" = "See {.run essurvey2::ess_rounds()}."
        ),
        call = call
      )
    }
  }

  out
}

# Convert a nested GraphQL list into a data.table using a row-builder function,
# which may return any number of rows per element -- or NULL for an element that
# yields none. Returns a zero-row table with the right columns when nothing
# survives, so downstream data.table code never has to special-case.
#
# This is the one place the package turns catalogue lists into tables: building a
# single-row data.table per row and appending it to a list costs thousands of
# allocations on a full round, so builders return one table per element instead.
ess_rows_to_dt <- function(x, builder, prototype) {
  if (is.null(x) || length(x) == 0L) {
    return(prototype)
  }

  rows <- lapply(x, builder)
  rows <- rows[!vapply(rows, is.null, logical(1))]

  if (length(rows) == 0L) {
    return(prototype)
  }

  data.table::rbindlist(rows, fill = TRUE, use.names = TRUE)
}

# Escape a string for interpolation into a GraphQL string literal. The aliased
# batch queries are built with sprintf(), and ess_data_file_info() takes its IDs
# from the caller, so an unescaped quote would otherwise let a caller inject
# arbitrary fields into the query document.
ess_gql_string <- function(x) {
  x <- as.character(x)
  x <- gsub("\\", "\\\\", x, fixed = TRUE)
  x <- gsub('"', '\\"', x, fixed = TRUE)
  x <- gsub("\n", "\\n", x, fixed = TRUE)
  x <- gsub("\r", "\\r", x, fixed = TRUE)
  x <- gsub("\t", "\\t", x, fixed = TRUE)
  paste0('"', x, '"')
}
