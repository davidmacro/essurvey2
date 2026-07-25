# The metadata catalogue: what is published ----------------------------------

# The catalogue enforces a query depth limit and refuses batch queries that
# reach into nested collections. Aliasing several top-level fields into one
# document sidesteps both, and turns "one request per round" into one request.
ess_alias_query <- function(field, items, selection) {
  parts <- vapply(
    seq_along(items),
    function(i) {
      it <- items[[i]]
      sprintf(
        "a%d: %s(id: %s, version: %d, instance: %s, agencyId: %s) { %s }",
        i, field, ess_gql_string(it$id), as.integer(it$version),
        ess_gql_instance, ess_gql_agency, selection
      )
    },
    character(1)
  )
  paste0("{ search { ", paste(parts, collapse = "\n"), " } }")
}

# Run an aliased batch and return the results in the order they were requested,
# dropping aliases the catalogue answered with null.
ess_alias_perform <- function(field, items, selection, cache = TRUE) {
  if (length(items) == 0L) {
    return(list())
  }
  data <- ess_gql(ess_alias_query(field, items, selection), cache = cache)
  res <- data$search
  out <- res[paste0("a", seq_along(items))]
  names(out) <- NULL
  out
}

#' ESS survey series
#'
#' Lists the survey series published by the ESS ERIC through the metadata
#' catalogue. Besides the European Social Survey itself this covers the CRONOS
#' web panels, ESS Multilevel Data, ICOS Cities and EOSC-Future-SP9.
#'
#' The rest of this package works on the main ESS series. Use the series IDs
#' returned here with [ess_gql()] to explore the others.
#'
#' @param search_terms Terms to match against series titles. The catalogue
#'   requires at least one, so this defaults to `"ESS"`, which matches every
#'   series the ESS ERIC publishes.
#' @param cache If `TRUE` (the default), reuse the catalogue answer for the
#'   rest of the session.
#'
#' @return A `data.table` with columns `series_id`, `series_version` and
#'   `title`.
#'
#' @examples
#' \dontrun{
#' ess_series()
#' }
#'
#' @export
ess_series <- function(search_terms = "ESS", cache = TRUE) {
  if (!is.character(search_terms) || length(search_terms) == 0L) {
    cli::cli_abort("{.arg search_terms} must be a character vector.")
  }

  query <- "
    query($input: SearchInput!) {
      search {
        searchSeries(input: $input, instance: PUBLISHED, agencyId: INT_ESSERIC) {
          results { id version title { en } }
        }
      }
    }"

  data <- ess_gql(
    query,
    variables = list(input = list(first = 100L, searchTerms = as.list(search_terms))),
    cache = cache
  )

  results <- data$search$searchSeries$results

  out <- ess_rows_to_dt(
    results,
    function(s) {
      data.table::data.table(
        series_id = ess_chr(s$id),
        series_version = ess_int(s$version),
        title = ess_en(s$title)
      )
    },
    data.table::data.table(
      series_id = character(), series_version = integer(), title = character()
    )
  )

  out[order(out$title)]
}

#' The main ESS series
#'
#' Returns the catalogue ID and version of the European Social Survey series
#' itself, which the round and country functions are all scoped to.
#'
#' Resolved from [ess_series()] rather than hard-coded, so that a new catalogue
#' version does not require a package update.
#'
#' @param cache If `TRUE` (the default), reuse the catalogue answer for the
#'   rest of the session.
#'
#' @return A list with elements `id` and `version`.
#'
#' @examples
#' \dontrun{
#' ess_series_id()
#' }
#'
#' @export
ess_series_id <- function(cache = TRUE) {
  series <- ess_series(cache = cache)

  # Match the series title exactly where possible, and fall back to a pattern
  # so a cosmetic retitling does not break the package.
  hit <- series[series$title == "The European Social Survey (ESS)"]

  if (nrow(hit) == 0L) {
    hit <- series[grepl("^(the )?european social survey", series$title, ignore.case = TRUE)]
  }

  if (nrow(hit) == 0L) {
    cli::cli_abort(
      c(
        "Could not find the European Social Survey series in the catalogue.",
        "i" = "Series found: {.val {series$title}}.",
        "i" = "This usually means the catalogue changed; please report it."
      ),
      class = "essurvey2_error_catalogue"
    )
  }

  list(id = hit$series_id[[1L]], version = hit$series_version[[1L]])
}

# One row per study (round) in the ESS series, with its main data files.
ess_series_studies <- function(cache = TRUE) {
  series <- ess_series_id(cache = cache)

  query <- "
    query($id: ID!, $version: Int) {
      search {
        seriesMetadata(id: $id, version: $version, instance: PUBLISHED, agencyId: INT_ESSERIC) {
          id
          version
          studies {
            id
            version
            title { en }
            alternateTitle { en }
            mainDataFiles { id version title { en } }
          }
        }
      }
    }"

  data <- ess_gql(
    query,
    variables = list(id = series$id, version = series$version),
    cache = cache
  )

  # One table per study, covering all of its main data files at once; the
  # study-level columns recycle down its rows.
  out <- ess_rows_to_dt(
    data$search$seriesMetadata$studies,
    function(st) {
      files <- st$mainDataFiles
      if (length(files) == 0L) {
        return(NULL)
      }
      study_title <- ess_en(st$title)
      data.table::data.table(
        round = suppressWarnings(
          as.integer(sub("^ESS0*([0-9]+)$", "\\1", study_title))
        ),
        study_title = study_title,
        description = ess_en(st$alternateTitle),
        study_id = ess_chr(st$id),
        study_version = ess_int(st$version),
        datafile_id = vapply(files, function(df) ess_chr(df$id), character(1)),
        datafile_version = vapply(files, function(df) ess_int(df$version), integer(1)),
        file_label = vapply(files, function(df) ess_en(df$title), character(1))
      )
    },
    data.table::data.table(
      round = integer(), study_title = character(), description = character(),
      study_id = character(), study_version = integer(),
      datafile_id = character(), datafile_version = integer(),
      file_label = character()
    )
  )

  if (nrow(out) == 0L) {
    cli::cli_abort(
      "The catalogue returned no ESS studies.",
      class = "essurvey2_error_catalogue"
    )
  }

  parsed <- ess_parse_file_label(out$file_label)

  # Prefer the round parsed from the study title; fall back to the file label.
  out[, round := data.table::fifelse(is.na(round), parsed$round, round)]
  out[, file_kind := parsed$file_kind]
  out[, edition := parsed$edition]

  out[order(out$round, out$file_kind)]
}

#' Published ESS rounds
#'
#' Lists every published round of the European Social Survey together with its
#' main integrated data file. This is the starting point for [ess_round()], and
#' it downloads no survey data.
#'
#' One row per round. From round 10 onwards the ESS also publishes a separate
#' self-completion integrated file, which appears in [ess_data_files()] as kind
#' `"self_completion"` rather than here.
#'
#' @param cache If `TRUE` (the default), reuse the catalogue answer for the
#'   rest of the session. [ess_catalogue_refresh()] forgets it.
#'
#' @return A `data.table` with one row per round and columns `round`,
#'   `description`, `file_label`, `edition`, `datafile_id`,
#'   `datafile_version`, `study_id` and `study_version`.
#'
#' @examples
#' \dontrun{
#' ess_rounds()
#'
#' # Which rounds exist?
#' ess_rounds()$round
#' }
#'
#' @export
ess_rounds <- function(cache = TRUE) {
  all <- ess_series_studies(cache = cache)
  # %in% rather than ==, so an unparseable label yields no row instead of a row
  # of NAs.
  out <- all[all$file_kind %in% "integrated"]

  data.table::setcolorder(out, c(
    "round", "description", "file_label", "edition",
    "datafile_id", "datafile_version", "study_id", "study_version"
  ))

  out[, c("study_title", "file_kind") := NULL]
  out[order(out$round)][]
}

#' Data files in an ESS round
#'
#' Lists the data files published for one or more rounds. Besides the main
#' integrated file, a round typically also has the interviewer's questionnaire,
#' the contact forms, and — from round 10 — the self-completion files. See
#' [ess_file_kinds()] for what the kinds mean.
#'
#' The `doi` column is what [ess_data()] and [ess_download_file()] take.
#'
#' @param rounds Round numbers to list. When `NULL` (the default) every
#'   published round is listed.
#' @param kind Optionally, restrict to one or more kinds, as named by
#'   [ess_file_kinds()].
#' @param doi If `TRUE` (the default), resolve each file's DOI. This costs one
#'   extra catalogue request for the whole batch; set it to `FALSE` if you only
#'   want the listing.
#' @param cache If `TRUE` (the default), reuse catalogue answers for the rest
#'   of the session.
#'
#' @return A `data.table` with columns `round`, `file_kind`, `file_label`,
#'   `file_country`, `edition`, `description`, `is_main`, `doi` (unless
#'   `doi = FALSE`), `n_variables`, `datafile_id` and `datafile_version`.
#'
#'   `file_country` is set only for `"country"` files, and `file_kind` is `NA`
#'   for the handful of older files whose label follows none of the ESS naming
#'   conventions. Those rows are still listed, with their DOI, so nothing is
#'   hidden.
#'
#' @examples
#' \dontrun{
#' # Everything published for the most recent round.
#' ess_data_files(11)
#'
#' # Contact forms across all rounds.
#' ess_data_files(kind = "contact_forms")
#'
#' # Sample design data, which only some rounds publish.
#' ess_data_files(kind = "sample_design")
#'
#' # Countries held out of an integrated file and published separately.
#' ess_data_files(kind = "country")[, .(round, file_country, description)]
#' }
#'
#' @export
ess_data_files <- function(rounds = NULL, kind = NULL, doi = TRUE, cache = TRUE) {
  ess_check_bool(doi, "doi")

  studies <- ess_series_studies(cache = cache)
  available <- sort(unique(studies$round))

  if (!is.null(rounds)) {
    rounds <- ess_check_rounds(rounds, available = available)
    studies <- studies[studies$round %in% rounds]
  }

  # One row per study, not per main data file, or a round with two main files
  # would be requested twice.
  wanted <- unique(studies[, c("round", "study_id", "study_version", "description")])

  items <- lapply(seq_len(nrow(wanted)), function(i) {
    list(id = wanted$study_id[[i]], version = wanted$study_version[[i]])
  })

  selection <- "
    id
    version
    dataFiles { id version isMainFile label { en } alternateTitle { en } }"

  results <- ess_alias_perform("studyMetadata", items, selection, cache = cache)

  # Mapped over the request indices rather than over `results`, because each
  # answer's round comes from the `wanted` row it was asked for.
  out <- ess_rows_to_dt(
    seq_along(results),
    function(i) {
      st <- results[[i]]
      if (is.null(st) || length(st$dataFiles) == 0L) {
        return(NULL)
      }
      files <- st$dataFiles
      data.table::data.table(
        round = wanted$round[[i]],
        file_label = vapply(files, function(df) ess_en(df$label), character(1)),
        description = vapply(files, function(df) ess_en(df$alternateTitle), character(1)),
        is_main = vapply(files, function(df) ess_lgl(df$isMainFile), logical(1)),
        datafile_id = vapply(files, function(df) ess_chr(df$id), character(1)),
        datafile_version = vapply(files, function(df) ess_int(df$version), integer(1))
      )
    },
    data.table::data.table(
      round = integer(), file_label = character(), description = character(),
      is_main = logical(), datafile_id = character(), datafile_version = integer()
    )
  )

  if (nrow(out) == 0L) {
    # Same columns a populated result has, so a caller can subset either.
    proto <- data.table::data.table(
      round = integer(), file_kind = character(), file_label = character(),
      file_country = character(), edition = character(), description = character(),
      is_main = logical(), n_variables = integer(), datafile_id = character(),
      datafile_version = integer()
    )
    if (doi) {
      proto[, doi := character()]
      data.table::setcolorder(proto, c(
        "round", "file_kind", "file_label", "file_country", "edition",
        "description", "is_main", "doi", "n_variables", "datafile_id",
        "datafile_version"
      ))
    }
    return(proto[])
  }

  parsed <- ess_parse_file_label(out$file_label)
  out[, file_kind := parsed$file_kind]
  out[, file_country := parsed$file_country]
  out[, edition := parsed$edition]

  if (!is.null(kind)) {
    known <- ess_file_kinds()$file_kind
    bad <- setdiff(kind, known)
    if (length(bad) > 0L) {
      cli::cli_abort(c(
        "{cli::qty(length(bad))}Unknown file kind{?s} {.val {bad}}.",
        "i" = "Known kinds: {.val {known}}.",
        "i" = "See {.run essurvey2::ess_file_kinds()}."
      ))
    }
    out <- out[out$file_kind %in% kind]
  }

  if (doi && nrow(out) > 0L) {
    # ess_data_file_info() answers in the order it was asked, row for row.
    info <- ess_data_file_info(
      out$datafile_id,
      out$datafile_version,
      cache = cache
    )
    out[, doi := info$doi]
    out[, n_variables := info$n_variables]

    # The catalogue occasionally records one DOI against two different files --
    # in round 8, the contact forms and the interview time data share one. A
    # download would then quietly return whichever file the DOI really points
    # at, so say so rather than let it pass.
    ess_warn_duplicate_dois(out)
  } else {
    # Still declare the columns, so an empty result has the same shape as a
    # populated one and callers can subset it without special-casing.
    if (doi) {
      out[, doi := NA_character_]
    }
    out[, n_variables := NA_integer_]
  }

  cols <- c(
    "round", "file_kind", "file_label", "file_country", "edition",
    "description", "is_main", if (doi) "doi", "n_variables", "datafile_id",
    "datafile_version"
  )
  data.table::setcolorder(out, cols)
  out <- out[, cols, with = FALSE]

  out[order(out$round, out$file_kind)][]
}

# A DOI identifies one data file, so two files sharing one means the catalogue
# is inconsistent and a download cannot be trusted to match its label.
ess_warn_duplicate_dois <- function(files) {
  dois <- files$doi[!is.na(files$doi)]
  dupes <- unique(dois[duplicated(dois)])

  for (d in dupes) {
    labels <- files$file_label[!is.na(files$doi) & files$doi == d]
    cli::cli_warn(
      c(
        "The ESS catalogue gives {length(labels)} data files the same DOI {.val {d}}.",
        "x" = "{.val {labels}}",
        "i" = "Downloading it returns one file, which may not be the one you asked for.",
        "i" = "This is an error in the catalogue, not in {.pkg essurvey2}; report it to {.email essdatasupport@sikt.no}."
      ),
      class = "essurvey2_warning_duplicate_doi"
    )
  }

  invisible(dupes)
}

#' Details of an ESS data file
#'
#' Resolves catalogue IDs to the information needed to download a file — above
#' all its DOI — plus the countries it covers and how many variables it has.
#'
#' Accepts vectors, and resolves them in a single catalogue request.
#'
#' @param datafile_id One or more data file IDs, as returned by
#'   [ess_data_files()] or [ess_rounds()].
#' @param datafile_version Matching versions. The catalogue requires a version,
#'   so this is not optional; [ess_data_files()] returns it alongside the ID.
#' @param cache If `TRUE` (the default), reuse catalogue answers for the rest
#'   of the session.
#'
#' @return A `data.table` with one row per data file and columns `file_label`,
#'   `description`, `edition`, `doi`, `doi_url`, `published`, `n_variables`,
#'   `n_countries`, `countries`, `datafile_id` and `datafile_version`. The
#'   `countries` column is a list column of ISO-2 codes.
#'
#' @examples
#' \dontrun{
#' r <- ess_rounds()
#' info <- ess_data_file_info(r$datafile_id, r$datafile_version)
#' info[, .(file_label, doi, n_countries)]
#' }
#'
#' @export
ess_data_file_info <- function(datafile_id, datafile_version, cache = TRUE) {
  if (!is.character(datafile_id) || length(datafile_id) == 0L) {
    cli::cli_abort("{.arg datafile_id} must be a non-empty character vector.")
  }
  if (length(datafile_version) != length(datafile_id)) {
    cli::cli_abort(c(
      "{.arg datafile_version} must be the same length as {.arg datafile_id}.",
      "x" = "Got {length(datafile_version)} version{?s} for {length(datafile_id)} ID{?s}."
    ))
  }
  if (anyNA(datafile_version)) {
    cli::cli_abort(c(
      "{.arg datafile_version} must not contain {.code NA}.",
      "i" = "The catalogue does not serve metadata without a version."
    ))
  }

  # Resolve each distinct file once, then map back onto the input order.
  keys <- paste0(datafile_id, "@", datafile_version)
  uniq <- !duplicated(keys)
  items <- lapply(which(uniq), function(i) {
    list(id = datafile_id[[i]], version = as.integer(datafile_version[[i]]))
  })

  selection <- "
    id
    version
    label { en }
    alternateTitle { en }
    curatedVersion
    variableCount
    citation { internationalIdentifier internationalIdentifierUrl date }
    coverage {
      spatialCoverage {
        countryCategoriesControlledVocabulary { value label { en } }
      }
    }"

  results <- ess_alias_perform("dataFileMetadata", items, selection, cache = cache)

  out <- ess_rows_to_dt(
    results,
    function(df) {
      if (is.null(df)) {
        return(NULL)
      }
      countries <- df$coverage$spatialCoverage$countryCategoriesControlledVocabulary
      codes <- vapply(countries, function(c) ess_chr(c$value), character(1))

      data.table::data.table(
        file_label = ess_en(df$label),
        description = ess_en(df$alternateTitle),
        edition = ess_chr(df$curatedVersion),
        doi = ess_chr(df$citation$internationalIdentifier),
        doi_url = ess_chr(df$citation$internationalIdentifierUrl),
        published = substr(ess_chr(df$citation$date), 1L, 10L),
        n_variables = ess_int(df$variableCount),
        n_countries = length(codes),
        countries = list(codes),
        datafile_id = ess_chr(df$id),
        datafile_version = ess_int(df$version)
      )
    },
    data.table::data.table(
      file_label = character(), description = character(), edition = character(),
      doi = character(), doi_url = character(), published = character(),
      n_variables = integer(), n_countries = integer(), countries = list(),
      datafile_id = character(), datafile_version = integer()
    )
  )

  if (nrow(out) == 0L) {
    cli::cli_abort(
      c(
        "The catalogue returned nothing for {cli::qty(length(items))}{?this/these} data file{?s}.",
        "i" = "Check the IDs and versions against {.run essurvey2::ess_data_files()}."
      ),
      class = "essurvey2_error_catalogue"
    )
  }

  # Return in the order asked for, including any repeats.
  idx <- match(keys, paste0(out$datafile_id, "@", out$datafile_version))
  out[idx][]
}

#' Countries covered by the ESS
#'
#' Lists the countries that appear in the requested rounds, with their ISO
#' 3166-1 alpha-2 codes as used in the data's `cntry` column.
#'
#' @param rounds Round numbers. When `NULL` (the default) coverage is reported
#'   across every published round.
#' @param cache If `TRUE` (the default), reuse catalogue answers for the rest
#'   of the session.
#'
#' @return A `data.table` with columns `country_code`, `country_name`,
#'   `n_rounds` and `rounds`, the last being a list column of round numbers.
#'
#' @examples
#' \dontrun{
#' ess_countries()
#'
#' # Countries in the most recent round only.
#' ess_countries(11)
#' }
#'
#' @export
ess_countries <- function(rounds = NULL, cache = TRUE) {
  files <- ess_data_files(
    rounds = rounds, kind = "integrated", doi = FALSE, cache = cache
  )

  info <- ess_data_file_info(files$datafile_id, files$datafile_version, cache = cache)

  # One (country, round) table per data file; ess_data_file_info() answers row
  # for row, so row i of `info` is the file named by row i of `files`.
  long <- ess_rows_to_dt(
    seq_len(nrow(info)),
    function(i) {
      codes <- info$countries[[i]]
      if (length(codes) == 0L) {
        return(NULL)
      }
      data.table::data.table(country_code = codes, round = files$round[[i]])
    },
    data.table::data.table(country_code = character(), round = integer())
  )

  if (nrow(long) == 0L) {
    return(data.table::data.table(
      country_code = character(), country_name = character(),
      n_rounds = integer(), rounds = list()
    ))
  }

  lookup <- ess_country_names(rounds = rounds, cache = cache)

  out <- long[, list(
    n_rounds = length(unique(round)),
    rounds = list(sort(unique(round)))
  ), by = "country_code"]

  out[, country_name := unname(lookup[out$country_code])]
  data.table::setcolorder(out, c("country_code", "country_name", "n_rounds", "rounds"))
  out[order(out$country_name)][]
}

# A code -> English name lookup, taken from the coverage vocabulary.
ess_country_names <- function(rounds = NULL, cache = TRUE) {
  files <- ess_data_files(
    rounds = rounds, kind = "integrated", doi = FALSE, cache = cache
  )

  selection <- "
    id
    coverage {
      spatialCoverage {
        countryCategoriesControlledVocabulary { value label { en } }
      }
    }"

  items <- lapply(seq_len(nrow(files)), function(i) {
    list(id = files$datafile_id[[i]], version = files$datafile_version[[i]])
  })

  results <- ess_alias_perform("dataFileMetadata", items, selection, cache = cache)

  # Collect every (code, name) pair, then keep the first name per code. Building
  # the named vector one element at a time rescanned its own names on each code.
  pairs <- ess_rows_to_dt(
    results,
    function(df) {
      vocab <- df$coverage$spatialCoverage$countryCategoriesControlledVocabulary
      if (is.null(df) || length(vocab) == 0L) {
        return(NULL)
      }
      data.table::data.table(
        country_code = vapply(vocab, function(c) ess_chr(c$value), character(1)),
        country_name = vapply(vocab, function(c) ess_en(c$label), character(1))
      )
    },
    data.table::data.table(country_code = character(), country_name = character())
  )

  pairs <- unique(pairs[!is.na(pairs$country_code)], by = "country_code")

  stats::setNames(pairs$country_name, pairs$country_code)
}

#' Rounds a country took part in
#'
#' Reports which ESS rounds include a given country, and which data file carries
#' each of them.
#'
#' @param country An ISO 3166-1 alpha-2 country code, such as `"NL"`. Case is
#'   ignored.
#' @param cache If `TRUE` (the default), reuse catalogue answers for the rest
#'   of the session.
#'
#' @return A `data.table` with columns `country_code`, `country_name`, `round`,
#'   `file_label`, `edition`, `doi`, `datafile_id` and `datafile_version`.
#'
#' @examples
#' \dontrun{
#' ess_country_rounds("NL")
#' }
#'
#' @export
ess_country_rounds <- function(country, cache = TRUE) {
  ess_check_string(country, "country")
  country <- toupper(trimws(country))

  known <- ess_countries(cache = cache)

  if (!country %in% known$country_code) {
    close <- known$country_code[
      startsWith(known$country_code, substr(country, 1L, 1L))
    ]
    cli::cli_abort(
      c(
        "{.val {country}} is not a country in the ESS.",
        if (length(close) > 0L) c("i" = "Codes starting with {.val {substr(country, 1L, 1L)}}: {.val {close}}."),
        "i" = "{.run essurvey2::ess_countries()} lists all {nrow(known)} of them."
      ),
      class = "essurvey2_error_country"
    )
  }

  series <- ess_series_id(cache = cache)

  query <- "
    query($country: String!, $seriesId: ID!, $seriesVersion: Int) {
      search {
        countrySeriesMetadata(
          countryCode: $country
          seriesId: $seriesId
          seriesVersion: $seriesVersion
          instance: PUBLISHED
          agencyId: INT_ESSERIC
        ) {
          country { label { en } }
          studies {
            studyId
            studyVersion
            studyCitation { title { en } }
            dataFiles { id version alternateTitle { en } }
          }
        }
      }
    }"

  data <- ess_gql(
    query,
    variables = list(
      country = country, seriesId = series$id, seriesVersion = series$version
    ),
    cache = cache
  )

  meta <- data$search$countrySeriesMetadata
  country_name <- ess_en(meta$country$label)

  rounds <- ess_rounds(cache = cache)

  out <- ess_rows_to_dt(
    meta$studies,
    function(st) {
      files <- st$dataFiles
      if (length(files) == 0L) {
        return(NULL)
      }
      title <- ess_en(st$studyCitation$title)
      data.table::data.table(
        country_code = country,
        country_name = country_name,
        round = suppressWarnings(as.integer(sub("^ESS0*([0-9]+)$", "\\1", title))),
        datafile_id = vapply(files, function(df) ess_chr(df$id), character(1)),
        datafile_version = vapply(files, function(df) ess_int(df$version), integer(1)),
        description = vapply(files, function(df) ess_en(df$alternateTitle), character(1))
      )
    },
    data.table::data.table(
      country_code = character(), country_name = character(), round = integer(),
      datafile_id = character(), datafile_version = integer(),
      description = character()
    )
  )

  if (nrow(out) == 0L) {
    return(data.table::data.table(
      country_code = character(), country_name = character(), round = integer(),
      file_label = character(), edition = character(), doi = character(),
      datafile_id = character(), datafile_version = integer()
    ))
  }

  info <- ess_data_file_info(out$datafile_id, out$datafile_version, cache = cache)
  out[, file_label := info$file_label]
  out[, edition := info$edition]
  out[, doi := info$doi]

  # Only keep rounds the country is actually in: the catalogue lists a study
  # whenever the country participated, but keep the check honest anyway.
  out <- out[!is.na(out$round)]

  cols <- c(
    "country_code", "country_name", "round", "file_label", "edition", "doi",
    "datafile_id", "datafile_version"
  )
  out <- out[, cols, with = FALSE]
  out[order(out$round)][]
}

#' ESS questionnaire themes
#'
#' Lists the themes covered by the ESS questionnaire. Six are core themes,
#' repeated in every round; the rest are rotating modules that appear in
#' particular rounds.
#'
#' @param cache If `TRUE` (the default), reuse the catalogue answer for the
#'   rest of the session.
#'
#' @return A `data.table` with columns `theme_id`, `theme_name`, `label` and
#'   `is_core`.
#'
#' @examples
#' \dontrun{
#' ess_themes()
#'
#' # Just the rotating modules.
#' ess_themes()[is_core == FALSE]
#' }
#'
#' @export
ess_themes <- function(cache = TRUE) {
  series <- ess_series_id(cache = cache)

  query <- "
    query($id: ID!, $version: Int) {
      search {
        seriesMetadata(id: $id, version: $version, instance: PUBLISHED, agencyId: INT_ESSERIC) {
          themes { id name { en } label { en } isCoreTheme }
        }
      }
    }"

  data <- ess_gql(
    query,
    variables = list(id = series$id, version = series$version),
    cache = cache
  )

  out <- ess_rows_to_dt(
    data$search$seriesMetadata$themes,
    function(t) {
      data.table::data.table(
        theme_id = ess_chr(t$id),
        theme_name = ess_en(t$name),
        label = ess_en(t$label),
        is_core = isTRUE(ess_lgl(t$isCoreTheme))
      )
    },
    data.table::data.table(
      theme_id = character(), theme_name = character(),
      label = character(), is_core = logical()
    )
  )

  out[order(-out$is_core, out$theme_name)][]
}

#' Rounds covering a theme
#'
#' Reports which ESS rounds carry a given questionnaire theme.
#'
#' @param theme A theme name or theme ID, as listed by [ess_themes()]. Names
#'   are matched case-insensitively, and a unique partial match is accepted.
#' @param cache If `TRUE` (the default), reuse catalogue answers for the rest
#'   of the session.
#'
#' @return A `data.table` with columns `theme_name`, `is_core`, `round`,
#'   `description` and `study_id`.
#'
#' @examples
#' \dontrun{
#' ess_theme_rounds("Justice")
#' ess_theme_rounds("Human values")
#' }
#'
#' @export
ess_theme_rounds <- function(theme, cache = TRUE) {
  ess_check_string(theme, "theme")

  themes <- ess_themes(cache = cache)

  hit <- themes[themes$theme_id == theme]

  if (nrow(hit) == 0L) {
    hit <- themes[tolower(themes$theme_name) == tolower(trimws(theme))]
  }

  if (nrow(hit) == 0L) {
    hit <- themes[grepl(theme, themes$theme_name, ignore.case = TRUE, fixed = FALSE)]
  }

  if (nrow(hit) == 0L) {
    cli::cli_abort(
      c(
        "No ESS theme matches {.val {theme}}.",
        "i" = "{.run essurvey2::ess_themes()} lists all {nrow(themes)} themes."
      ),
      class = "essurvey2_error_theme"
    )
  }

  if (nrow(hit) > 1L) {
    cli::cli_abort(
      c(
        "{.val {theme}} matches {nrow(hit)} themes.",
        "x" = "{.val {hit$theme_name}}",
        "i" = "Use a more specific name, or the theme ID."
      ),
      class = "essurvey2_error_theme"
    )
  }

  series <- ess_series_id(cache = cache)

  query <- "
    query($themeId: ID!, $seriesId: ID!, $seriesVersion: Int) {
      search {
        themeSeriesMetadata(
          themeId: $themeId
          seriesId: $seriesId
          seriesVersion: $seriesVersion
          instance: PUBLISHED
          agencyId: INT_ESSERIC
        ) {
          themeDetails { id name { en } isCoreTheme }
          studies { id version title { en } alternateTitle { en } }
        }
      }
    }"

  data <- ess_gql(
    query,
    variables = list(
      themeId = hit$theme_id[[1L]],
      seriesId = series$id,
      seriesVersion = series$version
    ),
    cache = cache
  )

  meta <- data$search$themeSeriesMetadata

  out <- ess_rows_to_dt(
    meta$studies,
    function(st) {
      title <- ess_en(st$title)
      data.table::data.table(
        theme_name = hit$theme_name[[1L]],
        is_core = hit$is_core[[1L]],
        round = suppressWarnings(as.integer(sub("^ESS0*([0-9]+)$", "\\1", title))),
        description = ess_en(st$alternateTitle),
        study_id = ess_chr(st$id)
      )
    },
    data.table::data.table(
      theme_name = character(), is_core = logical(), round = integer(),
      description = character(), study_id = character()
    )
  )

  out[order(out$round)][]
}
