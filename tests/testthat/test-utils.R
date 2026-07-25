test_that("data file labels decompose into round, kind and edition", {
  parsed <- essurvey2:::ess_parse_file_label(c(
    "ESS1e06_7", "ESS11e04_2", "ESS10SCe03_2", "ESS10INTe03_1",
    "ESS10CFe03_2", "ESS10SCCFe03_0", "ESS1MTMMe01_1", "ESS11ALCe02_1",
    "ESS1CFe01", "ESS7SDDFe1_2", "ESS6TIMEe01_1", "ESS6MCe04"
  ))

  expect_equal(parsed$round, c(1L, 11L, 10L, 10L, 10L, 10L, 1L, 11L, 1L, 7L, 6L, 6L))
  expect_equal(
    parsed$file_kind,
    c("integrated", "integrated", "self_completion", "interviewer",
      "contact_forms", "self_completion_contact_forms", "mtmm", "alcohol",
      "contact_forms", "sample_design", "interview_time", "media_claims")
  )
  expect_equal(
    parsed$edition,
    c("6.7", "4.2", "3.2", "3.1", "3.2", "3.0", "1.1", "2.1", "1.0",
      "1.2", "1.1", "4.0")
  )
})

test_that("the edition marker is read in all the spellings the ESS uses", {
  # Rounds 3 and 4 put an underscore before it, and round 3 writes "ed".
  parsed <- essurvey2:::ess_parse_file_label(c("ESS3CF_ed1_1", "ESS4CF_e02_1"))

  expect_equal(parsed$round, c(3L, 4L))
  expect_equal(parsed$file_kind, c("contact_forms", "contact_forms"))
  expect_equal(parsed$edition, c("1.1", "2.1"))
})

test_that("a two-letter tag that is not a known kind is read as a country", {
  # The ESS publishes supplementary files for countries held out of an
  # integrated file, labelled with an ISO code instead of an abbreviation.
  parsed <- essurvey2:::ess_parse_file_label(c(
    "ESS2IT", "ESS3LV", "ESS4AT", "ESS5ATe1_1", "ESS9ALe01"
  ))

  expect_equal(parsed$round, c(2L, 3L, 4L, 5L, 9L))
  expect_true(all(parsed$file_kind == "country"))
  expect_equal(parsed$file_country, c("IT", "LV", "AT", "AT", "AL"))
  # Only some of them carry an edition.
  expect_equal(parsed$edition, c(NA, NA, NA, "1.1", "1.0"))
})

test_that("two-letter kind abbreviations are not mistaken for countries", {
  parsed <- essurvey2:::ess_parse_file_label(c("ESS6CFe02", "ESS6MCe04", "ESS10SCe03_2"))

  expect_equal(parsed$file_kind, c("contact_forms", "media_claims", "self_completion"))
  expect_true(all(is.na(parsed$file_country)))
})

test_that("the round in a label is not confused by the edition digits", {
  # ESS10e03_3 must be round 10 edition 3.3, not round 1 or edition 03_3.
  parsed <- essurvey2:::ess_parse_file_label("ESS10e03_3")
  expect_equal(parsed$round, 10L)
  expect_equal(parsed$edition, "3.3")
  expect_equal(parsed$file_kind, "integrated")
})

test_that("an unrecognised label yields NA rather than a wrong guess", {
  # ESS3e03F32 is a real label that follows none of the conventions.
  parsed <- essurvey2:::ess_parse_file_label(c(
    "CRON2W1e01", "nonsense", "", "ESS3e03F32"
  ))
  expect_true(all(is.na(parsed$round)))
  expect_true(all(is.na(parsed$file_kind)))
})

test_that("an unknown questionnaire tag degrades to the lower-cased tag", {
  parsed <- essurvey2:::ess_parse_file_label("ESS12XYZe01_0")
  expect_equal(parsed$round, 12L)
  expect_equal(parsed$file_kind, "xyz")
  expect_true(is.na(parsed$file_country))
})

test_that("ess_file_kinds() documents every tag the parser knows", {
  kinds <- ess_file_kinds()
  expect_s3_class(kinds, "data.table")
  expect_named(kinds, c("file_kind", "file_tag", "description"))
  expect_true(all(c("integrated", "sample_design", "country") %in% kinds$file_kind))
  # The main integrated file is the one with no tag.
  expect_equal(kinds$file_tag[kinds$file_kind == "integrated"], "")
  # "country" has no fixed tag, since the tag is the country code.
  expect_true(is.na(kinds$file_tag[kinds$file_kind == "country"]))
  expect_false(anyNA(kinds$description))
  expect_equal(anyDuplicated(kinds$file_kind), 0L)

  # Every tag the parser maps must be documented, and vice versa.
  expect_setequal(
    kinds$file_kind[!is.na(kinds$file_tag)],
    essurvey2:::ess_file_kinds_chr
  )
})

test_that("two files sharing a DOI is flagged as a catalogue error", {
  files <- data.table::data.table(
    file_label = c("ESS8TIMEe01", "ESS8CFe03", "ESS8e02_3"),
    doi = c("10.21338/ess8timee01", "10.21338/ess8timee01", "10.21338/ess8e02_3")
  )

  expect_warning(
    dupes <- essurvey2:::ess_warn_duplicate_dois(files),
    class = "essurvey2_warning_duplicate_doi"
  )
  expect_equal(dupes, "10.21338/ess8timee01")

  suppressWarnings({
    w <- capture_warnings(essurvey2:::ess_warn_duplicate_dois(files))
  })
  expect_match(w, "ESS8TIMEe01", all = FALSE)
  expect_match(w, "not in .*essurvey2", all = FALSE)
})

test_that("distinct DOIs, and missing ones, raise nothing", {
  ok <- data.table::data.table(
    file_label = c("a", "b"),
    doi = c("10.21338/a", "10.21338/b")
  )
  expect_silent(essurvey2:::ess_warn_duplicate_dois(ok))

  # Two NA DOIs are not duplicates of each other.
  unknown <- data.table::data.table(
    file_label = c("a", "b"),
    doi = c(NA_character_, NA_character_)
  )
  expect_silent(essurvey2:::ess_warn_duplicate_dois(unknown))
})

test_that("round arguments are validated and de-duplicated", {
  check <- function(...) essurvey2:::ess_check_rounds(...)

  expect_equal(check(c(3, 1, 3)), c(3L, 1L))
  expect_equal(check("5"), 5L)
  expect_equal(check(1:3), 1:3)

  expect_error(check("nope"), "whole numbers")
  expect_error(check(1.5), "positive whole numbers")
  expect_error(check(0), "positive whole numbers")
  expect_error(check(-1), "positive whole numbers")
  expect_error(check(NULL), "must not be")
})

test_that("rounds outside the published set are rejected, singular and plural", {
  check <- function(...) essurvey2:::ess_check_rounds(...)

  expect_error(check(99, available = 1:11), "ESS round 99 is not published")
  expect_error(check(c(98, 99), available = 1:11), "ESS rounds 98 and 99 are not published")
  expect_equal(check(c(10, 11), available = 1:11), c(10L, 11L))
})

test_that("byte counts format at a sensible scale", {
  fmt <- function(x) essurvey2:::ess_format_bytes(x)

  expect_equal(fmt(512), "512 B")
  expect_equal(fmt(2048), "2.0 KB")
  expect_equal(fmt(5 * 1024^2), "5.0 MB")
  expect_equal(fmt(2 * 1024^3), "2.00 GB")
  expect_true(is.na(fmt(NA)))
})

test_that("localised text nodes are reduced to their English string", {
  en <- function(x) essurvey2:::ess_en(x)

  expect_equal(en(list(en = "Austria")), "Austria")
  expect_equal(en("Austria"), "Austria")
  expect_true(is.na(en(NULL)))
  expect_true(is.na(en(list())))
  expect_true(is.na(en(list(no = "Austria"))))
})

test_that("IDs are escaped before they reach a GraphQL string literal", {
  esc <- function(x) essurvey2:::ess_gql_string(x)

  expect_equal(esc("abc"), '"abc"')
  expect_equal(esc('a"b'), '"a\\"b"')
  expect_equal(esc("a\\b"), '"a\\\\b"')
  expect_equal(esc("a\nb"), '"a\\nb"')
})

test_that("a quote in a data file ID cannot inject fields into the query", {
  # ess_data_file_info() takes IDs from the caller, and the aliased batch query
  # is built with sprintf(), so the interpolation has to be escaped.
  query <- essurvey2:::ess_alias_query(
    "dataFileMetadata",
    list(list(id = 'x" ) { id } evil: junk(id: "y', version = 1L)),
    "id"
  )

  # The injected text survives, but inertly: its quote is escaped, so the ID
  # remains a single string literal delimited by exactly two unescaped quotes.
  expect_match(query, 'x\\\\" \\) \\{ id \\} evil: junk\\(id: \\\\"y')

  bare <- gsub('\\"', "", query, fixed = TRUE)
  expect_equal(lengths(regmatches(bare, gregexpr('"', bare, fixed = TRUE))), 2L)

  # One aliased field, not two.
  expect_equal(lengths(regmatches(query, gregexpr("dataFileMetadata", query))), 1L)
})

test_that("the alias query keeps the requested order and arguments", {
  query <- essurvey2:::ess_alias_query(
    "studyMetadata",
    list(list(id = "one", version = 3L), list(id = "two", version = 4L)),
    "id"
  )

  expect_match(query, 'a1: studyMetadata\\(id: "one", version: 3')
  expect_match(query, 'a2: studyMetadata\\(id: "two", version: 4')
  expect_match(query, "instance: PUBLISHED")
  expect_match(query, "agencyId: INT_ESSERIC")
})

test_that("ess_rows_to_dt() maps builders that yield no rows, one row, or many", {
  proto <- data.table::data.table(a = character(), b = integer())
  build <- function(x) {
    if (x == 0L) NULL else data.table::data.table(a = letters[seq_len(x)], b = x)
  }

  # Empty input, and input where every element yields nothing, both give the
  # prototype rather than a shapeless zero-column table.
  expect_identical(essurvey2:::ess_rows_to_dt(list(), build, proto), proto)
  expect_identical(essurvey2:::ess_rows_to_dt(NULL, build, proto), proto)
  expect_named(essurvey2:::ess_rows_to_dt(list(0L, 0L), build, proto), c("a", "b"))
  expect_equal(nrow(essurvey2:::ess_rows_to_dt(list(0L, 0L), build, proto)), 0L)

  out <- essurvey2:::ess_rows_to_dt(list(1L, 0L, 3L), build, proto)
  expect_equal(out$a, c("a", "a", "b", "c"))
  expect_equal(out$b, c(1L, 3L, 3L, 3L))
})

test_that("byte counts do not pick up padding when formatted as a batch", {
  # format() pads a vector to a common width; each entry must stand alone.
  expect_equal(
    essurvey2:::ess_format_bytes(c(2048, 512.5 * 1024)),
    c("2.0 KB", "512.5 KB")
  )
  expect_equal(
    essurvey2:::ess_format_bytes(c(512, NA, 2 * 1024^3)),
    c("512 B", NA, "2.00 GB")
  )
  expect_equal(essurvey2:::ess_format_bytes(numeric()), character())
})

test_that("scalar coercion helpers turn absent values into typed NA", {
  expect_true(is.na(essurvey2:::ess_chr(NULL)))
  expect_true(is.na(essurvey2:::ess_int(list())))
  expect_true(is.na(essurvey2:::ess_lgl(NULL)))
  expect_equal(essurvey2:::ess_int("11"), 11L)
  expect_true(essurvey2:::ess_lgl(TRUE))
})
