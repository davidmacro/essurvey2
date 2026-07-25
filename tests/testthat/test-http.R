test_that("ess_parse_doi() accepts the ways a DOI gets written", {
  expected <- list(prefix = "10.21338", suffix = "ess11e04_2")

  expect_equal(ess_parse_doi("10.21338/ess11e04_2"), expected)
  expect_equal(ess_parse_doi("doi:10.21338/ess11e04_2"), expected)
  expect_equal(ess_parse_doi("DOI:10.21338/ess11e04_2"), expected)
  expect_equal(ess_parse_doi("https://doi.org/10.21338/ess11e04_2"), expected)
  expect_equal(ess_parse_doi("http://dx.doi.org/10.21338/ess11e04_2"), expected)
  expect_equal(ess_parse_doi("  10.21338/ess11e04_2  "), expected)
})

test_that("ess_parse_doi() rejects things that are not DOIs", {
  expect_error(ess_parse_doi("ess11e04_2"), class = "essurvey2_error_doi")
  expect_error(ess_parse_doi("10.21338"), class = "essurvey2_error_doi")
  expect_error(ess_parse_doi("10.21338/"), class = "essurvey2_error_doi")
  expect_error(ess_parse_doi("https://example.com/thing"), class = "essurvey2_error_doi")
  expect_error(ess_parse_doi(""), "single non-empty string")
  expect_error(ess_parse_doi(NA_character_), "single non-empty string")
})

test_that("the data file URL carries userId and fileFormat", {
  url <- ess_data_file_url("10.21338/ess11e04_2", user_id = fake_user_id)

  expect_match(url, "^https://api\\.ess\\.sikt\\.no/v1/data/dataFile/10\\.21338/ess11e04_2\\?")
  expect_match(url, paste0("userId=", fake_user_id), fixed = TRUE)
  expect_match(url, "fileFormat=parquet", fixed = TRUE)
})

test_that("recodeMissingValues is emitted as a bare flag, or not at all", {
  # The API treats the parameter as present/absent: any value switches recoding
  # on, so FALSE has to mean omitting it entirely.
  on <- ess_data_file_url("10.21338/ess6e02_7", user_id = fake_user_id,
                          recode_missings = TRUE)
  off <- ess_data_file_url("10.21338/ess6e02_7", user_id = fake_user_id,
                           recode_missings = FALSE)

  expect_match(on, "&recodeMissingValues$")
  expect_false(grepl("recodeMissingValues", off, fixed = TRUE))
  expect_false(grepl("recodeMissingValues=", on, fixed = TRUE))
})

test_that("the URL honours the format and the api_url option", {
  for (fmt in c("parquet", "csv", "sav", "dta")) {
    url <- ess_data_file_url("10.21338/ess6e02_7", user_id = fake_user_id, format = fmt)
    expect_match(url, paste0("fileFormat=", fmt), fixed = TRUE)
  }

  withr::with_options(list(essurvey2.api_url = "https://api.ess.stage.sikt.no"), {
    expect_match(
      ess_data_file_url("10.21338/ess6e02_7", user_id = fake_user_id),
      "^https://api\\.ess\\.stage\\.sikt\\.no/"
    )
  })
})

test_that("an unsupported format is refused before any request is made", {
  expect_error(
    ess_data_file_url("10.21338/ess6e02_7", user_id = fake_user_id, format = "xlsx"),
    "must be one of"
  )
  expect_error(
    ess_data_file_url("10.21338/ess6e02_7", user_id = fake_user_id, format = "rds"),
    "must be one of"
  )
})

test_that("format matching is case insensitive", {
  expect_match(
    ess_data_file_url("10.21338/ess6e02_7", user_id = fake_user_id, format = "PARQUET"),
    "fileFormat=parquet", fixed = TRUE
  )
})

test_that("a bad user ID is caught before the network is touched", {
  expect_error(
    ess_data_file_url("10.21338/ess6e02_7", user_id = "nope@example.com"),
    class = "essurvey2_error_user_id"
  )
})

test_that("recode_missings must be a single TRUE or FALSE", {
  expect_error(
    ess_data_file_url("10.21338/ess6e02_7", user_id = fake_user_id, recode_missings = NA),
    "must be"
  )
  expect_error(
    ess_data_file_url("10.21338/ess6e02_7", user_id = fake_user_id,
                      recode_missings = c(TRUE, FALSE)),
    "must be"
  )
})
