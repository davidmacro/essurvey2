# Tests against the live ESS services. Skipped when offline, on CRAN, or -- for
# the download tests -- when no user ID is configured.

test_that("the catalogue lists the ESS series", {
  skip_if_no_catalogue()

  series <- ess_series()
  expect_s3_class(series, "data.table")
  expect_named(series, c("series_id", "series_version", "title"))
  expect_true(any(grepl("European Social Survey", series$title)))

  id <- ess_series_id()
  expect_type(id, "list")
  expect_match(id$id, "^[0-9a-f-]{36}$")
  expect_true(id$version > 0L)
})

test_that("ess_rounds() returns one row per published round", {
  skip_if_no_catalogue()

  rounds <- ess_rounds()
  expect_s3_class(rounds, "data.table")
  expect_gte(nrow(rounds), 11L)

  # One row per round, even though round 10 onwards has a second main file.
  expect_equal(anyDuplicated(rounds$round), 0L)
  expect_equal(rounds$round, sort(rounds$round))
  expect_false(anyNA(rounds$datafile_id))
  expect_false(anyNA(rounds$datafile_version))
  expect_true(all(grepl("^ESS[0-9]+e[0-9]", rounds$file_label)))
})

test_that("ess_data_files() finds the several files of a round and their DOIs", {
  skip_if_no_catalogue()

  files <- ess_data_files(11)
  expect_s3_class(files, "data.table")
  expect_true("integrated" %in% files$file_kind)
  expect_true("contact_forms" %in% files$file_kind)

  integrated <- files[files$file_kind == "integrated"]
  expect_equal(nrow(integrated), 1L)
  expect_match(integrated$doi, "^10\\.21338/ess11e")
  expect_gt(integrated$n_variables, 500L)
})

test_that("round 10 has both a face-to-face and a self-completion main file", {
  skip_if_no_catalogue()

  files <- ess_data_files(10, doi = FALSE)
  main <- files[files$is_main]
  expect_setequal(main$file_kind, c("integrated", "self_completion"))
})

test_that("filtering to a kind a round lacks gives an empty table, not an error", {
  skip_if_no_catalogue()

  out <- ess_data_files(1, kind = "self_completion")
  expect_s3_class(out, "data.table")
  expect_equal(nrow(out), 0L)
  # The shape must match a populated result so callers can subset it blindly.
  expect_true(all(c("doi", "file_country") %in% names(out)))
})

test_that("nearly every published label is recognised", {
  skip_if_no_catalogue()

  all <- suppressWarnings(ess_data_files(doi = FALSE))
  expect_gt(nrow(all), 50L)

  # One real file, ESS3e03F32, follows none of the naming conventions. Allow a
  # small number of such stragglers, but not a parser that has stopped working.
  expect_lt(sum(is.na(all$file_kind)), 3L)

  # Everything is still listed, parsed or not.
  expect_false(anyNA(all$file_label))
  expect_false(anyNA(all$datafile_id))
})

test_that("the supplementary country files are found", {
  skip_if_no_catalogue()

  out <- suppressWarnings(ess_data_files(kind = "country"))
  expect_gt(nrow(out), 0L)
  expect_true(all(grepl("^[A-Z]{2}$", out$file_country)))
  # Austria's late round 4 fieldwork is one of these.
  expect_true("AT" %in% out$file_country)
})

test_that("sample design files are found where they exist", {
  skip_if_no_catalogue()

  out <- suppressWarnings(ess_data_files(kind = "sample_design"))
  expect_gt(nrow(out), 0L)
  expect_true(all(grepl("SDDF", out$file_label)))
})

test_that("a sample design file downloads and has the design columns", {
  skip_if_no_api()

  doi <- suppressWarnings(ess_data_files(kind = "sample_design"))$doi[[1L]]
  dt <- ess_data(doi, quiet = TRUE)

  expect_s3_class(dt, "data.table")
  expect_true(all(c("cntry", "idno", "psu", "stratum", "prob") %in% names(dt)))
})

test_that("a supplementary country file downloads and holds one country", {
  skip_if_no_api()

  files <- suppressWarnings(ess_data_files(kind = "country"))
  row <- files[!is.na(files$doi)][1L]

  dt <- ess_data(row$doi, quiet = TRUE)
  expect_gt(nrow(dt), 0L)
  expect_equal(unique(dt$cntry), row$file_country)
})

test_that("country coverage is reported with ISO-2 codes and names", {
  skip_if_no_catalogue()

  countries <- ess_countries()
  expect_s3_class(countries, "data.table")
  expect_true(all(grepl("^[A-Z]{2}$", countries$country_code)))
  expect_true("NL" %in% countries$country_code)
  expect_equal(countries$country_name[countries$country_code == "NL"], "Netherlands")
  expect_false(anyNA(countries$country_name))

  nl <- ess_country_rounds("NL")
  expect_gte(nrow(nl), 10L)
  expect_true(all(nl$country_code == "NL"))
  expect_false(anyNA(nl$doi))
})

test_that("an unknown country code is refused with the valid ones to hand", {
  skip_if_no_catalogue()

  expect_error(ess_country_rounds("XX"), class = "essurvey2_error_country")
  expect_error(ess_country_rounds("Netherlands"), class = "essurvey2_error_country")
})

test_that("themes are listed, with the core ones flagged", {
  skip_if_no_catalogue()

  themes <- ess_themes()
  expect_s3_class(themes, "data.table")
  expect_gte(nrow(themes), 20L)
  expect_equal(sum(themes$is_core), 6L)
  expect_true("Human values" %in% themes$theme_name)

  # A core theme runs through every round.
  hv <- ess_theme_rounds("Human values")
  expect_gte(nrow(hv), 11L)
  expect_true(all(hv$is_core))
})

test_that("an ambiguous or unknown theme is reported as such", {
  skip_if_no_catalogue()

  expect_error(ess_theme_rounds("no such theme"), class = "essurvey2_error_theme")
})

test_that("variable metadata is available without downloading data", {
  skip_if_no_catalogue()

  vars <- ess_variables(11)
  expect_s3_class(vars, "data.table")
  expect_gt(nrow(vars), 500L)
  expect_true(all(c("cntry", "essround", "ppltrst", "agea") %in% vars$variable_name))
  expect_equal(anyDuplicated(vars$variable_name), 0L)

  info <- ess_variable_info("ppltrst", round = 11)
  expect_equal(info$name, "ppltrst")
  expect_true(nzchar(info$question))
  expect_s3_class(info$codes, "data.table")
  # 0-10 scale plus refusal, don't know and no answer.
  expect_true(all(c("77", "88", "99") %in% info$codes$value[info$codes$is_missing]))
  expect_false(any(info$codes$is_missing[info$codes$value %in% as.character(0:10)]))
})

test_that("missing codes differ between variables", {
  skip_if_no_catalogue()

  codes <- ess_missing_codes(11, variables = c("agea", "gndr", "ppltrst"), quiet = TRUE)

  expect_equal(codes$value[codes$variable_name == "agea"], "999")
  expect_equal(codes$value[codes$variable_name == "gndr"], "9")
  expect_setequal(codes$value[codes$variable_name == "ppltrst"], c("77", "88", "99"))
})

test_that("a round downloads into a data.table", {
  skip_if_no_api()

  dt <- ess_round(11, select = c("essround", "cntry", "idno", "agea", "gndr"),
                  quiet = TRUE)

  expect_s3_class(dt, "data.table")
  expect_named(dt, c("essround", "cntry", "idno", "agea", "gndr"))
  expect_gt(nrow(dt), 30000L)
  expect_true(all(dt$essround == 11L))
  expect_true(all(grepl("^[A-Z]{2}$", unique(dt$cntry))))
})

test_that("recode_missings = TRUE really does remove the sentinel codes", {
  skip_if_no_api()

  recoded <- ess_round(11, select = c("agea"), quiet = TRUE)
  raw <- ess_round(11, select = c("agea"), recode_missings = FALSE, quiet = TRUE)

  # 999 is "not available" for agea.
  expect_false(999L %in% recoded$agea)
  expect_true(999L %in% raw$agea)
  expect_gt(sum(is.na(recoded$agea)), 0L)
  expect_equal(sum(is.na(raw$agea)), 0L)
  expect_equal(nrow(recoded), nrow(raw))
})

test_that("locally recoding raw data matches what the API does", {
  skip_if_no_api()

  raw <- ess_round(11, select = c("essround", "agea", "ppltrst"),
                   recode_missings = FALSE, quiet = TRUE)
  api <- ess_round(11, select = c("essround", "agea", "ppltrst"), quiet = TRUE)

  ess_recode_missings(raw, quiet = TRUE)

  expect_equal(raw$agea, api$agea)
  expect_equal(raw$ppltrst, api$ppltrst)
})

test_that("several rounds stack, and are distinguishable", {
  skip_if_no_api()

  dt <- ess_round(10:11, select = c("essround", "cntry"), quiet = TRUE)
  expect_setequal(unique(dt$essround), c(10L, 11L))

  one <- ess_round(11, select = c("essround", "cntry"), quiet = TRUE)
  expect_equal(sum(dt$essround == 11L), nrow(one))
})

test_that("the self-completion file of round 10 is a different file", {
  skip_if_no_api()

  f2f <- ess_round(10, select = "essround", quiet = TRUE)
  sc <- ess_round(10, kind = "self_completion", select = "essround", quiet = TRUE)

  expect_gt(nrow(sc), 0L)
  expect_false(nrow(sc) == nrow(f2f))
})

test_that("ess_country() returns only that country", {
  skip_if_no_api()

  nl <- ess_country("NL", rounds = 11, select = c("essround", "agea"), quiet = TRUE)

  expect_s3_class(nl, "data.table")
  expect_equal(unique(nl$cntry), "NL")
  expect_gt(nrow(nl), 500L)
  # cntry is added even though it was not selected, since it is what filtered.
  expect_true("cntry" %in% names(nl))
})

test_that("asking for a country in a round it skipped warns and returns nothing", {
  skip_if_no_api()

  # Albania took part in round 6 only. Two warnings fire: one when the round
  # list is checked, one when the filter comes back empty.
  warnings <- capture_warnings(
    out <- ess_country("AL", rounds = 11, select = "essround", quiet = TRUE)
  )

  expect_match(warnings, "did not take part", all = FALSE)
  expect_match(warnings, "No rows for", all = FALSE)
  expect_equal(nrow(out), 0L)
})

test_that("the API's error codes map onto classed conditions", {
  skip_if_no_api()

  # A DOI that resolves to nothing.
  expect_error(
    ess_download_file("10.21338/nosuchfile_9", use_cache = FALSE, quiet = TRUE),
    class = "essurvey2_error_doi"
  )

  # A study-level DOI, which the endpoint does not serve.
  expect_error(
    ess_download_file("10.21338/NSD-ESS10-2020", use_cache = FALSE, quiet = TRUE),
    class = "essurvey2_error_doi"
  )

  # A well-formed but unregistered user ID.
  expect_error(
    ess_download_file("10.21338/ess11e04_2", user_id = "00000000-1111-2222-3333-444444444444",
                      use_cache = FALSE, quiet = TRUE),
    class = "essurvey2_error_user_id"
  )
})

test_that("the request ID is surfaced so it can be quoted to support", {
  skip_if_no_api()

  cnd <- tryCatch(
    ess_download_file("10.21338/nosuchfile_9", use_cache = FALSE, quiet = TRUE),
    essurvey2_error_api = function(e) e
  )
  expect_match(conditionMessage(cnd), "request ID")
  expect_equal(cnd$code, 201L)
})

test_that("the second download of a file comes from the cache", {
  skip_if_no_api()

  withr::local_options(list(essurvey2.cache_dir = withr::local_tempdir()))

  doi <- ess_data_files(11, kind = "interviewer")$doi

  expect_equal(nrow(ess_cache_list()), 0L)
  ess_download_file(doi, quiet = TRUE)
  expect_equal(nrow(ess_cache_list()), 1L)

  # A second call must not re-download: it reports the cache hit instead.
  expect_message(ess_download_file(doi), "Using cached")
})

test_that("csv is served as well as parquet, with the same rows", {
  skip_if_no_api()

  doi <- ess_data_files(11, kind = "interviewer")$doi

  pq <- ess_data(doi, quiet = TRUE)
  csv <- ess_data(doi, format = "csv", quiet = TRUE)

  expect_s3_class(csv, "data.table")
  expect_equal(nrow(csv), nrow(pq))
  expect_setequal(names(csv), names(pq))
})
