# Reading is tested against files written locally, so no network is involved.

make_parquet <- function(env = parent.frame()) {
  path <- withr::local_tempfile(fileext = ".parquet", .local_envir = env)
  arrow::write_parquet(
    data.frame(
      essround = c(11L, 11L, 11L),
      cntry = c("NL", "NL", "DE"),
      agea = c(45L, 999L, 31L),
      ppltrst = c(7L, 88L, 3L),
      stringsAsFactors = FALSE
    ),
    path
  )
  path
}

test_that("a parquet file is read as a data.table", {
  dt <- ess_read_file(make_parquet())

  expect_s3_class(dt, "data.table")
  expect_equal(nrow(dt), 3L)
  expect_named(dt, c("essround", "cntry", "agea", "ppltrst"))
})

test_that("select reads only the requested columns, in the requested order", {
  dt <- ess_read_file(make_parquet(), select = c("cntry", "essround"))

  expect_named(dt, c("cntry", "essround"))
  expect_equal(nrow(dt), 3L)
})

test_that("selecting a column that is not there is an error naming it", {
  expect_error(
    ess_read_file(make_parquet(), select = c("cntry", "nope")),
    class = "essurvey2_error_select"
  )
  expect_error(
    ess_read_file(make_parquet(), select = "nope"),
    "\"nope\""
  )
})

test_that("the format is inferred from the extension, and can be overridden", {
  path <- withr::local_tempfile(fileext = ".csv")
  data.table::fwrite(data.frame(cntry = "NL", agea = 45L), path)

  dt <- ess_read_file(path)
  expect_s3_class(dt, "data.table")
  expect_equal(dt$cntry, "NL")

  # An unknown extension needs an explicit format.
  odd <- withr::local_tempfile(fileext = ".dat")
  file.copy(path, odd)
  expect_error(ess_read_file(odd), "Cannot tell what format")
  expect_equal(nrow(ess_read_file(odd, format = "csv")), 1L)
})

test_that("reading a missing file fails clearly", {
  expect_error(ess_read_file(file.path(tempdir(), "no-such-file.parquet")), "No such file")
})

test_that("select must be a character vector", {
  expect_error(ess_read_file(make_parquet(), select = 1:2), "character vector")
  expect_error(ess_read_file(make_parquet(), select = NA_character_), "character vector")
})

test_that("a cached file is reused without a user ID being needed", {
  dir <- withr::local_tempdir()
  local_clean_config()

  withr::local_options(list(essurvey2.cache_dir = dir))

  # Pre-seed the cache the way a previous download would have.
  key <- ess_cache_key("10.21338/ess11e04_2", "parquet", TRUE)
  arrow::write_parquet(data.frame(cntry = "NL", essround = 11L), file.path(dir, key))

  expect_null(ess_user_id(error = FALSE))

  dt <- ess_data("10.21338/ess11e04_2", quiet = TRUE)
  expect_equal(nrow(dt), 1L)
  expect_equal(dt$cntry, "NL")
})
