test_that("the cache key distinguishes DOI, format and missing-value treatment", {
  expect_equal(
    ess_cache_key("10.21338/ess11e04_2"),
    "10.21338_ess11e04_2__recoded.parquet"
  )
  expect_equal(
    ess_cache_key("10.21338/ess11e04_2", recode_missings = FALSE),
    "10.21338_ess11e04_2__raw.parquet"
  )
  expect_equal(
    ess_cache_key("10.21338/ess11e04_2", format = "csv"),
    "10.21338_ess11e04_2__recoded.csv"
  )

  # Same file, different request: three distinct cache entries.
  keys <- c(
    ess_cache_key("10.21338/ess11e04_2"),
    ess_cache_key("10.21338/ess11e04_2", recode_missings = FALSE),
    ess_cache_key("10.21338/ess11e04_2", format = "sav")
  )
  expect_length(unique(keys), 3L)
})

test_that("the cache key is normalised across DOI spellings", {
  expect_equal(
    ess_cache_key("https://doi.org/10.21338/ess11e04_2"),
    ess_cache_key("10.21338/ess11e04_2")
  )
})

test_that("the cache key is a safe file name", {
  key <- ess_cache_key("10.21338/some weird/suffix")
  expect_false(grepl("[/\\\\ ]", key))
})

test_that("the cache directory follows the option and can be switched off", {
  dir <- withr::local_tempdir()

  withr::with_options(list(essurvey2.cache_dir = dir), {
    expect_equal(ess_cache_dir(), dir)
    expect_match(ess_cache_path("10.21338/ess11e04_2"), "__recoded\\.parquet$")
  })

  withr::with_options(list(essurvey2.cache_dir = FALSE), {
    expect_null(ess_cache_dir())
    expect_null(ess_cache_path("10.21338/ess11e04_2"))
  })
})

test_that("ess_cache_dir(create = TRUE) makes the directory", {
  parent <- withr::local_tempdir()
  dir <- file.path(parent, "nested", "cache")

  withr::with_options(list(essurvey2.cache_dir = dir), {
    expect_false(dir.exists(dir))
    expect_equal(ess_cache_dir(create = TRUE), dir)
    expect_true(dir.exists(dir))
  })
})

test_that("ess_cache_list() is empty for a fresh cache and has a stable shape", {
  withr::with_options(list(essurvey2.cache_dir = withr::local_tempdir()), {
    out <- ess_cache_list()
    expect_s3_class(out, "data.table")
    expect_equal(nrow(out), 0L)
    expect_named(
      out,
      c("file", "format", "recoded", "size", "size_bytes", "modified", "path")
    )
  })
})

test_that("ess_cache_list() reports what is on disk", {
  dir <- withr::local_tempdir()

  withr::with_options(list(essurvey2.cache_dir = dir), {
    writeBin(raw(2048), file.path(dir, ess_cache_key("10.21338/ess11e04_2")))
    writeBin(raw(1024), file.path(dir, ess_cache_key("10.21338/ess6e02_7", format = "csv",
                                                     recode_missings = FALSE)))

    out <- ess_cache_list()
    expect_equal(nrow(out), 2L)
    # Largest first.
    expect_equal(out$size_bytes, c(2048, 1024))
    expect_equal(out$format, c("parquet", "csv"))
    expect_equal(out$recoded, c(TRUE, FALSE))
  })
})

test_that("ess_cache_clear() removes everything, or just one DOI", {
  dir <- withr::local_tempdir()

  withr::with_options(list(essurvey2.cache_dir = dir), {
    a <- file.path(dir, ess_cache_key("10.21338/ess11e04_2"))
    b <- file.path(dir, ess_cache_key("10.21338/ess6e02_7"))
    writeBin(raw(10), a)
    writeBin(raw(10), b)

    n <- ess_cache_clear("10.21338/ess11e04_2", confirm = FALSE)
    expect_equal(n, 1L)
    expect_false(file.exists(a))
    expect_true(file.exists(b))

    n <- ess_cache_clear(confirm = FALSE)
    expect_equal(n, 1L)
    expect_equal(nrow(ess_cache_list()), 0L)
  })
})

test_that("clearing an empty cache is a no-op, not an error", {
  withr::with_options(list(essurvey2.cache_dir = withr::local_tempdir()), {
    expect_equal(ess_cache_clear(confirm = FALSE), 0L)
  })
})
