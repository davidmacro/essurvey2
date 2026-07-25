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

test_that("clearing one DOI does not take a file whose suffix extends it", {
  dir <- withr::local_tempdir()

  withr::with_options(list(essurvey2.cache_dir = dir), {
    short <- file.path(dir, ess_cache_key("10.21338/ess6e02_6"))
    long <- file.path(dir, ess_cache_key("10.21338/ess6e02_60"))
    writeBin(raw(10), short)
    writeBin(raw(10), long)

    n <- ess_cache_clear("10.21338/ess6e02_6", confirm = FALSE)
    expect_equal(n, 1L)
    expect_false(file.exists(short))
    expect_true(file.exists(long))
  })
})

test_that("every format and missing-value variant of one DOI is cleared together", {
  dir <- withr::local_tempdir()

  withr::with_options(list(essurvey2.cache_dir = dir), {
    for (key in c(
      ess_cache_key("10.21338/ess11e04_2"),
      ess_cache_key("10.21338/ess11e04_2", recode_missings = FALSE),
      ess_cache_key("10.21338/ess11e04_2", format = "csv")
    )) {
      writeBin(raw(10), file.path(dir, key))
    }

    expect_equal(ess_cache_clear("10.21338/ess11e04_2", confirm = FALSE), 3L)
  })
})

test_that("clearing an empty cache is a no-op, not an error", {
  withr::with_options(list(essurvey2.cache_dir = withr::local_tempdir()), {
    expect_equal(ess_cache_clear(confirm = FALSE), 0L)
  })
})


# Consent for the default location -------------------------------------------
#
# The default cache lives in the user's home filespace, which CRAN policy says
# a package must not write to unasked. These tests pin that down: reporting the
# location is always allowed, creating it is not.

# Point the default location at a throwaway directory, so nothing here can touch
# the real one, and clear any consent carried over from another test.
local_default_cache <- function(env = parent.frame()) {
  dir <- file.path(withr::local_tempdir(.local_envir = env), "home-cache")
  withr::local_options(
    list(essurvey2.cache_dir = NULL, essurvey2.cache_consent = NULL),
    .local_envir = env
  )
  withr::local_envvar(list(ESSURVEY2_CACHE_CONSENT = NA), .local_envir = env)
  rm(list = ls(ess_cache_state, all.names = TRUE), envir = ess_cache_state)
  local_mocked_bindings(ess_cache_default_dir = function() dir, .env = env)
  dir
}

test_that("asking where the cache is neither prompts nor creates it", {
  dir <- local_default_cache()

  expect_equal(ess_cache_dir(), dir)
  expect_false(dir.exists(dir))
})

test_that("without consent nothing is written to the default location", {
  dir <- local_default_cache()

  # Non-interactive, no preference expressed: the answer is no.
  expect_null(ess_cache_dir(create = TRUE))
  expect_false(dir.exists(dir))
})

test_that("the consent option grants consent without a prompt", {
  dir <- local_default_cache()
  withr::local_options(list(essurvey2.cache_consent = TRUE))

  expect_equal(ess_cache_dir(create = TRUE), dir)
  expect_true(dir.exists(dir))
  expect_true(file.exists(file.path(dir, ess_cache_marker)))
})

test_that("the consent environment variable is honoured too", {
  dir <- local_default_cache()
  withr::local_envvar(list(ESSURVEY2_CACHE_CONSENT = "true"))

  expect_equal(ess_cache_dir(create = TRUE), dir)
  expect_true(dir.exists(dir))
})

test_that("the consent option can also refuse outright", {
  dir <- local_default_cache()
  withr::local_options(list(essurvey2.cache_consent = FALSE))

  expect_null(ess_cache_dir(create = TRUE))
  expect_false(dir.exists(dir))
})

test_that("a recorded consent marker is enough on its own", {
  dir <- local_default_cache()
  dir.create(dir, recursive = TRUE)
  writeLines("agreed", file.path(dir, ess_cache_marker))

  expect_equal(ess_cache_dir(create = TRUE), dir)
})

test_that("the consent marker is not mistaken for a cached file", {
  dir <- withr::local_tempdir()

  withr::with_options(list(essurvey2.cache_dir = dir), {
    writeLines("agreed", file.path(dir, ess_cache_marker))
    writeBin(raw(2048), file.path(dir, ess_cache_key("10.21338/ess11e04_2")))

    expect_equal(nrow(ess_cache_list()), 1L)

    # Clearing the cache must not silently withdraw consent.
    expect_equal(ess_cache_clear(confirm = FALSE), 1L)
    expect_true(file.exists(file.path(dir, ess_cache_marker)))
  })
})


# Keeping the cache bounded ---------------------------------------------------

test_that("the size cap defaults to 1 GB and can be changed or switched off", {
  expect_equal(ess_cache_max_size(), 1024^3)

  withr::with_options(list(essurvey2.cache_max_size = 4096), {
    expect_equal(ess_cache_max_size(), 4096)
  })
  withr::with_options(list(essurvey2.cache_max_size = FALSE), {
    expect_true(is.infinite(ess_cache_max_size()))
  })
  withr::with_options(list(essurvey2.cache_max_size = -1), {
    expect_error(ess_cache_max_size(), "positive number of bytes")
  })
})

test_that("pruning drops the least recently modified files until it fits", {
  dir <- withr::local_tempdir()

  withr::with_options(
    list(essurvey2.cache_dir = dir, essurvey2.cache_max_size = 2500),
    {
      keys <- c(
        old = ess_cache_key("10.21338/ess6e02_7"),
        mid = ess_cache_key("10.21338/ess9e03_2"),
        new = ess_cache_key("10.21338/ess11e04_2")
      )
      for (key in keys) {
        writeBin(raw(1000), file.path(dir, key))
      }

      # Stamp distinct ages, oldest first, so eviction order is unambiguous.
      now <- Sys.time()
      Sys.setFileTime(file.path(dir, keys[["old"]]), now - 3600)
      Sys.setFileTime(file.path(dir, keys[["mid"]]), now - 1800)
      Sys.setFileTime(file.path(dir, keys[["new"]]), now)

      # 3000 bytes against a 2500-byte cap: exactly one file has to go.
      expect_equal(ess_cache_prune(dir, quiet = TRUE), 1L)
      expect_false(file.exists(file.path(dir, keys[["old"]])))
      expect_true(file.exists(file.path(dir, keys[["mid"]])))
      expect_true(file.exists(file.path(dir, keys[["new"]])))
    }
  )
})

test_that("pruning leaves a cache that already fits alone", {
  dir <- withr::local_tempdir()

  withr::with_options(
    list(essurvey2.cache_dir = dir, essurvey2.cache_max_size = 1024^3),
    {
      writeBin(raw(1000), file.path(dir, ess_cache_key("10.21338/ess11e04_2")))
      expect_equal(ess_cache_prune(dir, quiet = TRUE), 0L)
      expect_equal(nrow(ess_cache_list()), 1L)
    }
  )
})

test_that("pruning is a no-op when there is no cache or no cap", {
  expect_equal(ess_cache_prune(NULL, quiet = TRUE), 0L)

  dir <- withr::local_tempdir()
  withr::with_options(
    list(essurvey2.cache_dir = dir, essurvey2.cache_max_size = FALSE),
    {
      writeBin(raw(4096), file.path(dir, ess_cache_key("10.21338/ess11e04_2")))
      expect_equal(ess_cache_prune(dir, quiet = TRUE), 0L)
      expect_equal(nrow(ess_cache_list()), 1L)
    }
  )
})
