test_that("endpoints default to the live services and drop trailing slashes", {
  expect_equal(ess_api_url(), "https://api.ess.sikt.no")
  expect_equal(ess_gql_url(), "https://api.nsd.no/graphql")

  withr::with_options(list(essurvey2.api_url = "https://api.ess.stage.sikt.no/"), {
    expect_equal(ess_api_url(), "https://api.ess.stage.sikt.no")
  })
})

test_that("ess_user_id() prefers the option over the environment variable", {
  local_clean_config()

  withr::with_envvar(list(ESS_USER_ID = "aaaaaaaa-1111-2222-3333-444444444444"), {
    expect_equal(ess_user_id(), "aaaaaaaa-1111-2222-3333-444444444444")

    withr::with_options(list(essurvey2.user_id = fake_user_id), {
      expect_equal(ess_user_id(), fake_user_id)
    })
  })
})

test_that("an unset user ID errors, or returns NULL when asked not to", {
  local_clean_config()

  expect_null(ess_user_id(error = FALSE))
  expect_error(ess_user_id(), class = "essurvey2_error_user_id")
  expect_error(ess_user_id(), "no ESS API user ID is configured", ignore.case = TRUE)
})

test_that("whitespace and empty environment values count as unset", {
  local_clean_config()

  withr::with_envvar(list(ESS_USER_ID = "   "), {
    expect_null(ess_user_id(error = FALSE))
  })
})

test_that("a surrounding-whitespace user ID is trimmed rather than rejected", {
  local_clean_config()

  withr::with_envvar(list(ESS_USER_ID = paste0("  ", fake_user_id, "  ")), {
    expect_equal(ess_user_id(), fake_user_id)
  })
})

test_that("ess_set_user_id() validates and returns the previous value", {
  local_clean_config()

  old <- ess_set_user_id(fake_user_id)
  expect_null(old)
  expect_equal(ess_user_id(), fake_user_id)

  previous <- ess_set_user_id("11111111-2222-3333-4444-555555555555")
  expect_equal(previous, fake_user_id)

  # Clearing it puts us back to unconfigured.
  ess_set_user_id(NULL)
  expect_null(ess_user_id(error = FALSE))
})

test_that("obviously-wrong user IDs are rejected with an actionable error", {
  local_clean_config()

  expect_error(ess_set_user_id("nope@example.com"), class = "essurvey2_error_user_id")
  expect_error(ess_set_user_id("nope@example.com"), "hexadecimal")
  expect_error(ess_set_user_id("abc"), "too short")
  expect_error(ess_set_user_id(""), "non-empty")
  expect_error(ess_set_user_id(c("a", "b")), "single non-empty string")
})

test_that("a non-canonical but plausible user ID is accepted with a warning", {
  local_clean_config()

  # The API's own documentation uses this shape, which is not a valid UUID, so
  # rejecting it outright would lock out legitimate IDs.
  expect_warning(
    ess_set_user_id("12345678-1234-1234-1234-12345678900"),
    "does not look like a UUID"
  )
  expect_equal(ess_user_id(), "12345678-1234-1234-1234-12345678900")
})

test_that("ess_config() reports the source", {
  local_clean_config()

  withr::with_envvar(list(ESS_USER_ID = fake_user_id), {
    cfg <- expect_invisible(ess_config())
    expect_equal(cfg$user_id, fake_user_id)
    expect_equal(cfg$user_id_source, "ESS_USER_ID")
    expect_equal(cfg$api_url, "https://api.ess.sikt.no")
  })
})

test_that("the printed user ID is masked past its first block", {
  # ess_config() prints this, so that its output can be pasted into a bug report.
  mask <- function(x) essurvey2:::ess_mask_id(x)

  expect_equal(mask(fake_user_id), "00000000****************************")
  expect_equal(nchar(mask(fake_user_id)), nchar(fake_user_id))
  expect_false(grepl(fake_user_id, mask(fake_user_id), fixed = TRUE))

  # A short value is hidden entirely rather than half-revealed.
  expect_equal(mask("abc"), "***")
})

test_that("ess_config() says so when nothing is configured", {
  local_clean_config()

  cfg <- ess_config()
  expect_null(cfg$user_id)
  expect_true(is.na(cfg$user_id_source))
})
