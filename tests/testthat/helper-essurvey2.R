# Tests must never depend on the machine's own configuration, so the user ID and
# cache location are neutralised unless a test opts back in.
local_clean_config <- function(env = parent.frame()) {
  withr::local_options(
    list(essurvey2.user_id = NULL, essurvey2.cache_dir = withr::local_tempdir(.local_envir = env)),
    .local_envir = env
  )
  withr::local_envvar(list(ESS_USER_ID = NA), .local_envir = env)
}

# A user ID that is well-formed but not anyone's.
fake_user_id <- "00000000-1111-2222-3333-444444444444"

# Live tests need network access and a real user ID. They are skipped rather
# than failed when either is absent, so the suite passes on CRAN and offline.
skip_if_no_api <- function() {
  testthat::skip_if_offline()
  testthat::skip_on_cran()
  if (!nzchar(Sys.getenv("ESS_USER_ID", unset = ""))) {
    testthat::skip("ESS_USER_ID is not set")
  }
}

# Catalogue tests need the network but no user ID.
skip_if_no_catalogue <- function() {
  testthat::skip_if_offline()
  testthat::skip_on_cran()
}
