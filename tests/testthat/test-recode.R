# Recoding is driven by a code table, which is injected here so these tests do
# not need the catalogue. The live path is covered in test-live.R.

test_that("ess_recode_missings() insists on a data.table", {
  expect_error(ess_recode_missings(data.frame(a = 1)), "must be a <data.table>")
  expect_error(ess_recode_missings(data.frame(a = 1)), "setDT")
})

test_that("the round must be knowable", {
  dt <- data.table::data.table(agea = c(45L, 999L))
  expect_error(ess_recode_missings(dt), "Cannot tell which ESS round")

  mixed <- data.table::data.table(essround = c(10L, 11L), agea = c(45L, 999L))
  expect_error(ess_recode_missings(mixed), "2 ESS rounds")
  expect_error(ess_recode_missings(mixed), "separately")
})

test_that("ess_as_factor() applies the same round checks", {
  dt <- data.table::data.table(gndr = 1:2)
  expect_error(ess_as_factor(dt), "Cannot tell which ESS round")

  mixed <- data.table::data.table(essround = c(10L, 11L), gndr = 1:2)
  expect_error(ess_as_factor(mixed), "2 ESS rounds")
})

test_that("a code table with no missing codes leaves the data alone", {
  local_mocked_bindings(
    ess_missing_codes = function(...) {
      data.table::data.table(
        variable_name = character(), value = character(), label = character()
      )
    }
  )

  dt <- data.table::data.table(essround = 11L, agea = 999L)
  expect_message(ess_recode_missings(dt), "No missing codes")
  expect_equal(dt$agea, 999L)
})

test_that("missing codes are applied per variable, by reference", {
  local_mocked_bindings(
    ess_missing_codes = function(...) {
      data.table::data.table(
        variable_name = c("agea", "ppltrst", "ppltrst", "ppltrst", "gndr"),
        value = c("999", "77", "88", "99", "9"),
        label = c("Not available", "Refusal", "Don't know", "No answer", "No answer")
      )
    }
  )

  dt <- data.table::data.table(
    essround = rep(11L, 4L),
    agea = c(45L, 999L, 31L, 88L),
    ppltrst = c(7L, 88L, 999L, 3L),
    gndr = c(1L, 2L, 9L, 1L)
  )

  out <- ess_recode_missings(dt, quiet = TRUE)

  # Returned invisibly, and the same object as was passed in.
  expect_identical(out, dt)

  # agea: only 999 is missing, so the 88 stays -- it is a real age code there.
  expect_equal(dt$agea, c(45L, NA, 31L, 88L))
  # ppltrst: 88 is missing but 999 is not one of its codes, so it survives.
  expect_equal(dt$ppltrst, c(7L, NA, 999L, 3L))
  expect_equal(dt$gndr, c(1L, 2L, NA, 1L))
})

test_that("recoding can be restricted to some columns", {
  local_mocked_bindings(
    ess_missing_codes = function(round, variables = NULL, ...) {
      all <- data.table::data.table(
        variable_name = c("agea", "ppltrst"),
        value = c("999", "88"),
        label = c("Not available", "Don't know")
      )
      if (is.null(variables)) all else all[all$variable_name %in% variables]
    }
  )

  dt <- data.table::data.table(essround = 11L, agea = 999L, ppltrst = 88L)
  ess_recode_missings(dt, variables = "agea", quiet = TRUE)

  expect_true(is.na(dt$agea))
  expect_equal(dt$ppltrst, 88L)
})

test_that("string columns are matched as strings", {
  local_mocked_bindings(
    ess_missing_codes = function(...) {
      data.table::data.table(
        variable_name = "region", value = "XX", label = "Not applicable"
      )
    }
  )

  dt <- data.table::data.table(essround = 11L, region = c("NL01", "XX"))
  ess_recode_missings(dt, quiet = TRUE)
  expect_equal(dt$region, c("NL01", NA))
})

test_that("an explicit round overrides the essround column", {
  seen <- NULL
  local_mocked_bindings(
    ess_missing_codes = function(round, ...) {
      seen <<- round
      data.table::data.table(
        variable_name = "agea", value = "999", label = "Not available"
      )
    }
  )

  dt <- data.table::data.table(essround = c(10L, 11L), agea = c(999L, 45L))
  ess_recode_missings(dt, round = 11, quiet = TRUE)
  expect_equal(seen, 11)
  expect_equal(dt$agea, c(NA, 45L))
})

test_that("ess_as_factor() labels codes and drops the missing ones", {
  local_mocked_bindings(
    ess_value_labels = function(...) {
      data.table::data.table(
        variable_name = c("gndr", "gndr", "gndr"),
        value = c("1", "2", "9"),
        label = c("Male", "Female", "No answer"),
        is_missing = c(FALSE, FALSE, TRUE)
      )
    }
  )

  dt <- data.table::data.table(essround = rep(11L, 3L), gndr = c(1L, 2L, 9L))
  ess_as_factor(dt, quiet = TRUE)

  expect_s3_class(dt$gndr, "factor")
  expect_equal(levels(dt$gndr), c("Male", "Female"))
  expect_equal(as.character(dt$gndr), c("Male", "Female", NA))
})

test_that("ess_as_factor(keep_missing = TRUE) keeps them as levels", {
  local_mocked_bindings(
    ess_value_labels = function(...) {
      data.table::data.table(
        variable_name = c("gndr", "gndr", "gndr"),
        value = c("1", "2", "9"),
        label = c("Male", "Female", "No answer"),
        is_missing = c(FALSE, FALSE, TRUE)
      )
    }
  )

  dt <- data.table::data.table(essround = rep(11L, 3L), gndr = c(1L, 2L, 9L))
  ess_as_factor(dt, keep_missing = TRUE, quiet = TRUE)

  expect_equal(levels(dt$gndr), c("Male", "Female", "No answer"))
  expect_equal(as.character(dt$gndr), c("Male", "Female", "No answer"))
})

test_that("ess_as_factor() is idempotent: a second call does not blank the column", {
  # The conversion is by reference, so a re-run of the same script hits an
  # already-converted column. Re-levelling labels against codes would match
  # nothing and silently replace every value with NA.
  local_mocked_bindings(
    ess_value_labels = function(...) {
      data.table::data.table(
        variable_name = c("gndr", "gndr"),
        value = c("1", "2"),
        label = c("Male", "Female"),
        is_missing = c(FALSE, FALSE)
      )
    }
  )

  dt <- data.table::data.table(essround = rep(11L, 2L), gndr = c(1L, 2L))
  ess_as_factor(dt, quiet = TRUE)
  expect_equal(as.character(dt$gndr), c("Male", "Female"))

  expect_message(ess_as_factor(dt), "already")
  expect_equal(as.character(dt$gndr), c("Male", "Female"))
  expect_equal(levels(dt$gndr), c("Male", "Female"))
})

test_that("ess_as_factor() leaves a column whose codes collide numerically alone", {
  # Two codes that parse to the same number would make factor() drop every value
  # matching the second one.
  local_mocked_bindings(
    ess_value_labels = function(...) {
      data.table::data.table(
        variable_name = c("odd", "odd"),
        value = c("1", "1.0"),
        label = c("One", "Also one"),
        is_missing = c(FALSE, FALSE)
      )
    }
  )

  dt <- data.table::data.table(essround = rep(11L, 2L), odd = c(1L, 2L))
  expect_message(ess_as_factor(dt), "unchanged")
  expect_false(is.factor(dt$odd))
  expect_equal(dt$odd, c(1L, 2L))
})

test_that("ess_as_factor() leaves columns with too many codes alone", {
  local_mocked_bindings(
    ess_value_labels = function(...) {
      data.table::data.table(
        variable_name = rep("many", 10L),
        value = as.character(1:10),
        label = paste0("level", 1:10),
        is_missing = rep(FALSE, 10L)
      )
    }
  )

  dt <- data.table::data.table(essround = rep(11L, 3L), many = c(1L, 5L, 9L))
  ess_as_factor(dt, max_levels = 5L, quiet = TRUE)

  expect_false(is.factor(dt$many))
  expect_equal(dt$many, c(1L, 5L, 9L))
})
