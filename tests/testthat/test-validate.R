
testthat::test_that("function .validate_family returns argument if valid", {
  x <- c("gaussian", "binomial", "poisson", "cox")
  for (i in seq_along(x)) {
    y <- .validate_family(family = x[i])
    testthat::expect_identical(object = y, expected = x[i])
  }
  testthat::expect_error(object = .validate_family(family = "gamma"),
                         regexp = "Must be element of set")
})

testthat::test_that("function .na_action returns argument if valid", {
  x <- c("error", "complete_cases")
  for (i in seq_along(x)) {
    y <- .validate_na_action(na_action = x[i])
    testthat::expect_identical(object = y, expected = x[i])
  }
  testthat::expect_error(object = .validate_na_action(na_action = "warning"),
                         regexp = "Must be element of set")
})


".validate_primary"
".validate_alpha"
".validate_cor"
".validate_foldid"
".validate_group"
".validate_hyper"
".validate_x"
".validate_y"
".validate_y_hat"
