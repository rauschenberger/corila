
testthat::test_that("help file for corila-package exists", {
  info <- ?corila::`corila-package`
  testthat::expect_type(object = info, type = "list")
  testthat::expect_true(length(info) > 0)
})
