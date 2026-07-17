
testthat::test_that("help file for corila-package exists", {
  info <- ?corila::`corila-package`
  #testthat::expect_type(object = info, type = "character")
  testthat::expect_gt(object = length(info), expected = 0L)
})
