
data(data, package = "corila")

slots <- c("x_train", "y_train", "group", "primary", "beta", "x_test", "y_test")

testthat::test_that("list data contains expected slots", {
  testthat::expect_named(object = data, expected = slots)
})

testthat::test_that("slots have expected types", {
  testthat::expect_type(object = data$x_train, type = "double")
  testthat::expect_type(object = data$y_train, type = "double")
  testthat::expect_type(object = data$group, type = "integer")
  testthat::expect_type(object = data$primary, type = "logical")
  testthat::expect_type(object = data$beta, type = "double")
  testthat::expect_type(object = data$x_test, type = "double")
  testthat::expect_type(object = data$x_test, type = "double")
})

testthat::test_that("data have same number of rows (observations)", {
  testthat::expect_identical(object = nrow(data$x_train),
                             expected = length(data$y_train))
  testthat::expect_identical(object = nrow(data$x_test),
                             expected = length(data$y_test))
})

testthat::test_that("data have same number of columns (predictors)", {
  testthat::expect_identical(object = ncol(data$x_test),
                             expected = ncol(data$x_train))
  testthat::expect_length(object = data$primary,
                          n = ncol(data$x_train))
  testthat::expect_length(object = data$group,
                          n = ncol(data$x_train))
})
