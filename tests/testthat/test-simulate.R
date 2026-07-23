
#----- function .simulate_predictors -------------------------------------------

testthat::test_that(".simulate_predictors requires either p or group", {
  testthat::expect_error(
    object = .simulate_predictors(n = 2L, p = NULL, group = NULL),
    regexp = "either p or group"
  )
  testthat::expect_error(
    object = .simulate_predictors(n = 2L, p = 2L, group = c(1L, 2L)),
    regexp = "either p or group"
  )
}
)

testthat::test_that(".simulate_predictors handles single sample", {
  x <- .simulate_predictors(n = 1L, group = c(1L, 1L), rho = 1.0)
  testthat::expect_type(object = x, type = "double")
  testthat::expect_equal(object = x[1L], expected = x[2L])
  testthat::expect_shape(object = x, dim = c(1L, 2L))
}
)

testthat::test_that(".simulate_predictors handles multiple samples", {
  x <- .simulate_predictors(n = 10L, group = c(1L, 1L, 2L), rho = 1.0)
  testthat::expect_type(object = x, type = "double")
  testthat::expect_equal(object = x[, 1L], expected = x[, 2L],
                         tolerance = 1e-06)
  testthat::expect_shape(object = x, dim = c(10L, 3L))
}
)

testthat::test_that(".simulate_predictors works with p and group", {
  x0 <- .simulate_predictors(n = 10L, p = 5L)
  x1 <- .simulate_predictors(n = 10L, group = seq_len(5L))
  testthat::expect_equal(object = x0, expected = x1)
}
)

#----- function .simulate_response ---------------------------------------------

family <- c("gaussian", "binomial", "poisson", "cox")
n <- 10L
p <- 5L
for (i in seq_along(family)) {
  for (j in 1L:2L) {
    if (j == 1L) {
      y <- .simulate_response(n = n, family = family[i])
    } else {
      x <- matrix(data = stats::rnorm(n = n * p), nrow = n, ncol = p)
      beta <- stats::rnorm(n = p)
      y <- .simulate_response(x = x, beta = beta, family = family[i])
    }
    testthat::test_that("function throws expected errors", {
      testthat::expect_error(object = .simulate_response(),
                             regexp = "is missing")
      testthat::expect_error(object = .simulate_response(family = "gamma"),
                             regexp = "inside support")
      testthat::expect_error(object = .simulate_response(family = family[i]),
                             regexp = "Provide either")
      testthat::skip_if(j == 1L)
      testthat::expect_error(object = .simulate_response(x = x,
                                                         beta = beta[-1L],
                                                         family = family[i]),
                             regexp = "other length")
      testthat::expect_error(
        object = .simulate_response(x = NULL, beta = beta, family = family[i]),
        regexp = "either none or both"
      )
      testthat::expect_error(
        object = .simulate_response(x = x, beta = NULL, family = family[i]),
        regexp = "either none or both"
      )
    })
    testthat::test_that("simulated outcome is numeric", {
      testthat::skip_if_not(family[i] %in% c("gaussian", "cox"))
      testthat::expect_type(object = y, type = "double")
    })
    testthat::test_that("simulated outcome is integer", {
      testthat::skip_if_not(family[i] %in% c("poisson", "binomial"))
      testthat::expect_type(object = y, type = "integer")
    })
    testthat::test_that("simulated outcome has expected length", {
      testthat::expect_length(object = y, n = n)
    })
  }
}

#----- function simulate_data --------------------------------------------------

data <- simulate_data()

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

testthat::test_that("data have consistent row names", {
  testthat::expect_named(object = data$y_train,
                         expected = rownames(data$x_train))
  testthat::expect_named(object = data$y_test,
                         expected = rownames(data$x_test))
})

testthat::test_that("data have consistent column names", {
  colnames <- colnames(data$x_train)
  testthat::expect_named(object = data$group,
                         expected = colnames)
  testthat::expect_named(object = data$primary,
                         expected = colnames)
  testthat::expect_identical(object = colnames(data$x_test),
                             expected = colnames)
  testthat::expect_named(object = data$beta,
                         expected = colnames)
})
