
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
