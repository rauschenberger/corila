
family <- c("gaussian", "binomial", "poisson", "cox")
n <- 10
p <- 5
for (i in seq_along(family)) {
  for (j in 1:2) {
    if (j == 1) {
      y <- .simulate_outcome(n = n, family = family[i])
    } else {
      x <- matrix(data = stats::rnorm(n = n * p), nrow = n, ncol = p)
      beta <- stats::rnorm(n = p)
      y <- .simulate_outcome(x = x, beta = beta, family = family[i])
    }
    testthat::test_that("function throws expected errors", {
      testthat::expect_error(object = .simulate_outcome(),
                             regexp = "is missing")
      testthat::expect_error(object = .simulate_outcome(family = "gamma"),
                             regexp = "inside support")
      testthat::expect_error(object = .simulate_outcome(family = family[i]),
                             regexp = "Provide either")
      testthat::skip_if(j == 1)
      testthat::expect_error(object = .simulate_outcome(x = x,
                                                        beta = beta[-1],
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
