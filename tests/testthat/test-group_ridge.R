
## function "multiridge" -------------------------------------------------------

for (family in c("gaussian", "binomial", "cox")) {
  # simulate
  set.seed(1)
  n0 <- 100
  n1 <- 10000
  n <- n0 + n1
  p <- c(100, 50)
  z <- rep(x = seq_along(p), times = p)
  x <- vapply(X = z,
              FUN = function(x) stats::rnorm(n = n, sd = x),
              FUN.VALUE = numeric(length = n))
  beta <- stats::rnorm(n = sum(p), mean = 1, sd = 0) *
    stats::rbinom(n = sum(p), size = 1L, prob = 0.2)
  eta <- as.numeric(x %*% beta)
  if (family == "gaussian") {
    y <- eta + 0.5 * stats::rnorm(n = n, sd = stats::sd(eta))
  } else if (family == "binomial") {
    y <- stats::rbinom(n = n, size = 1L, prob = 1 / (1 + exp(-eta)))
  } else if (family == "cox") {
    time <- stats::rexp(n = n, rate = exp(eta))
    status <- stats::rbinom(n = n, size = 1L, prob = 0.5)
    y <- survival::Surv(time = time, event = status)
  }
  cond <- rep(x = c(TRUE, FALSE), times = c(n0, n1))
  # equality
  object <- multiridge(x = x[cond, ], y = y[cond], z = z, family = family)
  y_hat <- stats::predict(object, newx = x[!cond, ])
  if (family == "cox") {
    temp <- exp(x[!cond, ] %*% stats::coef(object))
  } else {
    temp <- .mean_function(
      x = drop(coef(object)[1L] + x[!cond, ] %*% coef(object)[-1]),
      family = family
    )
  }
  testthat::test_that("multiridge predict can be reconstructed with coef", {
    if (family == "cox") {
      testthat::expect_equal(object = temp,
                             expected = y_hat * mean(temp / y_hat),
                             check.attributes = FALSE)
    } else {
      testthat::expect_equal(object = temp, expected = y_hat)
    }
  })
  testthat::test_that("refit with penalties is identical", {
    refit <- multiridge(x = x[cond, ], y = y[cond], z = z,
                        family = family, penalties = object$penalties)
    object$indices <- NULL
    testthat::expect_identical(object = refit, expected = object)
  })
  testthat::test_that("multiridge-fit rejects wrong matrices", {
    testthat::expect_error(
      multiridge(x = x[cond, ], y = y[cond], z = z[-1], family = family)
    )
    testthat::expect_error(
      multiridge(x = x[cond, ], y = y[cond], z = z, family = family,
                 penalties = rep(x = 1, times = length(p) + 1L))
    )
  })
  testthat::test_that("multiridge-predict rejects wrong matrices", {
    testthat::expect_error(
      stats::predict(object, newx = cbind(x[!cond, ], x[!cond, ]))
    )
    testthat::expect_error(
      stats::predict(object, newx = x[!cond, -ncol(x)])
    )
  })
  testthat::test_that("refit with given folds is identical", {
    foldid <- rep(x = NA, times = sum(cond))
    for (i in seq_along(object$indices)) {
      foldid[object$indices[[i]]] <- i
    }
    refit <- multiridge(x = x[cond, ], y = y[cond], z = z,
                        family = family, foldid = foldid)
    testthat::expect_identical(object = refit, expected = object)
  })
}
