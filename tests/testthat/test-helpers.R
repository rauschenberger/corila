# Unit tests -------------------------------------------------------------------
#' @srrstats {G5.5} *correctness tests are run with a fixed random seed*
#' @srrstats {G5.3} *absence of NA, NaN, -Inf, and Inf in return is tested*

## functions ".forescale" and ".backscale" -------------------------------------

n <- 5L
p <- 10L

set.seed(1)
sd <- seq(from = 0L, to = 1L, length.out = p)
x <- vapply(X = sd,
            FUN = function(x) stats::rnorm(n = n, sd = x),
            FUN.VALUE = numeric(n))
for (family in c("gaussian", "binomial", "poisson", "cox")) {
  # response
  if (identical(family, "gaussian")) {
    y <- stats::rnorm(n)
  } else if (identical(family, "binomial")) {
    y <- stats::rbinom(n = n, size = 1L, prob = 0.5)
  } else if (identical(family, "poisson")) {
    y <- stats::rpois(n = n, lambda = 4)
  } else if (identical(family, "cox")) {
    time_survival <- stats::rexp(n = n, rate = 1)
    time_censoring <- stats::rexp(n = n, rate = 1)
    time <- pmin(time_survival, time_censoring)
    event <- 1 * (time_survival <= time_censoring)
    y <- survival::Surv(time = time, event = event)
  }
  scale <- .forescale(x = x, y = y, family = family, pars = NULL)
  y_back <- .backscale(pars = scale$pars, y = scale$y)$y
  testthat::test_that("func '.backscale' returns finite n-vector y", {
    type <- ifelse(family %in% c("binomial", "poisson"), "integer", "double")
    testthat::expect_type(object = y_back, type = type)
    testthat::expect_length(object = y_back, n = n)
    testthat::expect_true(all(is.finite(y_back)))
  })
  testthat::test_that("func '.backscale' recovers original response", {
    testthat::expect_equal(object = y_back, expected = y) # !!!
  })
  testthat::test_that("func '.backscale' errors under wrong arg 'pars'", {
    testthat::expect_error(.backscale(pars = scale$pars[-1], y = scale$y))
  })
  # coefficients
  beta <- stats::rnorm(p + (family != "cox"))
  mu_x <- rep(x = 0, times = p)
  sd_x <- rep(x = 1, times = p)
  mu_y <- 0
  sd_y <- 1
  pars <- list(mu.x = mu_x, sd.x = sd_x,
               mu.y = mu_y, sd.y = sd_y,
               family = family)
  coef <- .backscale(pars = pars, coef = beta)$coef
  testthat::test_that("func '.backscale' returns finite n-vector coef", {
    testthat::expect_type(object = coef, type = "double")
    testthat::expect_length(object = coef, n = p + (family != "cox"))
    testthat::expect_true(all(is.finite(coef)))
  })
  testthat::test_that("func '.backscale' recovers original coefficients", {
    testthat::expect_equal(object = coef, expected = beta) # !!!
  })
  testthat::test_that(
    desc = paste("func '.forescale' errors unless",
                 "either arg 'family' or arg 'pars' is provided"),
    code = {
      testthat::expect_error(.forescale(x = x, y = y))
      testthat::expect_error(.forescale(x = x, y = y, family = family,
                                        pars = pars))
    }
  )
}

## function ".type" ------------------------------------------------------------

expect <- list("ridge" = 0,
               "lasso" = 1,
               "elastic" = 0.5,
               "none" = NA,
               "pearson" = "pearson",
               "spearman" = "spearman",
               "kendall" = "kendall",
               "multi-penalty" = "multiridge")
testthat::test_that("initial coefficients are named correctly", {
  for (i in seq_along(expect)) {
    string <- .type(alpha = expect[[i]])
    testthat::expect_type(object = string, type = "character")
    testthat::expect_length(object = string, n = 1)
    testthat::expect_false(is.na(string))
    split <- tolower(strsplit(x = string, split = " ", fixed = TRUE)[[1]])
    testthat::expect_contains(object = split, expected = names(expect)[i])
  }
  testthat::expect_error(.type(alpha = -0.1))
  testthat::expect_error(.type(alpha = 1.1))
  testthat::expect_error(.type(alpha = "blabla"))
})

## function ".deviance" --------------------------------------------------------

for (family in c("gaussian", "binomial", "poisson", "cox", "gamma")) {
  n <- 10L
  set.seed(1L)
  if (family == "gaussian") {
    y <- stats::rnorm(n = n)
    y_hat <- stats::rnorm(n = n)
  } else if (family == "binomial") {
    y <- stats::rbinom(n = n, size = 1L, prob = 0.5)
    y_hat <- stats::rbinom(n = n, size = 1, prob = 0.5)
  } else if (family == "poisson") {
    y <- stats::rpois(n = n, lambda = 4)
    y_hat <- stats::rpois(n = n, lambda = 4)
  } else if (family == "cox") {
    time_survival <- stats::rexp(n = n, rate = 1)
    time_censoring <- stats::rexp(n = n, rate = 1)
    time <- pmin(time_survival, time_censoring)
    event <- 1 * (time_survival <= time_censoring)
    y <- survival::Surv(time = time, event = event)
    y_hat <- stats::rexp(n = n, rate = 1)
  } else if (family == "gamma") {
    y <- stats::rgamma(n = n, shape = 0.5)
    y_hat <- stats::rgamma(n = n, shape = 0.5)
  }
  testthat::test_that("deviance is finite", {
    testthat::skip_if(family == "gamma")
    deviance <- .deviance(y = y, y_hat = y_hat, family = family)
    testthat::expect_type(object = deviance, type = "double")
    testthat::expect_true(all(is.finite(deviance)))
  })
  testthat::test_that("imperfect predictions lead to positive deviance", {
    testthat::skip_if(family == "gamma")
    deviance <- .deviance(y = y, y_hat = y_hat, family = family)
    testthat::expect_gt(object = deviance, expected = 0)
  })
  testthat::test_that("perfect predictions lead to deviance zero", {
    testthat::skip_if(family %in% c("cox", "gamma"))
    deviance <- .deviance(y = y, y_hat = y, family = family)
    testthat::expect_identical(object = deviance, expected = 0)
  })
  testthat::test_that("worse predictions increase cox deviance", {
    testthat::skip_if_not(family == "cox")
    # NB: inversion due to "higher risk = shorter time"
    dev_best <- .deviance(y = y,
                          y_hat = exp(-time_survival),
                          family = family)
    dev_random <- .deviance(y = y,
                            y_hat = sample(exp(-time_survival)),
                            family = family)
    dev_worst <- .deviance(y = y,
                           y_hat = exp(time_survival),
                           family = family)
    testthat::expect_gt(object = dev_random, expected = dev_best)
    testthat::expect_gt(object = dev_worst, expected = dev_random)
  })
  testthat::test_that("mean shift does not change cox deviance", {
    testthat::skip_if_not(family == "cox")
    dev0 <- .deviance(y = y, y_hat = exp(y_hat), family = family)
    dev1 <- .deviance(y = y, y_hat = exp(y_hat + stats::rnorm(1)),
                      family = family)
    testthat::expect_equal(object = dev1, expected = dev0)
  })
  testthat::test_that("gamma deviance is not implemented", {
    testthat::skip_if_not(family == "gamma")
    testthat::expect_error(.deviance(y = y, y_hat = y_hat, family = family))
  })
}

## function ".mean_function" ---------------------------------------------------

testthat::test_that("mean function works", {
  n <- 10L
  eta <- stats::rnorm(n = n)
  for (family in c("gaussian", "binomial", "poisson", "cox")) {
    mean <- .mean_function(x = eta, family = family)
    testthat::expect_type(object = mean, type = "double")
    testthat::expect_true(all(is.finite(mean)))
    testthat::expect_length(object = mean, n = n)
    cor <- stats::cor(mean, eta, method = "spearman")
    testthat::expect_equal(object = cor, expected = 1)
    if (family %in% c("binomial", "poisson")) {
      testthat::expect_gte(object = min(mean), expected = 0)
    }
    if (identical(family, "binomial")) {
      testthat::expect_lte(object = max(mean), expected = 1)
    }
  }
})

## function "calc_sign_prec" ---------------------------------------------------

set.seed(1L)
n <- 10L
truth <- sample(x = c(-1L, 0L, 1L), size = n, replace = TRUE)
estim <- sample(x = c(-1L, 0L, 1L), size = n, replace = TRUE)

testthat::test_that("precision is finite scalar", {
  precision <- calc_sign_prec(truth = truth, estim = estim)
  testthat::expect_type(object = precision, type = "double")
  testthat::expect_length(object = precision, n = 1)
  testthat::expect_true(all(is.finite(precision)))
})

testthat::test_that("precision equals zero if all signs are inverted", {
  prec <- calc_sign_prec(truth = truth, estim = -truth)
  testthat::expect_identical(object = prec, expected = 0L)
})

testthat::test_that("precision equals one if all signs are true", {
  prec <- calc_sign_prec(truth = truth, estim = truth)
  testthat::expect_identical(object = prec, expected = 1L)
})

testthat::test_that("precision is not defined if all signs equal zero", {
  prec <- calc_sign_prec(truth = truth, estim = 0 * estim)
  testthat::expect_identical(object = prec, expected = NA)
})

testthat::test_that("precision is not influenced by estimated zeros", {
  prec1 <- calc_sign_prec(truth = truth, estim = estim)
  prec2 <- calc_sign_prec(truth = truth[estim != 0L],
                          estim = estim[estim != 0L])
  testthat::expect_identical(object = prec1, expected = prec2)
})

testthat::test_that("precision equals zero if all true signs are zero", {
  prec <- calc_sign_prec(truth = rep(x = 0L, times = n), estim = estim)
  testthat::expect_identical(object = prec, expected = 0L)
})

testthat::test_that("error if different lengths", {
  truth <- sample(x = c(-1L, 0L, 1L), size = n, replace = TRUE)
  estim <- sample(x = c(-1L, 0L, 1L), size = n - 1L, replace = TRUE)
  testthat::expect_error(calc_sign_prec(truth = truth, estim = estim))
})

## function ".folds" -----------------------------------------------------------

set.seed(1L)
n <- stats::rpois(n = 1L, lambda = 50)
for (family in c("gaussian", "binomial", "poisson", "cox")) {
  if (family == "gaussian") {
    y <- stats::rnorm(n = n)
    index <- rep(x = 1L, times = n)
  } else if (family == "binomial") {
    y <- stats::rbinom(n = n, size = 1, prob = 0.2)
    index <- y
  } else if (family == "poisson") {
    y <- stats::rpois(n = n, lambda = 4)
    index <- rep(x = 1L, times = n)
  } else if (family == "cox") {
    time <- stats::rexp(n = n, rate = 2)
    status <- stats::rbinom(n = n, size = 1L, prob = 0.2)
    y <- survival::Surv(time = time, event = status)
    index <- y[, "status"]
  }
  testthat::test_that(
    desc = "function '.folds' throws an error if nfolds < 2 or nfolds > n",
    code = {
      for (nfolds in c(-1, 0, 1, n + 1, Inf)) {
        testthat::expect_error(.folds(y = y, family = family, nfolds = nfolds))
      }
    }
  )
  nfolds <- 10L
  testthat::test_that(
    desc = "function '.folds' throws an error for gamma family",
    code = {
      testthat::expect_error(.folds(y = y, family = "gamma", nfolds = nfolds))
    }
  )
  foldid <- .folds(y = y, family = family, nfolds = nfolds)
  testthat::test_that("fold identifiers are in finite vector", {
    testthat::expect_type(object = foldid, type = "integer")
    testthat::expect_length(object = foldid, n = n)
    testthat::expect_true(all(is.finite(foldid)))
    testthat::expect_setequal(object = foldid, expected = seq_len(nfolds))
  })
  diff <- tapply(X = foldid,
                 INDEX = index,
                 FUN = function(x) diff(range(table(x))))
  testthat::test_that("folds are stratified and balanced", {
    testthat::expect_true(all(diff <= 1L))
  })
}

for (i in c(0L, 1L)){
  n <- 10L
  nfolds <- 5L
  foldid <- .folds(y = rep(i, times = n), family = "binomial", nfolds = nfolds)
  testthat::expect_type(object = foldid, type = "integer")
  testthat::expect_length(object = foldid, n = n)
  testthat::expect_setequal(object = foldid, expected = seq_len(nfolds))
}

## function ".simulate_outcome" ------------------------------------------------

testthat::test_that("outcomes are simulated", {
  family <- c("gaussian", "binomial", "poisson", "cox")
  n <- 10L
  p <- 5L
  x <- matrix(data = stats::rnorm(n = n * p), nrow = n, ncol = p)
  beta <- stats::rnorm(n = p)
  for (i in seq_along(family)) {
    testthat::expect_error(
      .simulate_outcome(family = family[i])
    )
    testthat::expect_error(
      .simulate_outcome(family = family[i], x = x, beta = beta, n = n)
    )
    for (j in 1L:2L){
      if (j == 1L) {
        y <- .simulate_outcome(family = family[i], x = x, beta = beta)
      } else {
        y <- .simulate_outcome(family = family[i], n = n)
      }
      testthat::expect_length(object = y, n = n)
      testthat::expect_no_error(
        object = {
          .validate(na_action = NULL,
                    x = matrix(data = 0, nrow = n, ncol = p),
                    y = y,
                    group = rep(x = 1L, times = p),
                    primary = NULL,
                    family = family[i], hyper = NULL, alpha_init = NULL,
                    alpha_final = NULL, cor = NULL, foldid = NULL,
                    nfolds = NULL, lambda_init = NULL, silent = FALSE)
        }
      )
    }
  }
})

## function .residuals ---------------------------------------------------------

testthat::test_that("residuals match those from stats::residuals", {
  n <- 100L
  x <- rnorm(n)
  for (family in c("gaussian", "binomial", "poisson")) {
    if (family == "gaussian") {
      y <- stats::rnorm(n)
    } else if (family == "binomial") {
      y <- stats::rbinom(n = n, size = 1L, prob = 0.5)
    } else if (family == "poisson") {
      y <- stats::rpois(n = n, lambda = 4)
    } else {
      stop()
    }
    glm <- stats::glm(y ~ x, family = family)
    y_hat <- fitted(glm)
    resid <- .residuals(y_obs = y, y_fit = y_hat, family = family)
    names(resid) <- seq_len(n)
    testthat::expect_type(object = resid, type = "double")
    testthat::expect_length(object = resid, n = n)
    testthat::expect_true(all(is.finite(resid)))
    testthat::expect_equal(object = resid, expected = stats::residuals(glm)) # !
  }
})
