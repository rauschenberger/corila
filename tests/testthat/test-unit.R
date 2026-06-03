
# Add unit tests for Cox model!

set.seed(1)

# Unit tests -------------------------------------------------------------------

## functions ".forescale" and ".backscale" -------------------------------------

n <- 5
p <- 10
testthat::test_that("", {
  x <- matrix(data = stats::rnorm(n * p), nrow = n, ncol = p)
  y <- stats::rnorm(n)
  scale <- .forescale(x = x, y = y, family = "gaussian", pars = NULL)
  y_back <- .backscale(pars = scale$pars, y = scale$y)
  testthat::expect_equal(object = y_back$y_original, expected = y)
  # CONTINUE HERE:
  # add tests for other families (i.e., forescale has no effect)
  # and for coefficients (i.e., no effect if x is already standardised).
})

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
    object <- tolower(strsplit(x = .type(alpha = expect[[i]]),
                               split = " ")[[1]])
    testthat::expect_contains(object = object,
                              expected = names(expect)[i])
  }
  testthat::expect_error(object = .type(alpha = -0.1))
  testthat::expect_error(object = .type(alpha = 1.1))
  testthat::expect_error(object = .type(alpha = "blabla"))
})

## function ".expand_auxiliary" ------------------------------------------------

n <- 5
p <- 10
x <- matrix(data = rnorm(n * p), nrow = n, ncol = p)
include <- as.logical(rbinom(n = p, size = 1, prob = 0.5))
x_primary <- x[, include]
x_expanded <- .expand_auxiliary(x = x_primary, include = include)
testthat::test_that("primary predictors are equal", {
  testthat::expect_identical(object = x_expanded[, include],
                             expected = x[, include])
})
testthat::test_that("auxiliary features are zero", {
  testthat::expect_setequal(object = x_expanded[, !include], expected = 0)
})

## function ".combine_slopes" --------------------------------------------------

alpha <- stats::rnorm(1)
temp <- stats::rnorm(10)
beta <- pmax(c(temp, -temp), 0)
coef <- .combine_slopes(alpha = alpha, beta = beta)
testthat::test_that("intercept does not change", {
  testthat::expect_identical(object = coef[1], expected = alpha)
})
testthat::test_that("slopes do not change", {
  testthat::expect_identical(object = coef[-1], expected = temp)
})

## function ".deviance" --------------------------------------------------------

for (family in c("gaussian", "binomial", "poisson", "cox", "gamma")) {
  n <- 10
  set.seed(1)
  if (family == "gaussian") {
    y <- stats::rnorm(n = n)
    y_hat <- stats::rnorm(n = n)
  } else if (family == "binomial") {
    y <- stats::rbinom(n = n, size = 1, prob = 0.5)
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
  } else if (family == "gamma") {
    y <- stats::rgamma(n = n, shape = 0.5)
    y_hat <- stats::rgamma(n = n, shape = 0.5)
  }
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
  testthat::test_that("worse predictions increase deviance", {
    testthat::skip_if(family != "cox")
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
  testthat::test_that("gamma deviance is not implemented", {
    testthat::skip_if(family != "gamma")
    testthat::expect_error(.deviance(y = y, y_hat = y_hat, family = family))
  })
}

## function ".set_candidates" --------------------------------------------------

for (tune in c("none", "trial", "wgt", "exp", "sep", "both", "all")) {
  hyper <- .set_candidates(tune = tune)
  testthat::test_that("candidate values", {
    labels <- c("wgt_local", "exp_local", "wgt_global", "exp_global")
    testthat::expect_equal(object = names(hyper), expected = labels)
    testthat::expect_gte(object = min(hyper), expected = 0)
    testthat::expect_identical(object = hyper, expected = unique(hyper))
    testthat::expect_identical(object = rownames(hyper),
                               expected = as.character(seq_len(nrow(hyper))))
  })
}

## function ".mean_function" ---------------------------------------------------

testthat::test_that("mean function works", {
  n <- 10
  eta <- stats::rnorm(n = n)
  for (family in c("gaussian", "binomial", "poisson", "cox")) {
    mean <- .mean_function(x = eta, family = family)
    testthat::expect_length(object = mean, n = n)
    cor <- stats::cor(mean, eta, method = "spearman")
    testthat::expect_equal(object = cor, expected = 1)
    if (family %in% c("binomial", "poisson")) {
      testthat::expect_gte(object = min(mean), expected = 0)
    }
    if(identical(family, "binomial")) {
      testthat::expect_lte(object = max(mean), expected = 1) 
    }
  }
})

## function "calc_sign_prec" ---------------------------------------------------

truth <- sample(x = c(-1, 0, 1), size = 10, replace = TRUE)
estim <- sample(x = c(-1, 0, 1), size = 10, replace = TRUE)

testthat::test_that("precision equals zero if all signs are inverted", {
  prec <- calc_sign_prec(truth = truth, estim = -truth)
  testthat::expect_identical(object = prec, expected = 0)
})

testthat::test_that("precision equals one if all signs are true", {
  prec <- calc_sign_prec(truth = truth, estim = truth)
  testthat::expect_identical(object = prec, expected = 1)
})

testthat::test_that("precision is not defined if all signs equal zero", {
  prec <- calc_sign_prec(truth = truth, estim = 0 * estim)
  testthat::expect_identical(object = prec, expected = NA)
})

testthat::test_that("precision is not influenced by estimated zeros", {
  prec1 <- calc_sign_prec(truth = truth, estim = estim)
  prec2 <- calc_sign_prec(truth = truth[estim != 0], estim = estim[estim != 0])
  testthat::expect_identical(object = prec1, expected = prec2)
})

## function ".folds" -----------------------------------------------------------

n <- stats::rpois(n = 1, lambda = 50)
for (family in c("gaussian", "binomial", "poisson", "cox")) {
  if (family == "gaussian") {
    y <- stats::rnorm(n = n)
    index <- rep(x = 1, times = n)
  } else if (family == "binomial") {
    y <- stats::rbinom(n = n, size = 1, prob = 0.2)
    index <- y
  } else if (family == "poisson") {
    y <- stats::rpois(n = n, lambda = 4)
    index <- rep(x = 1, times = n)
  } else if (family == "cox") {
    time <- stats::rexp(n = n, rate = 2)
    status <- stats::rbinom(n = n, prob = 0.2, size = 1)
    y <- survival::Surv(time = time, event = status)
    index <- y[, "status"]
  }
  foldid <- .folds(y = y, family = family, nfolds = 10)
  diff <- tapply(X = foldid,
                 INDEX = index,
                 FUN = function(x) diff(range(table(x))))
  testthat::test_that("folds are stratified and balanced", {
    testthat::expect_true(all(diff <= 1))
  })
}

## function ".is_adjacent" -----------------------------------------------------

testthat::test_that("adjacency is detected", {
  p <- 5
  names <- paste0("x", seq_len(p))
  group <- list()
  group$index_vector <- setNames(object = c(1, 1, 2, 2, 3), nm = names)
  group$label_vector <- setNames(object = LETTERS[group$index_vector],
                                 nm = names(group$index_vector))
  group$index_list <- lapply(X = setNames(nm = unique(group$label_vector)),
                             FUN = function(x) which(group$label_vector == x))
  group$label_list <- lapply(group$index_list, names)
  group$matrix <- 1 * outer(X = group$index_vector,
                            Y = group$index_vector,
                            FUN = "==")
  p <- length(group$index_vector)
  cond <- list()
  for (i in seq_along(group)) {
    cond[[i]] <- corila:::.is_adjacent(group = group[[i]],
                                       j = 1,
                                       p = p,
                                       names = names(group$index_vector))
  }
  lapply(X = cond[-1],
         FUN = testthat::expect_equal,
         expected = cond[[1]],
         check.attributes = FALSE)
})
