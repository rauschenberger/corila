
# Unit tests -------------------------------------------------------------------

## functions ".forescale" and ".backscale" -------------------------------------

n <- 5
p <- 10

set.seed(1)
sd <- seq(from = 0, to = 1, length.out = p)
x <- vapply(X = sd,
            FUN = function(x) stats::rnorm(n = n, sd = x),
            FUN.VALUE = numeric(n))
for (family in c("gaussian", "binomial", "poisson", "cox")) {
  # response
  if (identical(family, "gaussian")) {
    y <- stats::rnorm(n)
  } else if (identical(family, "binomial")) {
    y <- stats::rbinom(n = n, size = 1, prob = 0.5)
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
    testthat::expect_length(object = y_back, n= n)
    testthat::expect_true(all(is.finite(y_back)))
  })
  testthat::test_that("func '.backscale' recovers original response", {
    testthat::expect_equal(object = y_back, expected = y) # !!!
  })
  testthat::test_that("func '.backscale' errors under wrong arg 'pars'", {
    testthat::expect_error(.backscale(pars = scale$pars[-1], y = scale$y))
  })
  # coefficients
  beta <- stats::rnorm(p + (family!="cox"))
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
    testthat::expect_length(object = coef, n = p + (family!="cox"))
    testthat::expect_true(all(is.finite(coef)))
  })
  testthat::test_that("func '.backscale' recovers original coefficients", {
    testthat::expect_equal(object = coef, expected = beta) # !!!
  })
  testthat::test_that(paste("func '.forescale' errors unless",
                            "either arg 'family' or arg 'pars' is provided"), {
    testthat::expect_error(.forescale(x = x, y = y))
    testthat::expect_error(.forescale(x = x, y = y, family = family,
                                      pars = pars))
  })
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
    testthat::expect_true(!is.na(string))
    split <- tolower(strsplit(x = string, split = " ")[[1]])
    testthat::expect_contains(object = split, expected = names(expect)[i]) # !!!
  }
  testthat::expect_error(object = .type(alpha = -0.1))
  testthat::expect_error(object = .type(alpha = 1.1))
  testthat::expect_error(object = .type(alpha = "blabla"))
})

## function ".expand_auxiliary" ------------------------------------------------

n <- as.integer(5)
p <- as.integer(10)
set.seed(1)
x <- matrix(data = stats::rnorm(n * p), nrow = n, ncol = p)
primary <- as.logical(stats::rbinom(n = p, size = 1, prob = 0.5))
x_primary <- x[, primary]
testthat::test_that("incompatible dimensions are rejected", {
  testthat::expect_error(
    .expand_auxiliary(x = x_primary, primary = primary[-1])
  )
  testthat::expect_error(
    .expand_auxiliary(x = x_primary, primary = c(primary, TRUE))
  )
})
testthat::test_that("nothing happens if there are primary features only", {
  x_expanded <- .expand_auxiliary(x = x, primary = rep(TRUE, times = p))
  testthat::expect_identical(object = x_expanded, expected = x)
})
x_expanded <- .expand_auxiliary(x = x_primary, primary = primary)
testthat::test_that("primary predictors are equal", {
  testthat::expect_identical(object = x_expanded[, primary],
                             expected = x[, primary])
})
testthat::test_that("auxiliary features are zero", {
  testthat::expect_setequal(object = x_expanded[, !primary], expected = 0)
})
testthat::test_that("expanded features are in a finite n x p matrix", {
  testthat::expect_type(object = x_expanded, type = "double")
  testthat::expect_length(object = x_expanded, n = n * p)
  testthat::expect_identical(object = nrow(x_expanded), expected = n)
  testthat::expect_identical(object = ncol(x_expanded), expected = p)
  testthat::expect_true(all(is.finite(x_expanded)))
})

## function ".combine_slopes" --------------------------------------------------

set.seed(1)
p <- 10
alpha <- stats::rnorm(1)
temp <- stats::rnorm(p)
beta <- pmax(c(temp, -temp), 0)
coef <- .combine_slopes(alpha = alpha, beta = beta)
testthat::test_that("coefficients are in finite p + 1 vector", {
  testthat::expect_type(object = coef, type = "double")
  testthat::expect_length(object = coef, n = p + 1)
  testthat::expect_true(all(is.finite(coef)))
})
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
  testthat::test_that("mean shift does not change cox deviance", {
    testthat::skip_if(family != "cox")
    dev0 <- .deviance(y = y, y_hat = exp(y_hat), family = family)
    dev1 <- .deviance(y = y, y_hat = exp(y_hat + stats::rnorm(1)),
                      family = family)
    testthat::expect_equal(object = dev1, expected = dev0)
  })
  testthat::test_that("gamma deviance is not implemented", {
    testthat::skip_if(family != "gamma")
    testthat::expect_error(.deviance(y = y, y_hat = y_hat, family = family))
  })
}

## function ".set_candidates" --------------------------------------------------

for (tune in c("none", "weight", "exponent", "bivariate", "factorial")) {
  hyper <- .set_candidates(tune = tune)
  testthat::test_that("candidate values", {
    labels <- c("wgt_local", "exp_local", "wgt_global", "exp_global")
    testthat::expect_type(object = hyper, type = "list")
    testthat::expect_equal(object = names(hyper), expected = labels)
    testthat::expect_gte(object = min(hyper), expected = 0)
    testthat::expect_identical(object = hyper, expected = unique(hyper))
    testthat::expect_identical(object = rownames(hyper),
                               expected = as.character(seq_len(nrow(hyper))))
    testthat::expect_error(.set_candidates(tune = "random"))
  })
}

## function ".mean_function" ---------------------------------------------------

testthat::test_that("mean function works", {
  n <- 10
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

set.seed(1)
truth <- sample(x = c(-1, 0, 1), size = 10, replace = TRUE)
estim <- sample(x = c(-1, 0, 1), size = 10, replace = TRUE)

testthat::test_that("precision is finite scalar", {
  precision <- calc_sign_prec(truth = truth, estim = estim)
  testthat::expect_type(object = precision, type = "double")
  testthat::expect_length(object = precision, n = 1)
  testthat::expect_true(all(is.finite(precision)))
})

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

set.seed(1)
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
  testthat::test_that("fold identifiers are in finite vector", {
    testthat::expect_type(object = foldid, type = "integer")
    testthat::expect_length(object = foldid, n = n)
    testthat::expect_true(all(is.finite(foldid)))
  })
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
    cond[[i]] <- .is_adjacent(group = group[[i]],
                              j = 1,
                              p = p,
                              names = names(group$index_vector))
  }
  lapply(X = cond[-1],
         FUN = testthat::expect_equal,
         expected = cond[[1]],
         check.attributes = FALSE)
  factor_vector <- as.factor(group$label_vector)
  testthat::expect_error(
    .is_adjacent(group = factor_vector, j = 1, p = p, names = NULL)
  )
  factor_list <- lapply(group$label_list, function(x) as.factor(x))
  testthat::expect_error(
    .is_adjacent(group = factor_list, j = 1, p = p, names = NULL)
  )
})

## function ".simulate_outcome" ------------------------------------------------

testthat::test_that("outcomes are simulated", {
  family <- c("gaussian", "binomial", "poisson", "cox")
  n <- 10
  p <- 5
  x <- matrix(data = stats::rnorm(n = n * p), nrow = n, ncol = p)
  beta <- stats::rnorm(n = p)
  for (i in seq_along(family)) {
    testthat::expect_error(
      .simulate_outcome(family = family[i])
    )
    testthat::expect_error(
      .simulate_outcome(family = family[i], x = x, beta = beta, n = n)
    )
    for (j in 1:2){
      if (j == 1) {
        y <- .simulate_outcome(family = family[i], x = x, beta = beta)
      } else {
        y <- .simulate_outcome(family = family[i], n = n)
      }
      .validate(na_action = NULL,
                x = matrix(data = 0, nrow = n, ncol = p),
                y = y,
                group = rep(x = 1, times = p),
                primary = NULL,
                family = family[i], hyper = NULL, alpha_init = NULL,
                alpha_final = NULL, cor = NULL, foldid = NULL,
                nfolds = NULL, lambda_init = NULL)
      testthat::expect_length(object = y, n = n)
    }
  }
})


## function ".estim_initial_coefs" ---------------------------------------------

testthat::test_that("initial coefficients are estimated", {
  family <- c("gaussian", "binomial", "poisson", "cox")
  alpha <- list(0, 0.5, 1, "pearson", "spearman", "kendall", "multiridge", NA)
  n <- 20
  p <- 10
  x <- matrix(rnorm(n * p), nrow = n, ncol = p)
  group <- rep(1:4, times = c(3, 3, 2, 2))
  beta <- rbinom(n = p, size = 1, prob = 0.5) * rnorm(p)
  for (i in seq_along(family)) {
    for (j in seq_along(alpha)) {
      if (identical(family[i], "poisson") &
            identical(alpha[[j]][1], "multiridge")) {
        next
      }
      if (identical(family[i], "cox")
          & alpha[[j]][1] %in% c("pearson", "spearman", "kendall")) {
        next
      }
      y <- .simulate_outcome(family = family[i], x = x, beta = beta)
      init <- list()
      for (k in 1:2) {
        if (k == 1) {
          lambda <- NULL
        } else {
          lambda <- init[[1]]$lambda
        }
        init[[k]] <- .estim_initial_coefs(
          x = x,
          y = y,
          family = family[i],
          alpha_init = alpha[[j]][1],
          group = group,
          foldid = NULL,
          nfolds = 10,
          lambda = lambda
        )
      }
      testthat::expect_identical(object = init[[1]], expected = init[[2]])
      testthat::expect_length(object = init[[1]]$coef, n = p)
      if (identical(alpha[[j]][1], "multiridge")) {
        length <- length(unique(group))
      } else if (is.character(alpha[[j]][1]) | is.na(alpha[[j]][1])) {
        length <- 0
      } else {
        length <- 1
      }
      testthat::expect_length(object = init[[1]]$lambda, n = length)
    }
  }
  alpha <- list(-1, Inf, "A")
  for (i in seq_along(alpha)) {
    testthat::expect_error(
      .estim_initial_coefs(
        x = x,
        y = stats::rnorm(n),
        family = "gaussian",
        alpha_init = alpha[[i]],
        group = group,
        foldid = NULL,
        nfolds = 10,
        lambda = lambda
      )
    )
  }
})

## function .residuals ---------------------------------------------------------

testthat::test_that("residuals match those from stats::residuals", {
  n <- 100
  x <- rnorm(n)
  for (family in c("gaussian", "binomial", "poisson")) {
    if (family == "gaussian") {
      y <- stats::rnorm(n)
    } else if (family == "binomial") {
      y <- stats::rbinom(n = n, size = 1, prob = 0.5)
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
