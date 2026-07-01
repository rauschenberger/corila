
# Integration tests ------------------------------------------------------------

## functions ".forescale" and ".backscale" -------------------------------------

for (glmnet in c(FALSE, TRUE)) {
  for (family in c("gaussian", "binomial", "poisson", "cox")) {
    cat(paste0(
      ifelse(glmnet, "glmnet::cv.glmnet", "stats::glm"),
      ", family=\"", family, "\"\n"
    ))
    testthat::test_that("stats::glm without/with scale returns same results", {
      set.seed(1)
      #--- simulate data ---
      n0 <- 100
      n1 <- 50
      p <- 3
      n <- n0 + n1
      fold <- rep(x = c(0, 1), times = c(n0, n1))
      foldid <- sample(rep(seq_len(10), length.out = n0))
      sd <- stats::rpois(n = p, lambda = 5)
      x <- vapply(X = sd,
                  FUN = function(x) stats::rnorm(n = n, sd = x),
                  FUN.VALUE = numeric(length = n))
      beta <- stats::rnorm(n = p)
      eta <- x %*% beta
      if (family  ==  "gaussian") {
        y <- stats::rnorm(n = n, mean = eta)
      } else if (family == "binomial") {
        y <- stats::rbinom(n = n, size = 1, prob = 1 / (1 + exp(-eta)))
      } else if (family == "poisson") {
        y <- stats::rpois(n = n, lambda = exp(eta))
      } else if (family == "cox") {
        time <- stats::rexp(n = n, rate = exp(eta))
        status <- stats::rbinom(n = n, prob = 0.5, size = 1)
        y <- survival::Surv(time = time, event = status)
      }
      data <- data.frame(x = x)
      #--- regression without standardisation ---
      # (NB: glmnet standardises internally for tuning lambda)
      if (glmnet) {
        lm1 <- glmnet::cv.glmnet(x = x[fold == 0, ],
                                 y = y[fold == 0],
                                 family = family,
                                 foldid = foldid,
                                 lambda = c(99e99, 0),
                                 standardize = TRUE)
        y_hat1 <- as.numeric(stats::predict(object = lm1,
                                            newx = x[fold == 1, ],
                                            s = 0,
                                            type = "response"))
      } else {
        if (family == "cox") {
          lm1 <- survival::coxph(y[fold == 0] ~ .,
                                 data = data[fold == 0, ])
        } else {
          lm1 <- stats::glm(formula = y[fold == 0] ~ .,
                            family = family,
                            data = data[fold == 0, ])
        }
        y_hat1 <- stats::predict(
          object = lm1,
          newdata = data[fold == 1, ],
          type = ifelse(family == "cox", "risk", "response")
        )
      }
      coef1 <- as.numeric(stats::coef(object = lm1, s = 0))
      #--- regression with standardisation ---
      scale <- .forescale(x = x[fold == 0, ], y = y[fold == 0], family = family)
      newx <- .forescale(x = x[fold == 1, ], pars = scale$pars)$x
      if (glmnet) {
        lm2 <- glmnet::cv.glmnet(x = scale$x,
                                 y = scale$y,
                                 family = family,
                                 foldid = foldid,
                                 lambda = c(99e99, 0),
                                 standardize = TRUE)
        y_hat_temp <- as.numeric(stats::predict(object = lm2,
                                                newx = newx,
                                                s = 0,
                                                type = "response"))
      } else {
        if (family == "cox") {
          lm2 <- survival::coxph(formula = scale$y ~ .,
                                 data = data.frame(x = scale$x))
        } else {
          lm2 <- stats::glm(formula = scale$y ~ .,
                            family = family,
                            data = data.frame(x = scale$x))
        }
        y_hat_temp <- as.numeric(
          stats::predict(
            object = lm2,
            newdata = data.frame(x = newx),
            type = ifelse(family == "cox", "risk", "response")
          )
        )
      }
      coef.temp <- as.numeric(stats::coef(object = lm2, s = 0))
      result <- .backscale(pars = scale$pars, y = y_hat_temp, coef = coef.temp)
      y_hat2 <- result$y
      coef2 <- result$coef
      #--- equality ---
      testthat::expect_equal(object = coef1,
                             expected = coef2,
                             check.attributes = FALSE)
      if (glmnet & family == "cox") {
        testthat::expect_equal(object = y_hat1,
                               expected = y_hat2 * mean(y_hat1 / y_hat2),
                               tolerance = 1e-06,
                               check.attributes = FALSE)
      } else {
        testthat::expect_equal(object = y_hat2,
                               expected = y_hat1,
                               check.attributes = FALSE)
      }
      dev1 <- .deviance(y = y[fold == 1], y_hat = y_hat1, family = family)
      dev2 <- .deviance(y = y[fold == 1], y_hat = y_hat2, family = family)
      testthat::expect_equal(object = dev1,
                             expected = dev2,
                             check.attributes = FALSE)
    })
  }
}

## S3 methods for class cv.corila ----------------------------------------------

n <- as.integer(10)
data <- simulate(family = "gaussian", n0 = n, n1 = n, n_group = 3,
                 size_group = c(3, 2))
p <- data$info$p
object <- cv.corila(x = data$x_train, y = data$y_train, group = data$group)

testthat::test_that("function 'cv.corila' returns a list", {
  testthat::expect_type(object = object, type = "list")
})

testthat::test_that("function 'coef.cv.corila' returns finite p-vector", {
  beta_hat <- coef(object, s = "lambda.min")
  testthat::expect_type(object = beta_hat, type = "double")
  testthat::expect_length(object = beta_hat, n = p + 1)
  testthat::expect_true(all(is.finite(beta_hat)))
  testthat::expect_error(object = coef(object, s = "lambda.1se"))
  testthat::expect_error(object = coef(object, s = -1))
})

testthat::test_that("function 'predict.cv.corila' returns finite n-vector", {
  y_hat <- predict(object, newx = data$x_train, s = "lambda.min")
  testthat::expect_type(object = y_hat, type = "double")
  testthat::expect_length(object = y_hat, n = n)
  testthat::expect_true(all(is.finite(y_hat)))
  testthat::expect_error(object = predict(object, newx = data$x_train,
                                          s = "lambda.1se"))
  testthat::expect_error(object = predict(object, newx = data$x_train,
                                          s = -1))
})

testthat::test_that("function 'nobs.cv.corila' returns the correct integer", {
  n_obs <- stats::nobs(object)
  testthat::expect_type(object = n_obs, type = "integer")
  testthat::expect_length(object = n_obs, n = 1)
  testthat::expect_true(is.finite(n_obs))
  testthat::expect_identical(object = n_obs, expected = n)
})

testthat::test_that("function 'plot.cv.corila' returns NULL invisibly", {
  testthat::expect_invisible(call = plot(object))
  testthat::expect_identical(object = plot(object), expected = NULL)
})

testthat::test_that("function 'print.cv.corila' returns object invisibly", {
  testthat::expect_invisible(call = print(object))
  testthat::expect_equal(object = print(object), expected = object)
})

testthat::test_that("function 'print.cv.corila' prints a string", {
  string <- capture.output(print(object))
  testthat::expect_type(object = string, type = "character")
  testthat::expect_length(object = string, n = 3)
  testthat::expect_true(object = any(grepl(pattern = "cv.corila", x = string)))
})

testthat::test_that(paste("function 'summary.cv.corila' returns a list",
                          "with 13 named slots"), {
  testthat::expect_type(object = summary(object), type = "list")
  testthat::expect_length(object = summary(object), n = 13)
  testthat::expect_type(object = names(summary(object)), type = "character")
})

testthat::test_that(paste("function 'print.summary.cv.corila'",
                          "returns NULL invisibly"), {
  testthat::expect_invisible(call = print(summary(object)))
  testthat::expect_identical(object = print(summary(object)), expected = NULL)
})

## function "cv.corila" --------------------------------------------------------

for (family in c("gaussian", "binomial", "poisson", "cox")) {
  message(paste0("family=\"", family, "\""))
  data <- simulate(family = family, n1 = 50, n_group = 3,
                   size_group = c(3, 2))
  colnames(data$x_train) <- LETTERS[seq_len(ncol(data$x_train))]
  group <- list()
  group$vector <- data$group
  group$vector_char <- LETTERS[data$group]
  group$list <- lapply(X = unique(group$vector),
                       FUN = function(x) which(group$vector == x))
  group$list_char <- lapply(X = unique(group$vector),
                            FUN = function(x) LETTERS[which(group$vector == x)])
  group$matrix <- 1 * outer(X = group$vector,
                            Y = group$vector,
                            FUN = "==")
  model <- lapply(X = group, FUN = function(x) NULL)
  for (i in seq_along(group)) {
    set.seed(1)
    model[[i]] <- cv.corila(x = data$x_train,
                            y = data$y_train,
                            group = group[[i]],
                            family = family)
  }
  coef <- lapply(X = model,
                 FUN = stats::coef)
  y_hat <- lapply(X = model,
                  FUN = function(x) predict(object = x, newx = data$x_test))
  testthat::test_that(
    desc = paste0(
      "corila returns same coefficients ",
      "with argument group as vector, list, or matrix"
    ),
    code = {
      lapply(X = coef[-1],
             FUN = testthat::expect_equal,
             expected = coef[[1]],
             check.attributes = FALSE)
    }
  )
  testthat::test_that(
    desc = paste0(
      "corila returns same predictions ",
      "with argument group as vector, list, or matrix"
    ),
    code = {
      lapply(X = y_hat[-1],
             FUN = testthat::expect_equal,
             expected = y_hat[[1]],
             check.attributes = FALSE)
    }
  )
  testthat::test_that(
    desc = paste0(
      "function predict returns same results ",
      "as feature matrix times coef"
    ),
    code = {
      eta <- cbind(c(1)[family != "cox"], data$x_test) %*% coef$vector
      if (family %in% c("gaussian")) {
        pred <- eta
      } else if (family == "binomial") {
        pred <- 1 / (1 + exp(-eta))
      } else if (family %in% c("poisson", "cox")) {
        pred <- exp(eta)
      }
      if (family == "cox") {
        testthat::expect_equal(
          object = as.numeric(pred),
          expected = as.numeric(y_hat$vector * mean(pred / y_hat$vector))
        )
      } else {
        testthat::expect_equal(
          object = as.numeric(pred),
          expected = as.numeric(y_hat$vector)
        )
      }
    }
  )
}

for (family in c("gaussian", "binomial", "poisson", "cox")) {
  message(paste0("family=\"", family, "\""))
  n <- 100
  p <- 50
  sd <- abs(stats::rnorm(n = p))
  x <- y <- list()
  x$original <- vapply(X = sd,
                       FUN = function(x) stats::rnorm(n = n, mean = 0, sd = x),
                       FUN.VALUE = numeric(length = n))
  beta <- stats::rbinom(n = p, size = 1, prob = 0.2) * stats::rnorm(n = p)
  eta <- as.numeric(scale(x$original %*% beta))
  if (family == "gaussian") {
    y <- eta + stats::rnorm(n = n, sd = 0.5)
  } else if (family == "binomial") {
    y <- stats::rbinom(n = n, size = 1, prob = 1 / (1 + exp(-eta)))
  } else if (family == "poisson") {
    y <- stats::rpois(n = n, lambda = exp(eta))
  } else if (family == "cox") {
    time <- stats::rexp(n = n, rate = exp(eta))
    status <- stats::rbinom(n = n, prob = 0.5, size = 1)
    y <- survival::Surv(time = time, event = status)
  }
  x$scaled <- scale(x$original)
  y_hat <- list()
  for (i in seq_along(x)) {
    set.seed(1)
    object <- cv.corila(x = x[[i]],
                        y = y,
                        group = rep(1:5, each = 10),
                        family = family)
    y_hat[[i]] <- predict(object = object, newx = x[[i]])
  }
  testthat::test_that(paste0(
    "corila returns same predictions",
    "without and with standardisation"
  ), {
    testthat::expect_equal(y_hat[[1]], y_hat[[2]])
  })
}

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
    stats::rbinom(n = sum(p), size = 1, prob = 0.2)
  eta <- as.numeric(x %*% beta)
  if (family == "gaussian") {
    y <- eta + 0.5 * stats::rnorm(n = n, sd = stats::sd(eta))
  } else if (family == "binomial") {
    y <- stats::rbinom(n = n, size = 1, prob = 1 / (1 + exp(-eta)))
  } else if (family == "cox") {
    time <- stats::rexp(n = n, rate = exp(eta))
    status <- stats::rbinom(n = n, prob = 0.5, size = 1)
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
      x = drop(coef(object)[1] + x[!cond, ] %*% coef(object)[-1]),
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
                 penalties = rep(1, times = length(p) + 1))
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

## privileged information ------------------------------------------------------

for (family in c("gaussian", "binomial", "poisson", "cox")) {
  # simulate data
  data <- simulate(family = family)
  primary <- as.logical(stats::rbinom(n = data$info$p, size = 1, prob = 0.5))
  # fit model
  object <- cv.corila(x = data$x_train, y = data$y_train,
                      group = data$group, primary = primary, family = family)
  testthat::test_that("predict is not influenced by auxiliary predictors", {
    y_hat1 <- predict(object = object, newx = data$x_test)
    y_hat2 <- predict(object = object, newx = data$x_test[, primary])
    newx <- data$x_test
    newx[, !primary] <- 0
    y_hat3 <- predict(object = object, newx = newx)
    testthat::expect_equal(object = y_hat1, expected = y_hat2)
    testthat::expect_equal(object = y_hat1, expected = y_hat3)
    testthat::expect_equal(object = y_hat2, expected = y_hat3)
  })
}

## noise susceptibility --------------------------------------------------------

#' @srrstats {G5.9} *noise susceptibility test:*
#' @srrstats {G5.9a} *- adding trivial noise*
#' @srrstats {G5.9b} *- running under different random seeds*

for (family in c("gaussian", "binomial", "poisson", "cox")) {
  # simulate data
  data <- simulate(family = family)
  foldid <- .folds(y = data$y_train, family = family, nfolds = 10)
  object <- y_hat <- coef <- list()
  for (i in 1:2) {
    set.seed(i)
    x <- data$x_train + stats::rnorm(n = length(data$x_train),
                                     sd = .Machine$double.eps)
    if (family == "gaussian") {
      y <- data$y_train + stats::rnorm(n = length(data$y_train),
                                       sd = .Machine$double.eps)
    } else {
      y <- data$y_train
    }
    object[[i]] <- cv.corila(x = x, y = y, group = data$group,
                             family = family, foldid = foldid)
    coef[[i]] <- coef(object[[i]])
    y_hat[[i]] <- predict(object[[i]], newx = data$x_test)
  }
  testthat::test_that("coefficients do not change", {
    testthat::expect_equal(object = coef[[2]], expected = coef[[1]])
  })
  testthat::test_that("prediction do not change", {
    testthat::expect_equal(object = y_hat[[2]], expected = y_hat[[1]])
  })
}

## complete case analysis ------------------------------------------------------

testthat::test_that("complete case analysis works", {
  family <- "gaussian"
  data <- simulate(family = family)
  foldid <- .folds(y = data$y_train, family = family, nfolds = 10)
  missing <- stats::rbinom(n = nrow(data$x_train), size = 1, prob = 0.2) == 1
  data$x_train[missing, 1] <- NA
  object0 <- cv.corila(x = data$x_train[!missing, ],
                       y = data$y_train[!missing],
                       group = data$group,
                       family = family,
                       foldid = foldid[!missing],
                       na_action = "error")
  object1 <- cv.corila(x = data$x_train,
                       y = data$y_train,
                       group = data$group,
                       family = family,
                       foldid = foldid,
                       na_action = "complete_cases")
  testthat::expect_identical(object = object0, expected = object1)
})
