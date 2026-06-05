
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

## function "summary" ----------------------------------------------------------

data <- simulate(family = "gaussian", n1 = 50, n_group = 3,
                 size_group = c(3, 2))
object <- cv.corila(x = data$x_train, y = data$y_train, group = data$group)
print <- print(object)
testthat::test_that("print returns object",
                    {testthat::expect_equal(object, print)})
string <- capture.output(print)
testthat::test_that("corila prints a string", {
  testthat::expect_type(object = string, type = "character")
})
list <- summary(object)
cond <- vapply(X = list, FUN = is.null, FUN.VALUE = logical(1))
testthat::test_that("summary does not contain NULL",
                    {testthat::expect_all_false(cond)})
testthat::test_that("print.summary.cv.corila returns NULL invisibly", {
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

# fix bug in Cox model

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
}

## privileged information ------------------------------------------------------

for (family in c("gaussian", "binomial", "poisson", "cox")) {
  # simulate data
  data <- simulate(family = family)
  include <- stats::rbinom(n = data$info$p, size = 1, prob = 0.5)  ==  1
  # fit model
  object <- cv.corila(x = data$x_train, y = data$y_train,
                      group = data$group, include = include, family = family)
  testthat::test_that("predict is not influenced by auxiliary predictors", {
    y_hat1 <- predict(object = object, newx = data$x_test)
    y_hat2 <- predict(object = object, newx = data$x_test[, include])
    newx <- data$x_test
    newx[, !include] <- 0
    y_hat3 <- predict(object = object, newx = newx)
    testthat::expect_equal(object = y_hat1, expected = y_hat2)
    testthat::expect_equal(object = y_hat1, expected = y_hat3)
    testthat::expect_equal(object = y_hat2, expected = y_hat3)
  })
}
