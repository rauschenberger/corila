
#----- extra -----

# Functions for the manuscript are in the folder "scripts".

#' @title
#' Precision for sign variable
#'
#' @description
#' Calculates precision for ternary variables with support \eqn{\{-1, 0, 1\}},
#' i.e., the proportion of positive or negative estimated signs
#' that match the true sign.
#'
#' @param truth
#' integer vector with values in \eqn{\{-1, 0, 1\}}
#'
#' @param estim
#' integer vector of same length with values in \eqn{\{-1, 0, 1\}}
#'
#' @return
#' Returns a scalar between 0 (minimum precision) and 1 (maximum precision),
#' or `NA` if all estimated signs equal 0.
#'
#' @examples
#' \dontshow{calc_sign_prec <- corila:::calc_sign_prec}
#' truth <- sample(x = c(-1L, 0L, 1L), size = 10L, replace = TRUE)
#' estim <- sample(x = c(-1L, 0L, 1L), size = 10L, replace = TRUE)
#' calc_sign_prec(truth = truth, estim = estim) # observed value
#' calc_sign_prec(truth = truth, estim = -truth) # lower limit 0
#' calc_sign_prec(truth = truth, estim = truth) # upper limit 1
#' calc_sign_prec(truth = truth, estim = 0L * estim) # not defined
#'
#' @keywords internal
#'
calc_sign_prec <- function(truth, estim) {
  .assert(x = truth, type = "integer", dim = Inf,
          min = -1, max = 1, na.rm = TRUE)
  truth <- as.integer(truth)
  .assert(x = estim, type = "integer", dim = length(truth),
          min = -1, max = 1, na.rm = TRUE)
  estim <- as.integer(estim)
  if (all(is.na(estim) | estim == 0)) {
    NA
  } else {
    sum(estim != 0 & truth != 0 & sign(estim) == sign(truth)) / sum(estim != 0)
  }
}

#----- simulation -----

#' @title
#' Data simulation
#'
#' @description
#' Simulates data with grouped predictor variables.
#'
#' @param family
#' character `"gaussian"`, `"binomial"`, `"poisson"`, or `"cox"`
#'
#' @param n0
#' number of training observations
#' (positive integer)
#'
#' @param n1
#' number of testing observations
#' (positive integer)
#'
#' @param n_group
#' number of variable groups
#' (positive integer)
#'
#' @param n_type
#' number of variable types
#' (positive integer)
#'
#' @param size_group
#' size of variable groups (per variable type):
#' integer vector of length `n_type`
#'
#' @param effect_size
#' effect sizes (per variable type):
#' numeric vector of length `n_type`
#'
#' @param corfac_feature
#' decrease of correlation if different variable:
#' scalar in unit interval
#'
#' @param corfac_type
#' decrease of correlation if different type:
#' scalar in unit interval
#'
#' @param corfac_group
#' decrease of correlation if different group:
#' scalar in unit interval
#'
#' @param n_group_causal
#' number of causal groups:
#' integer
#'
#' @param prop_causal
#' proportion of causal features within causal groups:
#' scalar in unit interval
#'
#' @param noise_factor
#' noise factor:
#' numeric scalar
#'
#' @param plot
#' Attempt to visualise effects of and correlation between variables?
#' (`TRUE` or `FALSE`)
#'
#' @param trial
#' logical (groups of negatively correlated subgroups)
#'
#' @return
#' Returns a list with the following slots:
#' - \eqn{n_0 \times p} matrix `x_train`
#' - \eqn{p}-dimensional vector `type`
#' - \eqn{p}-dimensional vector `group`
#' - \eqn{n_0}-dimensional vector `y_train`
#' - \eqn{n_1 \times p} matrix `x_test`
#' - \eqn{n_1}-dimensional vector `y_test`
#' - \eqn{p}-dimensional vector `beta`
#' - data frame `info` with entries
#' \eqn{n_0}, \eqn{n_1}, \eqn{p}, `n_type`,
#' `n_group`, and `family`
#'
#' @examples
#' data <- corila:::simulate()
#' dims <- function(x) {
#'    if (is.matrix(x)||is.data.frame(x)) {
#'      paste(base::dim(x), collapse = " x ")
#'    } else {
#'      paste0(base::length(x))
#'    }
#' }
#' sapply(X = data, FUN = dims)
#'
#' @keywords internal
#'
simulate <- function(family = "gaussian", n0 = 100L, n1 = 10000L, n_group = 20L,
                     n_type = 2L, size_group = c(5L, 3L), effect_size = c(1, 1),
                     corfac_feature = 0.5, corfac_type = 0.5,
                     corfac_group = 0.25, n_group_causal = 2,
                     prop_causal = 0.5, noise_factor = 1,
                     plot = FALSE, trial = FALSE) {
  # --- check arguments ---
  .assert(x = family, type = "nominal",
          support = c("gaussian", "binomial", "poisson", "cox"))
  .assert(x = n0, type = "integer", min = 2L)
  n0 <- as.integer(n0)
  .assert(x = n1, type = "integer", min = 2L)
  n1 <- as.integer(n1)
  .assert(x = n_group, type = "integer", min = 2L)
  n_group <- as.integer(n_group)
  .assert(x = n_type, type = "integer", min = 2L)
  n_type <- as.integer(n_type)
  .assert(x = size_group, type = "integer", dim = n_type, min = 1L)
  size_group <- as.integer(size_group)
  .assert(x = effect_size, type = "numeric", dim = n_type, min = 0)
  .assert(x = corfac_feature, type = "numeric", min = 0, max = 1)
  .assert(x = corfac_type, type = "numeric", min = 0, max = 1)
  .assert(x = corfac_group, type = "numeric", min = 0, max = 1)
  .assert(x = n_group_causal, type = "integer", min = 0, max = n_group)
  .assert(x = prop_causal, type = "numeric", min = 0, max = 1)
  .assert(x = noise_factor, type = "numeric", min = 0)
  .assert(x = plot, type = "logical")
  .assert(x = trial, type = "logical")
  # family = "gaussian";n0 = 100;n1 = 10000;n_group = 20;n_type = 2;
  # size_group = c(5, 3);effect_size = c(1, 1);corfac_feature = 0.5;
  # corfac_type = 0.5;corfac_group = 0.25;n_group_causal = 2;
  # prop_causal = 0.5; noise_factor = 1; plot = TRUE
  n <- n0 + n1
  #if (n_type != length(size_group)) {
  #  stop("Wrong length.")
  #}
  #- - - feature modalities and groups - - -
  p <- sum(n_group * size_group)
  if (!trial) {
    type <- rep(x = seq_len(n_type),
                times = n_group * size_group) # original
    group <- unlist(
      lapply(
        X = size_group,
        FUN = function(x) rep(x = seq_len(n_group), each = x)
      )
    ) # original
  } else {
    group <- rep(x = seq_len(n_group),
                 each = sum(size_group)) # trial 2025-09-22
    type <- rep(x = rep(x = seq_len(n_type), times = size_group),
                times = n_group) # trial 2025-09-22
  }
  #- - - effect vector - - -
  beta <- rep(x = 0, times = p)
  index_common <- sample(x = seq_len(n_group), size = n_group_causal)
  cond <- group %in% index_common
  var_binom <- stats::rbinom(n = sum(cond), size = 1L, prob = prop_causal)
  var_norm <- abs(stats::rnorm(n = sum(cond)))
  beta[cond] <- var_binom * var_norm
  if (!trial) {
    beta <- beta * rep(x = effect_size, times = table(type))
    # NB: original, added on 2025-06-20
  } else {
    for (i in seq_along(unique(type))) { # trial 2025-09-22
      beta[type == i] <- beta[type == i] * effect_size[i] # trial 2025-09-22
    } # trial 2025-09-22
  }
  if (plot) {
    tryCatch(expr = graphics::plot(x = beta, col = group, pch = type),
             error = function(x) NULL)
  }
  #- - - feature matrix - - -
  mean <- rep(x = 0, times = p)
  sigma <- matrix(data = NA, nrow = p, ncol = p)
  for (i in seq_len(p)) {
    for (j in seq_len(p)) {
      if (!trial) {
        sigma[i, j] <- corfac_feature^(i != j) *
          corfac_type^(type[i] != type[j]) *
          corfac_group^(group[i] != group[j]) # original
      } else {
        sigma[i, j] <- ifelse(i == j, 1, ifelse(group[i] == group[j] & type[i] == type[j], 0.5, ifelse(group[i] == group[j], -0.25, ifelse(type[i] == type[j], 0.125, -0.125)))) # Consider not only + but also - (but then use + and - for effect sizes), was -0.0625 MAKE THIS LINE SHORTER USING IF ELSE STATEMENTS # nolint: line_length_linter.
      }
    }
  }
  if (any(diag(sigma) != 1)) {
    stop("diagonal != 1")
  }
  if (plot) {
    tryCatch(graphics::image(x = sigma[, rev(seq_len(p))]),
             error = function(x) NULL)
  }
  x <- mvtnorm::rmvnorm(n = n, mean = mean, sigma = sigma)
  #- - - target vector - - -
  eta <- scale(x %*% as.vector(beta)) # was without scale
  if (identical(family, "gaussian")) {
    y <- eta + noise_factor * stats::rnorm(n = n, sd = stats::sd(eta))
    # NB: decrease/increase noise?
    if (stats::sd(y) == 0) {
      warning("Replacing constant y by random noise.")
      y <- stats::rnorm(n = n)
    }
  } else if (identical(family, "binomial")) {
    y <- stats::rbinom(n = n, size = 1L, prob = 1 / (1 + exp(-2 * eta)))
    # NB: was without 2*
  } else if (identical(family, "cox")) {
    time <- stats::rexp(n = n, rate = exp(eta))
    status <- stats::rbinom(n = n, size = 1L, prob = 0.5)
    #y <- cbind(time = time, status = status)
    y <- survival::Surv(time = time, event = status)
  } else if (identical(family, "poisson")) {
    y <- stats::rpois(n = n, lambda = exp(eta))
  }
  #- - - outputs - - -
  fold <- rep(x = c(0L, 1L), times = c(n0, n1))
  x_train <- x[fold == 0L, ]
  y_train <- y[fold == 0L]
  x_test <- x[fold == 1L, ]
  y_test <- y[fold == 1L]
  info <- data.frame(n0 = n0,
                     n1 = n1,
                     p = p,
                     n_type = n_type,
                     n_group = n_group,
                     family = family)
  list(x_train = x_train,
       type = type,
       group = group,
       y_train = y_train,
       x_test = x_test,
       y_test = y_test,
       beta = beta,
       info = info)
}

#' @title
#' Simulate outcome
#'
#' @description
#' Simulates outcome vector.
#'
#' @inheritParams simulate
#'
#' @param x
#' numeric \eqn{n \times p} matrix
#'
#' @param beta
#' numeric \eqn{p}-dimensional vector
#'
#' @param n
#' positive integer or \code{NULL}
#'
#' @param factor
#' non-negative numeric scalar (default: `factor=1.0`)
#' for multiplying the linear predictor
#' (to increase or decrease the signal strength)
#'
#' @return
#' Returns an \eqn{n}-dimensional outcome vector.
#'
#' @seealso
#' Use [simulate()] to simulate a predictor matrix,
#' an effect vector, and an outcome vector.
#'
#' @keywords internal
#'
#' @examples
#' \dontshow{.simulate_outcome <- corila:::.simulate_outcome}
#' # simulate independent outcome
#' .simulate_outcome(family = "gaussian", n = 10L, factor = 1.0)
#'
#' # simulate dependent outcome
#' n <- 10L
#' p <- 20L
#' x <- matrix(rnorm(n * p), n, p)
#' beta <- rnorm(p)
#' .simulate_outcome(family = "gaussian", x = x, beta = beta, factor = 1.0)
#'
.simulate_outcome <- function(family, x = NULL, beta = NULL, n = NULL,
                              factor = 1.0) {
  if (is.character(family)) family <- tolower(family)
  .assert(x = family, type = "nominal",
          support = c("gaussian", "binomial", "poisson", "cox"))
  .assert(x = x, type = "numeric", dim = c(Inf, Inf))
  .assert(x = beta, type = "numeric", dim = ncol(x))
  .assert(x = n, type = "integer", min = 1L)
  .assert(x = factor, type = "numeric", min = 0)
  if (!is.null(x) && !is.null(beta) && is.null(n)) {
    eta <- as.numeric(scale(x %*% as.vector(beta))) # was without scale
    n <- nrow(x)
  } else if (is.null(x) && is.null(beta) && !is.null(n)) {
    eta <- rep(x = 0, times = n)
  } else {
    stop("Provide either `x` and `beta` or `n`.")
  }
  if (identical(family, "gaussian")) {
    factor * eta + stats::rnorm(n = n, sd = 1)
  } else if (identical(family, "binomial")) {
    stats::rbinom(n = n, size = 1L, prob = 1 / (1 + exp(-factor * eta)))
  } else if (identical(family, "cox")) {
    time <- stats::rexp(n = n, rate = exp(factor * eta))
    status <- stats::rbinom(n = n, size = 1L, prob = 0.5)
    survival::Surv(time = time, event = status)
  } else if (identical(family, "poisson")) {
    stats::rpois(n = n, lambda = exp(factor * eta))
  }
}
