
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
#' Simulates a predictor matrix, an effect vector and a response vector.
#' The simulated datasets can be used for modelling a response based on
#' grouped and correlated primary and auxiliary predictors.
#'
#' @inheritParams cv.corila family
#'
#' @param n0
#' number of training observations
#' (positive integer)
#'
#' @param n1
#' number of testing observations
#' (positive integer)
#'
#' @param p
#' number of predictors
#' (positive integer)
#'
#' @param q
#' number of predictor groups
#' (positive integer)
#'
#' @param rho
#' correlation coefficient for predictors within the same group:
#' numeric scalar in the unit interval (minimum 0, maximum 1)
#'
#' @param prob_primary
#' probability for each predictor to be primary (rather than auxiliary):
#' numeric scalar in the unit interval
#' (minimum 0 leads to auxiliary predictors only,
#' maximum 1 leads to primary predictors only)
#'
#' @param signal_strength
#' non-negative numeric scalar (default: `signal_strength=1`)
#' for multiplying the effect sizes
#' (to increase or decrease the signal strength)
#'
#' @param prob_group
#' probability for each group to be active:
#' numeric scalar in the unit interval
#'
#' @param prob_predictor
#' probability for each predictor in an active group to be active:
#' numeric scalar in the unit interval
#'
#' @param seed
#' random seed for reproducibility:
#' integer scalar
#'
#' @aliases simulate
#'
#' @return
#' Returns a list with multiple slots:
#' - `x_train`:
#'   predictor matrix of the training observations
#'   (\eqn{n_0} rows, \eqn{p} columns)
#' - `y_train`:
#'   response vector of the training observations
#'   (length \eqn{n_0})
#' - `group`:
#'   integer vector indicating the group of the predictors
#'   (length \eqn{p})
#' - `primary`:
#'   logical vector indicating
#'   primary (`TRUE`) and auxiliary (`FALSE`) predictors
#'   (length \eqn{p})
#' - `beta`:
#'    numeric vector of the effects of the predictors on the response
#'    (length \eqn{p})
#' - `x_test`:
#'   \eqn{n_1 \times p} predictor matrix for the test observations
#' - `y_test`:
#'   response vector for the test observations of length \eqn{n_1}
#'
#' Training and testing observations are named `train_` or `test_`,
#' respectively, followed by a number indexing the observations
#' (e.g., `train_1` or `test_1`).
#'
#' Primary and auxiliary predictors are named `pri_` or `aux_`, respectively,
#' followed by a number indexing the predictor groups, a point,
#' and a number indexing the predictors within this group
#' (e.g., `pri_1.1` or `aux_1.1`).
#'
#' @details
#' - Use the objects `x_train`, `y_train`, `group`, and `primary`
#'   for model training.
#'   Estimated coefficients can be compared with `beta`.
#' - Use the object `x_test` for model testing.
#'   Predicted values can be compared with `y_test`.
#'
#' @seealso
#' This function calls the internal functions [.simulate_predictors()],
#' [.simulate_effects()], and [.simulate_response()] for simulating the
#' predictor matrix, the effect vector, or the response vector, respectively.
#'
#' @export
#'
#' @examples
#' data <- simulate_data(n0 = 100, n1 = 10000)
#' utils::str(data, vec.len = 2)
#'
#' @srrstats {G5.1} *data set for tests and examples is exported*
#'
simulate_data <- function(n0 = 50L, n1 = 20L, p = 30L, q = 10L,
                          family = "gaussian", rho = 0.5,
                          prob_primary = 0.5, signal_strength = 1.0,
                          prob_group = 0.5, prob_predictor = 0.8, seed = 1L) {
  # argument checks
  .assert(x = n0, type = "integer", min = 2L)
  n0 <- as.integer(n0)
  .assert(x = n1, type = "integer", min = 0L)
  n1 <- as.integer(n1)
  .assert(x = p, type = "integer", min = 2L)
  p <- as.integer(p)
  .assert(x = q, type = "integer", min = 1L)
  q <- as.integer(q)
  .assert(x = family, type = "nominal",
          support = c("gaussian", "binomial", "poisson", "cox"))
  .assert(x = rho, type = "numeric", min = 0.0, max = 1.0)
  .assert(x = prob_primary, type = "numeric", min = 0.0, max = 1.0)
  .assert(x = prob_group, type = "numeric", min = 0.0, max = 1.0)
  .assert(x = prob_predictor, type = "numeric", min = 0.0, max = 1.0)
  .assert(x = seed, type = "integer")
  # simulation
  set.seed(seed)
  n <- n0 + n1
  group <- sort(c(seq_len(q),
                  sample(x = seq_len(q), size = p - q, replace = TRUE)))
  primary <- as.logical(stats::rbinom(n = p, size = 1L, prob = prob_primary))
  holdout <- rep(x = c(FALSE, TRUE), times = c(n0, n1))
  x <- .simulate_predictors(n = n, group = group, rho = rho)
  beta <- .simulate_effects(group = group,
                            signal_strength = signal_strength,
                            prob_group = prob_group,
                            prob_predictor = prob_predictor)
  y <- .simulate_response(family = family, x = x, beta = beta)
  # names of observations and predictors
  rownames <- c(paste0("train_", seq_len(n0)), paste0("test_", seq_len(n1)))
  rownames(x) <- names(y) <- rownames
  count <- vapply(
    X = seq_len(p),
    FUN = function(i) sum(group[seq_len(i)] == group[i]),
    FUN.VALUE = numeric(1L)
  )
  colnames <- paste0(group, ".", count)
  colnames[primary] <- paste0("pri_", colnames[primary])
  colnames[!primary] <- paste0("aux_", colnames[!primary])
  colnames(x) <- names(primary) <- names(group) <- names(beta) <- colnames
  # training/test split
  x_train <- x[!holdout, ]
  y_train <- y[!holdout]
  x_test <- x[holdout, ]
  y_test <- y[holdout]
  # privileged information
  x_test[, !primary] <- NA
  # dataset
  list(x_train = x_train,
       y_train = y_train,
       group = group,
       primary = primary,
       beta = beta,
       x_test = x_test,
       y_test = y_test)
}

#' @title
#' Simulate predictors
#'
#' @description
#' Simulates predictor matrix.
#'
#' @inheritParams simulate_data p rho
#'
#' @param n
#' number of observations (positive integer)
#'
#' @param p
#' number of predictors (positive integer)
#'
#' @param group
#' integer vector (length \eqn{p}, minimum 1, maximum \eqn{p})
#'
#' @return
#' Returns a numeric matrix with \eqn{n} rows (observations)
#' and \eqn{p} columns (predictors).
#'
#' @seealso
#' This function is called by [simulate_data()].
#'
#' @keywords internal
#'
#' @examples
#' \dontshow{.simulate_predictors <- corila:::.simulate_predictors}
#' .simulate_predictors(n = 5L, p = 7L)
#'
#' .simulate_predictors(n = 5L, group = rep(c(1L, 2L), each = 3L), rho = 1.0)
#'
.simulate_predictors <- function(n, p = NULL, group = NULL, rho = 0.0) {
  if (is.null(p) == is.null(group)) stop("Provide either p or group.")
  .assert(x = n, type = "integer", min = 2)
  .assert(x = p, type = "integer", min = 2)
  if (is.null(group)) group <- seq_len(p)
  .assert(x = group, type = "integer", dim = Inf, min = 1L, max = length(group))
  group <- as.integer(group)
  .assert(x = rho, type = "numeric", min = 0.0, max = 1.0)
  p <- length(group)
  mu <- rep(x = 0.0, times = p)
  sigma <- rho * outer(X = group, Y = group, FUN = "==") +
    (1.0 - rho) * diag(rep(x = 1.0, times = p))
  MASS::mvrnorm(n = n, mu = mu, Sigma = sigma)
}

#' @title
#' Simulate effects
#'
#' @description
#' Simulates effect vector.
#'
#' @inheritParams simulate_data
#' @inheritParams .simulate_predictors group
#'
#' @return
#' Returns a numeric vector of length \eqn{p}.
#'
#' @seealso
#' This function is called by [simulate_data()].
#'
#' @keywords internal
#'
#' @examples
#' \dontshow{.simulate_effects <- corila:::.simulate_effects}
#' .simulate_effects(group = rep(c(1L:5L), each = 3L))
#'
.simulate_effects <- function(group, prob_group = 0.5, prob_predictor = 0.8,
                              signal_strength = 1.0) {
  .assert(x = group, type = "integer", dim = Inf, min = 1L, max = length(group))
  group <- as.integer(group)
  .assert(x = prob_group, type = "numeric", min = 0.0, max = 1.0)
  .assert(x = prob_predictor, type = "numeric", min = 0.0, max = 1.0)
  .assert(x = signal_strength, type = "numeric", min = 0.0)
  p <- length(group)
  q <- length(unique(group))
  beta_group <-
    sign(stats::rnorm(n = q)) *
    stats::rbinom(n = q, size = 1L, prob = prob_group)
  beta <- rep(x = NA, times = p)
  for (i in seq_len(q)) {
    beta[group == i] <-
      beta_group[i] * signal_strength *
      abs(stats::rnorm(n = sum(group == i))) *
      stats::rbinom(n = sum(group == i), size = 1L, prob = prob_predictor)
  }
  beta
}

#' @title
#' Simulate outcome
#'
#' @description
#' Simulates outcome vector.
#'
#' @inheritParams simulate_data family
#'
#' @param x
#' predictors:
#' numeric matrix with \eqn{n} rows (observations)
#' and \eqn{p} columns (predictors)
#'
#' @param beta
#' effects:
#' numeric vector of length \eqn{q}
#'
#' @param n
#' sample size:
#' positive integer scalar or \code{NULL}
#'
#' @return
#' Returns an \eqn{n}-dimensional response vector.
#'
#' @seealso
#' This function is called by [simulate_data()].
#'
#' @keywords internal
#'
#' @examples
#' \dontshow{.simulate_response <- corila:::.simulate_response}
#' # simulate independent response
#' .simulate_response(family = "gaussian", n = 10L)
#'
#' # simulate dependent response
#' n <- 10L
#' p <- 20L
#' x <- matrix(rnorm(n * p), n, p)
#' beta <- rnorm(p)
#' .simulate_response(family = "gaussian", x = x, beta = beta)
#'
.simulate_response <- function(family, x = NULL, beta = NULL, n = NULL) {
  if (is.character(family)) family <- tolower(family)
  .assert(x = family, type = "nominal",
          support = c("gaussian", "binomial", "poisson", "cox"))
  .assert(x = x, type = "numeric", dim = c(Inf, Inf))
  .assert(x = beta, type = "numeric", dim = ncol(x))
  .assert(x = n, type = "integer", min = 1L)
  if (!is.null(x) && !is.null(beta) && is.null(n)) {
    eta <- as.numeric(x %*% as.vector(beta))
    n <- nrow(x)
  } else if (is.null(x) && is.null(beta) && !is.null(n)) {
    eta <- rep(x = 0.0, times = n)
  } else {
    stop("Provide either `x` and `beta` or `n`.")
  }
  if (identical(family, "gaussian")) {
    eta + stats::rnorm(n = n, sd = 1.0)
  } else if (identical(family, "binomial")) {
    stats::rbinom(n = n, size = 1L, prob = 1.0 / (1.0 + exp(-eta)))
  } else if (identical(family, "cox")) {
    time <- stats::rexp(n = n, rate = exp(eta))
    status <- stats::rbinom(n = n, size = 1L, prob = 0.5)
    survival::Surv(time = time, event = status)
  } else if (identical(family, "poisson")) {
    stats::rpois(n = n, lambda = exp(eta))
  }
}
