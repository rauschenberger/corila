
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
          min = -1L, max = 1L, na.rm = TRUE)
  truth <- as.integer(round(truth))
  .assert(x = estim, type = "integer", dim = length(truth),
          min = -1L, max = 1L, na.rm = TRUE)
  estim <- as.integer(round(estim))
  if (all(is.na(estim) | estim == 0L)) {
    NA
  } else {
    sum(estim != 0L & truth != 0L &
          sign(estim) == sign(truth)) / sum(estim != 0L)
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
#' number of training observations:
#' positive integer scalar
#' (minimum 1,
#' maximum \eqn{10\,000})
#'
#' @param n1
#' number of testing observations:
#' non-negative integer scalar
#' (minimum 0,
#' maximum \eqn{100\,000})
#'
#' @param p
#' number of predictors:
#' positive integer scalar
#' (minimum 1 leads to a single predictor,
#' maximum \eqn{1\,000})
#'
#' @param q
#' number of predictor groups:
#' positive integer scalar
#' (minimum 1 assigns all predictors to the same group.
#' maximum `p` assigns each predictor to its own group)
#'
#' @param rho
#' correlation coefficient for predictors within the same group:
#' numeric scalar in the unit interval
#' (minimum 0 leads to uncorrelated predictors within each group,
#' maximum 1 leads to identical predictors within each group)
#'
#' @param prob_primary
#' probability for each predictor to be primary (rather than auxiliary):
#' numeric scalar in the unit interval
#' (minimum 0 leads to auxiliary predictors only,
#' maximum 1 leads to primary predictors only)
#'
#' @param signal_strength
#' non-negative numeric scalar
#' for multiplying the effect sizes
#' (default: `signal_strength=1.0`,
#' minimum 0 sets all effect sizes to 0,
#' maximum 2 to avoid undefined values)
#'
#' @param prob_group
#' probability for each predictor group to be active:
#' numeric scalar in the unit interval
#' (minimum 0 makes all groups inactive,
#' maximum 1 makes all groups active)
#'
#' @param prob_predictor
#' probability for each predictor in an active group to be active:
#' numeric scalar in the unit interval
#' (minimum 0 makes all predictors inactive,
#' maximum 1 makes all predictors in active groups active)
#'
#' @param seed
#' random seed for reproducibility:
#' integer scalar (unrestricted)
#'
#' @return
#' Returns a named list with the following slots:
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
#' @details
#' Use the objects `x_train`, `y_train`, `group`, and `primary`
#' for model training.
#' Estimated coefficients can be compared with `beta`.
#'
#' Use the object `x_test` for model testing.
#' Predicted values can be compared with `y_test`.
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
#' @seealso
#' This function calls the internal functions [.simulate_predictors()],
#' [.simulate_effects()], and [.simulate_response()] for simulating the
#' predictor matrix, the effect vector, or the response vector, respectively.
#'
#' @export
#'
#' @examples
#' data <- simulate_data(n0 = 50L, n1 = 20L, p = 30L, q = 10L,
#'                      family = "gaussian", rho = 0.5,
#'                      prob_primary = 0.5, signal_strength = 1.0,
#'                      prob_group = 0.5, prob_predictor = 0.8, seed = 1L)
#' utils::str(data, vec.len = 2L)
#'
#' @srrstats {G5.1} *data set for tests and examples is exported*
#'
simulate_data <- function(n0 = 50L, n1 = 20L, p = 30L, q = 10L,
                          family = "gaussian", rho = 0.5,
                          prob_primary = 0.5, signal_strength = 1.0,
                          prob_group = 0.5, prob_predictor = 0.8, seed = 1L) {
  # argument checks
  #.assert(x = n0, type = "integer", min = 1L, max = 1e04L)
  checkmate::assert_int(x = n0, lower = 1L, upper = 1e04L)
  n0 <- as.integer(round(n0))
  #.assert(x = n1, type = "integer", min = 0L, max = 1e05L)
  checkmate::assert_int(x = n1, lower = 0L, upper = 1e05L)
  n1 <- as.integer(round(n1))
  #.assert(x = p, type = "integer", min = 1L, max = 1e03L)
  checkmate::assert_int(x = p, lower = 1L, upper = 1e03L)
  p <- as.integer(round(p))
  #.assert(x = q, type = "integer", min = 1L, max = p)
  checkmate::assert_int(x = q, lower = 1L, upper = p)
  q <- as.integer(round(q))
  if (is.character(family)) family <- tolower(family)
  #.assert(x = family, type = "nominal",
  #        support = c("gaussian", "binomial", "poisson", "cox"))
  checkmate::assert_choice(x = family, choices = c("gaussian", "binomial",
                                                   "poisson", "cox"))
  #.assert(x = rho, type = "numeric", min = 0.0, max = 1.0)
  checkmate::assert_number(x = rho, lower = 0.0, upper = 1.0)
  rho <- round(rho, digits = 6L)
  #.assert(x = prob_primary, type = "numeric", min = 0.0, max = 1.0)
  checkmate::assert_number(x = prob_primary, lower = 0.0, upper = 1.0)
  prob_primary <- round(prob_primary, digits = 6L)
  #.assert(x = signal_strength, type = "numeric", min = 0.0, max = 2.0)
  checkmate::assert_number(x = signal_strength, lower = 0.0, upper = 2.0)
  signal_strength <- round(signal_strength, digits = 6L)
  #.assert(x = prob_group, type = "numeric", min = 0.0, max = 1.0)
  checkmate::assert_number(x = prob_group, lower = 0.0, upper = 1.0)
  prob_group <- round(prob_group, digits = 6L)
  #.assert(x = prob_predictor, type = "numeric", min = 0.0, max = 1.0)
  checkmate::assert_number(x = prob_predictor, lower = 0.0, upper = 1.0)
  prob_predictor <- round(prob_predictor, digits = 6L)
  #.assert(x = seed, type = "integer")
  checkmate::assert_int(x = seed)
  set.seed(as.integer(round(seed)))
  # simulation
  n <- n0 + n1
  group <- sort(c(seq_len(q),
                  sample(x = seq_len(q), size = p - q, replace = TRUE)))
  primary <- as.logical(stats::rbinom(n = p, size = 1L, prob = prob_primary))
  holdout <- rep(x = c(FALSE, TRUE), times = c(n0, n1))
  x <- .simulate_predictors(n = n, group = group, rho = rho, seed = seed)
  beta <- .simulate_effects(group = group,
                            signal_strength = signal_strength,
                            prob_group = prob_group,
                            prob_predictor = prob_predictor,
                            seed = seed)
  y <- .simulate_response(family = family, x = x, beta = beta, seed = seed)
  # names of observations and predictors
  rownames <- c(paste0("train_"[n0 > 0L], seq_len(n0)),
                paste0("test_"[n1 > 0L], seq_len(n1)))
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
  x_train <- x[!holdout, , drop = FALSE]
  y_train <- y[!holdout]
  x_test <- x[holdout, , drop = FALSE]
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
#' @inheritParams simulate_data p rho seed
#'
#' @param n
#' number of observations:
#' positive integer
#' (minimum 1, maximum \eqn{110\,000})
#'
#' @param group
#' group indicator:
#' integer vector of length \eqn{p} with entries between 1 and \eqn{q},
#' where \eqn{p} is the number of predictors
#' and \eqn{q} is the number of predictor groups
#' (maximum length \eqn{1\,000},
#' minimum entry 1, maximum entry \eqn{1\,000})
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
#' .simulate_predictors(n = 5L, group = rep(c(1L, 2L), each = 3L), rho = 1.0)
#'
.simulate_predictors <- function(n, p = NULL, group = NULL, rho = 0.0,
                                 seed = 1L) {
  if (is.null(p) == is.null(group)) stop("Provide either p or group.")
  #.assert(x = n, type = "integer", min = 1L, max = 11e04L)
  checkmate::assert_int(x = n, lower = 1L, upper = 11e04L)
  #.assert(x = p, type = "integer", min = 1L, max = 1e03L)
  checkmate::assert_int(x = p, lower = 1L, upper = 1e03L, null.ok = TRUE)
  if (is.null(group)) group <- seq_len(p)
  #.assert(x = group, type = "integer", dim = Inf, min = 1L,
  #        max = length(group))
  #.assert(x = length(group), type = "integer", min = 1L, max = 1e03L)
  checkmate::assert_integer(x = group, min.len = 1L, max.len = 1e03L,
                            lower = 1L, upper = length(group), null.ok = TRUE)
  group <- as.integer(round(group))
  #.assert(x = rho, type = "numeric", min = 0.0, max = 1.0)
  checkmate::assert_number(x = rho, lower = 0.0, upper = 1.0)
  rho <- round(rho, digits = 6L)
  #.assert(x = seed, type = "integer")
  checkmate::assert_int(x = seed)
  set.seed(as.integer(round(seed)))
  p <- length(group)
  mu <- rep(x = 0.0, times = p)
  sigma <- rho * outer(X = group, Y = group, FUN = "==") +
    (1.0 - rho) * diag(rep(x = 1.0, times = p))
  x <- MASS::mvrnorm(n = n, mu = mu, Sigma = sigma)
  #x <- mvtnorm::rmvnorm(n = n, mean = mu, sigma = sigma)
  if (n == 1L) x <- matrix(data = x, nrow = 1L)
  x
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
#' group <- rep(c(1L:5L), each = 3L)
#' .simulate_effects(group = group)
#' .simulate_effects(group = group, signal_strength = 1.5)
#'
.simulate_effects <- function(group, prob_group = 0.5, prob_predictor = 0.8,
                              signal_strength = 1.0, seed = 1L) {
  #.assert(x = group, type = "integer", dim = Inf, min = 1L,
  #        max = length(group))
  #.assert(x = length(group), type = "integer", min = 1L, max = 1e03L)
  checkmate::assert_integer(x = group, min.len = 1L, max.len = 1e03L,
                            lower = 1L, upper = length(group))
  group <- as.integer(round(group))
  #.assert(x = prob_group, type = "numeric", min = 0.0, max = 1.0)
  checkmate::assert_number(x = prob_group, lower = 0.0, upper = 1.0)
  prob_group <- round(prob_group, digits = 6L)
  #.assert(x = prob_predictor, type = "numeric", min = 0.0, max = 1.0)
  checkmate::assert_number(x = prob_predictor, lower = 0.0, upper = 1.0)
  prob_predictor <- round(prob_predictor, digits = 6L)
  #.assert(x = signal_strength, type = "numeric", min = 0.0)
  checkmate::assert_number(x = signal_strength, lower = 0.0, upper = 2.0)
  signal_strength <- round(signal_strength, digits = 6L)
  checkmate::assert_int(x = seed)
  #.assert(x = seed, type = "integer")
  set.seed(as.integer(round(seed)))
  p <- length(group)
  order <- order(group)
  size <- tabulate(group[order])
  q <- length(size)
  beta_group <-
    sign(stats::rnorm(n = q)) *
    stats::rbinom(n = q, size = 1L, prob = prob_group)
  #beta <- rep(x = NA, times = p)
  #for (i in seq_len(q)) {
  #  beta[group == i] <-
  #    beta_group[i] * signal_strength *
  #    abs(stats::rnorm(n = sum(group == i))) *
  #    stats::rbinom(n = sum(group == i), size = 1L, prob = prob_predictor)
  #}
  #beta
  beta <- numeric(p)
  beta[order] <- signal_strength * rep(x = beta_group, times = size) *
    abs(stats::rnorm(n = p)) *
    stats::rbinom(n = p, size = 1L, prob = prob_predictor)
  beta
}

#' @title
#' Simulate outcome
#'
#' @description
#' Simulates outcome vector.
#'
#' @inheritParams simulate_data family seed
#'
#' @param x
#' predictors:
#' numeric matrix with \eqn{n} rows (observations)
#' and \eqn{p} columns (predictors)
#'
#' @param beta
#' effects:
#' numeric vector of length \eqn{p}
#'
#' @param n
#' sample size:
#' positive integer scalar or \code{NULL}
#' (minimum 1, maximum \eqn{100\,000})
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
#' set.seed(1L)
#' n <- 10L
#' p <- 20L
#' x <- matrix(rnorm(n * p), n, p)
#' beta <- rnorm(p)
#' .simulate_response(family = "gaussian", x = x, beta = beta)
#'
.simulate_response <- function(family, x = NULL, beta = NULL, n = NULL,
                               seed = 1L) {
  if (is.character(family)) family <- tolower(family)
  #.assert(x = family, type = "nominal",
  #        support = c("gaussian", "binomial", "poisson", "cox"))
  checkmate::assert_choice(x = family, choices = c("gaussian", "binomial",
                                                   "poisson", "cox"))
  if (is.null(x) != is.null(beta)) {
    stop("Provide either none or both of 'x' and 'beta'.")
  }
  #.assert(x = x, type = "numeric", dim = c(Inf, Inf))
  checkmate::assert_matrix(x = x, mode = "numeric",
                           min.rows = 1L, max.rows = 1e05L,
                           min.cols = 1L,
                           any.missing = FALSE, null.ok = TRUE)
  #.assert(x = beta, type = "numeric", dim = ncol(x))
  checkmate::assert_numeric(x = beta, len = ncol(x), any.missing = FALSE,
                            null.ok = TRUE)
  #.assert(x = n, type = "integer", min = 1L, max = 1e05L)
  checkmate::assert_int(x = n, lower = 1L, upper = 1e05L, null.ok = TRUE)
  #.assert(x = seed, type = "integer")
  checkmate::assert_int(x = seed)
  set.seed(as.integer(round(seed)))
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
