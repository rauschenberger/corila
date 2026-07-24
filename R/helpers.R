
#----- helpers -----

#' @title
#' Standardisation
#'
#' @description
#' Transforms variables to mean 0 and variance 1.
#'
#' @inheritParams cv.corila x
#'
#' @param y
#' response vector
#' (only required if `family="gaussian"`)
#' or `NULL`
#'
#' @param family
#' character string `"gaussian"`, `"binomial"`,
#' `"poisson"`, or `"cox"`;
#' or `NULL` (if `pars` is provided)
#'
#' @param pars
#' list as defined in section *Value*,
#' or `NULL` (if `family` is provided)
#'
#' @details
#' This function is called by [corila()] for the training data
#' and by [predict.corila()] for the testing data.
#'
#' @return
#' Returns a list with multiple slots:
#' - standardised \eqn{n_0 \times p} or \eqn{n_1 \times p}
#' predictor matrix \eqn{x}
#' - standardised \eqn{n_0}-dimensional or \eqn{n_1}-dimensional
#' response vector \eqn{y}
#' (only if \eqn{y} is provided and `family = "gaussian"`
#' or `pars$family = "gaussian"`; otherwise output equals input)
#' - character string `family` indicates the model (`"gaussian"`,
#' `"binomial"`, `"poisson"`, or `"cox"`),
#' determined by argument `family` or `pars$family`
#' - list `pars` with slots `mu.x` and `sd.x`
#' (\eqn{p}-dimensional vectors of means and standard deviations
#' of the predictor variables),
#' `mu.y` and `sd.y`
#' (mean and standard deviation of response variable for Gaussian family,
#' 0 and 1 for other families),
#' and `family`
#' (character string `"gaussian"`, `"binomial"`,
#' `"poisson"`, or `"cox"`)
#'
#' @seealso
#' Use function [.backscale()]
#' to bring coefficients and predictions back to original scale.
#'
#' @inherit .backscale examples
#'
#' @keywords internal
#'
#' @rdname forescale
#'
#' @srrstats {RE2.3} *data are centred internally*
#' @srrstats {RE4.12} *function to transform input data, and inverse function*
#'
.forescale <- function(x, y = NULL, family = NULL, pars = NULL) {
  # --- check arguments ---
  #if (is.character(family)) family <- tolower(family)
  checkmate::assert_matrix(x = x, mode = "numeric", any.missing = FALSE,
                           min.rows = 1L, min.cols = 1L)
  if (is.null(family) == is.null(pars)) {
    stop('Expect either "family" or "pars".')
  }
  #families <- c("gaussian", "binomial", "poisson", "cox")
  #checkmate::assert_choice(x = family, choices = families, null.ok = TRUE)
  if (!is.null(pars)) {
    slots <- c("family", "mu.x", "sd.x", "mu.y", "sd.y")
    checkmate::assert_list(x = pars, len = 5L)
    checkmate::assert_names(x = names(pars), identical.to = slots)
    checkmate::assert_numeric(x = pars$mu.x, len = ncol(x))
    checkmate::assert_numeric(x = pars$sd.x, len = ncol(x), lower = 0.0)
    checkmate::assert_number(x = pars$mu.y)
    checkmate::assert_number(x = pars$sd.y, lower = 0.0)
  }
  #checkmate::assert_choice(x = c(family, pars$family), choices = families)
  family <- .validate_family(family = c(family, pars$family))
  # --- estimate parameters ---
  if (is.null(pars)) {
    pars <- list()
    pars$family <- family
    cond <- rep(x = TRUE, times = length(y))
    # scaling with uncensored observations only: cond <- y[, 2L] == 1L
    pars$mu.x <- colMeans(x = x[cond, ], na.rm = TRUE)
    pars$sd.x <- apply(X = x[cond, ],
                       MARGIN = 2L,
                       FUN = stats::sd, na.rm = TRUE)
    if (!is.null(y) && identical(family, "gaussian")) {
      pars$mu.y <- mean(y, na.rm = TRUE)
      pars$sd.y <- stats::sd(y, na.rm = TRUE)
    } else if (!is.null(y)) {
      pars$mu.y <- 0.0
      pars$sd.y <- 1.0
    }
  }
  # --- standardise variables ---
  x_scaled <- t((t(x) - pars$mu.x) / pars$sd.x)
  x_scaled[, pars$sd.x < .Machine$double.eps] <- 0.0
  if (!is.null(y) && identical(family, "gaussian")) {
    y_scaled <- (y - pars$mu.y) / pars$sd.y
  } else if (!is.null(y)) {
    y_scaled <- y
  } else {
    y_scaled <- NULL
  }
  list(x = x_scaled, y = y_scaled, family = family, pars = pars)
}

#' @title
#' Inverse standardisation
#'
#' @description
#' Transforms response variable back to original scale
#' or transforms coefficients for predictor variables and response variable
#' on original scales.
#'
#' @param pars
#' list with slots `mu.x` and `sd.x`
#' (\eqn{p}-dimensional vectors of means and standard deviations
#' of the predictor variables),
#' `mu.y` and `sd.y`
#' (mean and standard deviation of response variable for Gaussian family,
#' 0 and 1 for other families),
#' and `family`
#' (character string `"gaussian"`, `"binomial"`,
#' `"poisson"`, or `"cox"`)
#'
#' @param y
#' \eqn{n_1}-dimensional response vector
#' or response matrix with \eqn{n_1} rows and multiple columns
#' (for multiple values of the regularisation parameter),
#' or `NULL` (default)
#'
#' @param coef
#' \eqn{(1 + p)}-dimensional vector
#' containing the estimated intercept
#' and the estimated slopes,
#' or `NULL` (default)
#'
#' @details
#' This function is called by [predict.cv.corila()]
#' for the predicted values
#' and by [coef.cv.corila()]
#' for the estimated coefficients.
#'
#' @return
#' Returns a list with slots `y` or `coef`.
#'
#' @seealso
#' Use function [.forescale()] to standardise variables.
#'
#' @examples
#' \donttest{
#' \dontshow{
#' .forescale <- corila:::.forescale
#' .backscale <- corila:::.backscale
#' }
#' # simulate data
#' family <- "gaussian"
#' n0 <- 100L; n1 <- 50L; p <- 3L
#' n <- n0 + n1
#' fold <- rep(c(0L, 1L), times = c(n0, n1))
#' sd <- stats::rpois(n = p, lambda = 5.0)
#' x <- data.frame(x = sapply(X = sd,
#'                            FUN = function(x) stats::rnorm(n = n, sd = x)))
#' beta <- stats::rnorm(n = p)
#' eta <- as.matrix(x) %*% beta
#' if (identical(family, "gaussian")) {
#'   y <- stats::rnorm(n = n, mean = eta)
#' } else if (identical(family, "binomial")) {
#'   y <- stats::rbinom(n = n, size = 1L, prob = 1.0 / (1.0 + exp(-eta)))
#' } else if (identical(family, "poisson")) {
#'   y <- stats::rpois(n = n, lambda = exp(eta))
#' } else if (identical(family, "cox")) {
#'   time <- stats::rexp(n = n, rate = exp(eta))
#'   status <- stats::rbinom(n = n, size = 1L, prob = 0.5)
#'   y <- survival::Surv(time = time, event = status)
#' }
#'
#' # regression without standardisation
#' if (identical(family, "cox")) {
#'   lm1 <- survival::coxph(y[fold == 0L]~., data=x[fold == 0L, ])
#' } else {
#'   lm1 <- stats::glm(y[fold == 0L]~., data=x[fold == 0L, ], family=family)
#' }
#' coef1 <- stats::coef(lm1)
#' yhat1 <- predict(lm1, newdata = x[fold == 1L, ])
#'
#' # regression with standardisation
#' scale <- .forescale(x = as.matrix(x)[fold == 0L, ],
#'                     y = y[fold == 0L],
#'                     family = family)
#' if (identical(family, "cox")) {
#'   lm2 <- survival::coxph(scale$y~., data = data.frame(scale$x))
#' } else {
#'   lm2 <- stats::glm(scale$y~., data = data.frame(scale$x), family = family)
#' }
#' coef_temp <- stats::coef(lm2)
#' newx_temp <- .forescale(x = as.matrix(x)[fold == 1L, ],
#'                         pars = scale$pars)$x
#' yhat_temp <- predict(object = lm2, newdata = data.frame(newx_temp))
#' result <- .backscale(pars = scale$pars,
#'                      y = yhat_temp,
#'                      coef = coef_temp)
#' coef2 <- result$coef
#' yhat2 <- result$y
#'
#' # equality
#' all.equal(coef1, coef2, check.attributes = FALSE)
#' all.equal(yhat1, yhat2, check.attributes = FALSE)
#' \dontshow{
#' stopifnot(
#'  isTRUE(all.equal(coef1, coef2, check.attributes = FALSE)),
#'  isTRUE(all.equal(yhat1, yhat2, check.attributes = FALSE))
#' )
#' }
#' }
#'
#' @keywords internal
#'
#' @rdname backscale
#'
.backscale <- function(pars, y = NULL, coef = NULL) {
  # --- check arguments ---
  slots <- c("family", "mu.x", "sd.x", "mu.y", "sd.y")
  families <- c("gaussian", "binomial", "poisson", "cox")
  if (is.null(y) && is.null(coef)) {
    stop("Provide 'y' or 'coef'.")
  }
  checkmate::assert_list(x = pars, len = 5L)
  checkmate::assert_names(x = names(pars), identical.to = slots)
  checkmate::assert_choice(x = pars$family, choices = families)
  checkmate::assert_numeric(x = pars$mu.x, min.len = 1L)
  checkmate::assert_numeric(x = pars$sd.x, len = length(pars$mu.x), lower = 0.0)
  checkmate::assert_number(x = pars$mu.y)
  checkmate::assert_number(x = pars$sd.y, lower = 0.0)
  if (is.matrix(y)) {
    checkmate::assert_matrix(x = y, min.rows = 1L, min.cols = 1L,
                             any.missing = FALSE, null.ok = TRUE)
  } else {
    checkmate::assert_numeric(x = y, min.len = 1L, any.missing = FALSE,
                              null.ok = TRUE)
  }
  #checkmate::assert(
  #  checkmate::check_numeric(x = y, min.len = 1L, any.missing = FALSE),
  #  checkmate::check_matrix(x = y, min.rows = 1L, min.cols = 1L,
  #                           any.missing = FALSE),
  #  combine = "or"
  #)
  checkmate::assert_numeric(
    x = coef,
    len = length(pars$mu.x) + !identical(pars$family, "cox"),
    null.ok = TRUE
  )
  # --- transform target ---
  list <- list()
  if (!is.null(y) && identical(pars$family, "gaussian")) {
    list$y <- pars$mu.y + pars$sd.y * y
  } else if (!is.null(y)) {
    list$y <- y
  }
  # --- transform coefficients ---
  if (!is.null(coef)) {
    if (identical(pars$family, "cox")) {
      alpha <- NULL
      beta <- coef * ifelse(test = pars$sd.x < .Machine$double.eps,
                            yes = 0.0,
                            no = pars$sd.y / pars$sd.x)
    } else {
      factor <- ifelse(test = pars$sd.x < .Machine$double.eps,
                       yes = 0.0,
                       no = pars$mu.x / pars$sd.x)
      alpha <- pars$mu.y + pars$sd.y * (coef[1L] - sum(coef[-1L] * factor))
      beta <- coef[-1L] * ifelse(test = pars$sd.x < .Machine$double.eps,
                                 yes = 0.0,
                                 no = pars$sd.y / pars$sd.x)
    }
    list$coef <- c(alpha, beta)
  }
  list
}

#' @title
#' Fold identifiers
#'
#' @description
#' Splits observations into balanced and stratified folds.
#'
#' @inheritParams cv.corila y family nfolds
#'
#' @return
#' Returns an \eqn{n_0}-dimensional vector
#' with entries in \eqn{\{1, \ldots, }`nfolds`\eqn{\}}.
#'
#' @details
#' Randomly splits observations into balanced folds
#' (approximately the same number of observations per fold)
#' and stratified folds
#' (separate splitting for both classes in binomial family
#' or censored/uncensored observations in Cox model).
#'
#' @examples
#' \dontshow{.folds <- corila:::.folds}
#' # Gaussian and Poisson families
#' y <- stats::rnorm(n = 100L)
#' y <- stats::rpois(n = 100L, lambda = 4.0)
#' foldid <- .folds(y = y, family = "gaussian", nfolds = 10L)
#' table(foldid)
#'
#' # binomial family
#' y <- stats::rbinom(n = 100L, size = 1L, prob = 0.2)
#' foldid <- .folds(y = y, family = "binomial", nfolds = 10L)
#' table(y, foldid)
#'
#' \donttest{
#' # Cox model
#' time <- stats::rexp(n = 100L, rate = 5.0)
#' status <- stats::rbinom(n = 100L, size = 1L, prob = 0.2)
#' y <- survival::Surv(time = time, event = status)
#' foldid <- .folds(y = y, family = "cox", nfolds = 10L)
#' table(y[, "status"], foldid)
#' }
#'
#' @keywords internal
#'
#' @rdname folds
#'
.folds <- function(y, family, nfolds) {
  # --- check arguments ---
  #if (is.character(family)) family <- tolower(family)
  #checkmate::assert_choice(
  #  x = family,
  #  choices = c("gaussian", "binomial", "poisson", "cox")
  #)
  family <- .validate_family(family = family)
  y <- .validate_y(y = y, family = family, n = NULL, na_action = "error",
                   names = NULL)
  if (length(y) < 2L) stop("Require at least 2 observations.")
  checkmate::assert_int(x = nfolds, lower = 2L, upper = length(y))
  nfolds <- as.integer(round(nfolds))
  # --- set fold identifiers ---
  if (family %in% c("binomial", "logistic", "cox")) {
    if (family %in% c("binomial", "logistic")) {
      y <- as.integer(round(y))
    }
    if (identical(family, "cox")) {
      y <- y[, "status"]
    }
    foldid <- rep(x = NA, times = length(y))
    if (sum(y == 0L) == 1L) {
      foldid[y == 0L] <- 1L
    } else {
      foldid[y == 0L] <- sample(x = rep(x = seq_len(nfolds),
                                        length.out = sum(y == 0L)))
    }
    if (sum(y == 1L) == 1L) {
      foldid[y == 1L] <- nfolds
    } else {
      foldid[y == 1L] <- sample(x = rep(x = rev(seq_len(nfolds)),
                                        length.out = sum(y == 1L)))
    }
  } else {
    foldid <- sample(x = rep(x = sample(x = seq_len(nfolds)),
                             length.out = length(y)))
  }
  foldid
}

#' @title
#' Mean function
#'
#' @description
#' Transform the linear predictor to predicted values/probabilities.
#'
#' @inheritParams cv.corila family
#'
#' @param x
#' numeric vector of length \eqn{n}
#'
#' @return
#' Returns a numeric vector of length \eqn{n}.
#'
#' @examples
#' \dontshow{.mean_function <- corila:::.mean_function}
#' x <- rnorm(n = 10L)
#' .mean_function(x, family = "binomial")
#' .mean_function(x, family = "poisson")
#'
#' @keywords internal
#'
#' @rdname mean_function
#'
.mean_function <- function(x, family) {
  # --- check arguments ---
  #if (is.character(family)) family <- tolower(family)
  #support <- c("gaussian", "binomial", "poisson", "cox")
  #checkmate::assert_choice(x = family, choices = support)
  checkmate::assert_numeric(x = x, min.len = 1L)
  family <- .validate_family(family = family)
  # --- transform target ---
  if (family %in% c("gaussian", "cox")) {
    x
  } else if (identical(family, "binomial")) {
    1.0 / (1.0 + exp(-x))
  } else if (identical(family, "poisson")) {
    exp(x)
  }
}

#' @title
#' Deviance
#'
#' @description
#' Calculates the deviance.
#'
#' @inheritParams cv.corila y family
#'
#' @param y_hat
#' predicted response:
#' numeric vector of length \eqn{n},
#' with entries on the real range (`family="gaussian"` or `family="cox"`),
#' in the unit interval (`family="binomial"`),
#' or on the non-negative real range (`family="poisson"`)
#'
#' @return
#' Returns the deviance (a numeric scalar).
#'
#' @seealso
#' The function [deviance.cv.corila()]
#' extracts the deviance from a fitted model.
#'
#' @examples
#' \dontshow{.deviance <- corila:::.deviance}
#' n <- 10L
#'
#' y <- rnorm(n)
#' y_hat <- rnorm(n)
#' .deviance(y = y, y_hat = y_hat, family = "gaussian")
#'
#' y <- rbinom(n = n, size = 1L, prob = 0.5)
#' y_hat <- runif(n)
#' .deviance(y = y, y_hat = y_hat, family = "binomial")
#'
#' y <- rpois(n = n, lambda = 4.0)
#' y_hat <- rexp(n)
#' .deviance(y = y, y_hat = y_hat, family = "poisson")
#'
#' @keywords internal
#'
#' @rdname deviance
#'
.deviance <- function(y, y_hat, family) {
  # --- check arguments ---
  #if (is.character(family)) family <- tolower(family)
  #checkmate::assert_choice(
  #  x = family,
  #  choice = c("gaussian", "binomial", "poisson", "cox")
  #)
  family <- .validate_family(family = family)
  y <- .validate_y(y = y, family = family, n = NULL,
                   na_action = "complete_cases", names = NULL)
  y_hat <- .validate_y_hat(y_hat = y_hat, family = family, n = length(y))
  #if (length(y) != length(y_hat)) {
  #  stop("Arguments 'y' and 'y_hat' must have the same length.")
  #}
  # --- calculate deviance ---
  eps <- 1e-06
  if (identical(family, "gaussian")) {
    2.0 * mean((y - y_hat)^2.0)
  } else if (identical(family, "binomial")) {
    2.0 * mean(
      -y * log(pmax(y_hat, eps)) - (1.0 - y) * log(1.0 - pmin(y_hat, 1.0 - eps))
    )
  } else if (identical(family, "poisson")) {
    mean(
      2.0 * (ifelse(test = abs(y) < .Machine$double.eps,
                    yes = 0.0,
                    no = y * log(y / y_hat)) - y + y_hat)
    )
  } else if (identical(family, "cox")) {
    glmnet::coxnet.deviance(pred = log(y_hat), y = y)
  }
}
