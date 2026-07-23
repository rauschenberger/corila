#----- helpers -----

#' @title
#' Assertions
#'
#' @description
#' Check whether provided arguments satisfy expectations.
#'
#' @inheritParams cv.corila family
#'
#' @param x
#' scalar, vector (of length `dim`),
#' matrix (of dimensions `dim`),
#' or array (of dimensions `dim`) to be checked
#' - `type = "numeric"`: numeric
#' - `type = "integer"`: integer
#' - `type = "nominal"`: character
#' - `type = "logical"`: logical
#' - `family = "binomial"`: integers 0 or 1
#' - `family = "poisson"`: non-negative integers
#' - `family = "cox"`: object created with [survival::Surv]
#'
#' @param dim
#' vector of length 1, 2 or 3 containing positive integers
#' (minimum 1, maximum \eqn{100,000})
#' defining the dimensionality:
#' - scalar `x`: `dim = 1`
#' - vector `x` of length 100: `dim = 100`
#' - vector `x` of arbitrary length: `dim = Inf`
#' - matrix `x` with 100 rows: `dim = c(100, Inf)`
#' - matrix `x` of arbitrary dimensions: `dim = c(Inf, Inf)`
#' - array `x` of arbitrary dimensions: `dim = c(Inf, Inf, Inf)`
#'
#' @param type
#' character scalar `"numeric"` (default), `"integer"`,
#' `"nominal"`, or `"logical"`
#'
#' @param na.rm
#' logical scalar (or numeric 0/1):
#' - `na.rm=FALSE` (or `na.rm=0`): missing values are not allowed
#' - `na.rm=TRUE` (or `na.rm=1`): missing values are allowed
#'
#' @param support
#' character vector (only used for `type = "nominal"`),
#' (matching with `x` is case-insensitive)
#'
#' @param min
#' numeric scalar (not used for `type = "nominal"`)
#'
#' @param max
#' numeric scalar (not used for `type = "nominal"`)
#'
#' @details
#' This function is called by multiple function of the [corila-package].
#'
#' @return
#' Returns `NULL` invisibly, or throws an error.
#'
#' @seealso
#' The function [.validate()] verifies whether the main arguments
#' have compatible dimensions (number of samples and features).
#'
#' @examples
#' \dontshow{.assert <- corila:::.assert}
#' n <- 3L; p <- 4L
#' .assert(x = matrix(rnorm(n = n * p), nrow = n, ncol = p),
#'         dim = c(n, p),
#'         type = "numeric",
#'         family = "gaussian")
#' .assert(x = rpois(n = n, lambda = 4.0),
#'         dim = n,
#'         type = "integer",
#'         family = "poisson")
#' .assert(x = rbinom(n = n, size = 1L, prob = 0.5),
#'         dim = n,
#'         type = "integer",
#'         family = "binomial")
#' .assert(x = "a",
#'         dim = 1L,
#'         type = "nominal",
#'         support = letters)
#'
#' @keywords internal
#'
#' @srrstats {G2.0} *implements assertions on lengths of inputs*
#' @srrstats {G2.1} *rejects unexpected input types*
#' @srrstats {G2.2} *rejects multivariate input if expecting univariate input*
#' @srrstats {G2.15} *rejects missing values by default*
#' @srrstats {G2.3a} *rejects unexpected values*
#' @srrstats {G2.13} *checks for missing data*
#' @srrstats {G5.2a} *messages are unique*
#' @srrstats {RE1.4} *tests assumptions for input data*
#'
.assert <- function(x, type = "numeric", dim = 1L, na.rm = FALSE,
                    support = NULL, family = NULL, min = -Inf, max = Inf) {
  eps <- 1e-06
  if (is.null(x)) return(invisible(NULL))
  stopifnot(
    "require argument 'type' to be a character scalar" =
      length(type) == 1L && is.character(type) && !is.na(type),
    "require argument `type` to be inside support" =
      tolower(type) %in% c("numeric", "integer", "nominal", "logical"),
    "require argument 'support' to be a character vector" =
      is.null(support) || (is.character(support) && is.atomic(support)),
    "require argument support = NULL unless argument type = 'nominal'" =
      tolower(type) == "nominal" || is.null(support),
    "require argument 'family' to be a character scalar" =
      is.null(family) ||
      (length(family) == 1L && is.character(family) && !is.na(family)),
    "require argument 'family' to be inside support" =
      is.null(family) ||
      tolower(family) %in% c("gaussian", "binomial", "poisson", "cox"),
    "require argument 'dim' to be an integer vector" =
      is.atomic(dim) && all(dim > 0L) &&
      all(abs(dim - round(dim)) < eps | is.infinite(dim)) && !anyNA(dim),
    "require argument 'dim' to have length 1, 2, or 3" =
      length(dim) %in% c(1L, 2L, 3L),
    "require argument 'na.rm' to be a logical scalar" =
      length(na.rm) == 1L &&
      (is.logical(na.rm) || (abs(na.rm - 1L) < eps | abs(na.rm - 0L) < eps)) &&
      !is.na(na.rm),
    "require argument 'min' to be a numeric scalar" =
      length(min) == 1L && is.numeric(min) && !is.na(min),
    "require argument 'max' to be a numeric scalar" =
      length(max) == 1L && is.numeric(max) && !is.na(max)
  )
  type <- tolower(type)
  if (!is.null(family)) family <- tolower(family)
  if (!is.null(support)) support <- tolower(support)
  if (type == "nominal") x <- tolower(x)
  na.rm <- as.logical(na.rm)
  stopifnot(
    "expected vector"  =
      length(dim) != 1L || (is.atomic(x) && is.null(dim(x))) ||
      inherits(x = x, what = "Surv"),
    "expected matrix" =
      length(dim) != 2L || is.matrix(x),
    "expected array" =
      length(dim) <= 2L || is.array(x),
    "expected vector with other length" =
      length(dim) != 1L || dim == Inf || abs(length(x) - dim) < eps,
    "expected matrix/array with other number of dimensions" =
      length(dim) == 1L || length(dim) == length(dim(x)),
    "expected matrix/array with other dimensions" =
      length(dim) == 1L || all((dim == Inf | abs(dim(x) - dim) < eps)),
    "expected no missing values" =
      na.rm || !anyNA(x),
    "expected numeric values" =
      !type %in% c("numeric", "integer") ||
      is.numeric(x) || (na.rm && all(is.na(x))),
    "expected integer values" =
      type != "integer" || all(abs(x - round(x)) < eps, na.rm = TRUE),
    "expected nominal values" =
      type != "nominal" || is.character(x),
    "expected logical values" =
      type != "logical" || is.logical(x),
    "expected values inside support" =
      type != "nominal" || is.null(support) ||
      all(x[!is.na(x)] %in% support),
    "expected values greater than or equal to minimum" =
      type == "nominal" || min == -Inf || all(x >= min - eps, na.rm = TRUE),
    "expected values less than or equal to maximum" =
      type == "nominal" || max == Inf || all(x <= max + eps, na.rm = TRUE),
    "expected binary variable" =
      is.null(family) || family != "binomial" ||
      all((x > -eps & x < eps) | (x > 1.0 - eps & x < 1.0 + eps), na.rm = TRUE),
    "expected count variable" =
      is.null(family) || family != "poisson" ||
      (all(abs(x - round(x)) < eps, na.rm = TRUE) && all(x >= -eps)),
    "expected survival object" =
      is.null(family) || family != "cox" || inherits(x = x, what = "Surv")
  )
  invisible(NULL)
}

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
#' @srrstats {RE2.3} *data are centred internally*
#' @srrstats {RE4.12} *function to transform input data, and inverse function*
#'
.forescale <- function(x, y = NULL, family = NULL, pars = NULL) {
  # --- check arguments ---
  if (is.character(family)) family <- tolower(family)
  checkmate::assert_matrix(x = x, mode = "numeric", any.missing = FALSE,
                           min.rows = 1L, min.cols = 1L)
  if (is.null(family) == is.null(pars)) {
    stop('Expect either "family" or "pars".')
  }
  families <- c("gaussian", "binomial", "poisson", "cox")
  slots <- c("family", "mu.x", "sd.x", "mu.y", "sd.y")
  checkmate::assert_choice(x = family, choices = families, null.ok = TRUE)
  checkmate::assert_list(x = pars, len = 5L, null.ok = TRUE)
  if (!is.null(pars)) {
    checkmate::assert_names(x = names(pars), identical.to = slots)
    checkmate::assert_numeric(x = pars$mu.x, len = ncol(x))
    checkmate::assert_numeric(x = pars$sd.x, len = ncol(x), lower = 0.0)
    checkmate::assert_number(x = pars$mu.y)
    checkmate::assert_number(x = pars$sd.y, lower = 0.0)
  }
  checkmate::assert_choice(x = c(family, pars$family), choices = families)
  # --- estimate parameters ---
  if (is.null(family)) {
    family <- pars$family
  } else {
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
.folds <- function(y, family, nfolds) {
  # --- check arguments ---
  if (is.character(family)) family <- tolower(family)
  checkmate::assert_choice(
    x = family,
    choices = c("gaussian", "binomial", "poisson", "cox")
  )
  y <- .validate_response(y = y, family = family)
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
.mean_function <- function(x, family) {
  # --- check arguments ---
  if (is.character(family)) family <- tolower(family)
  support <- c("gaussian", "binomial", "poisson", "cox")
  checkmate::assert_numeric(x = x, min.len = 1L)
  checkmate::assert_choice(x = family, choices = support)
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
.deviance <- function(y, y_hat, family) {
  # --- check arguments ---
  if (is.character(family)) family <- tolower(family)
  checkmate::assert_choice(
    x = family,
    choice = c("gaussian", "binomial", "poisson", "cox")
  )
  y <- .validate_response(y = y, family = family)
  y_hat <- .validate_fitted(y_hat = y_hat, family = family)
  if (length(y) != length(y_hat)) {
    stop("Arguments 'y' and 'y_hat' must have the same length.")
  }
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

.validate_response <- function(y, family, ...) {
  eps <- 1e-06
  checkmate::assert_choice(
    x = family, choices = c("gaussian", "binomial", "poisson", "cox")
  )
  checkmate::assert_numeric(
    x = y, min.len = 1L, all.missing = FALSE, ...
  )
  if(identical(family, "cox") != inherits(y, "Surv")){
    stop("Expects survival response if and only if Cox model.")
  }
  if (identical(family, "binomial")) {
    checkmate::assert_integerish(x = y, lower = - eps, upper = 1.0 + eps)
    as.integer(round(y))
  } else if (identical(family, "poisson")) {
    checkmate::assert_integerish(x = y, lower = - eps)
    as.integer(round(y))
  } else {
    y
  }
}

.validate_fitted <- function(y_hat, family, ...) {
  eps <- 1e-06
  checkmate::assert_choice(
    x = family, choices = c("gaussian", "binomial", "poisson", "cox")
  )
  checkmate::assert_numeric(
    x = y_hat, min.len = 1L, any.missing = FALSE, ...
  )
  if (identical(family, "binomial")) {
    checkmate::assert_numeric(x = y_hat, lower = - eps, upper = 1.0 + eps)
    pmax(0, pmin(y_hat, 1))
  } else if (identical(family, "poisson")) {
    checkmate::assert_numeric(x = y_hat, lower = - eps)
    pmax(0, y_hat)
  } else {
    y_hat
  }
}

# .validate_foldid <- function(foldid, y, family) {
#   checkmate::assert_numeric()
#   checkmate::assert_integerish(x = foldid, lower = 1L, upper = length(foldid),
#                                min.len = 1L, null.ok = TRUE)
#   if (is.null(foldid)) {
#     nfolds <- as.integer(round(nfolds))
#   } else {
#     foldid <- as.integer(round(foldid))
#     nfolds <- max(foldid)
#  }
#
#  list(n = nfolds, id = foldid)
# }

# .validate_group <- function(group) {
#   check all three option
# }

# .validate_family <- function(family, poisson = TRUE) {
#   tolower
#   check wether inside support
# }

# in function residuals: rename y_fit to y_hat and y_obs to y
