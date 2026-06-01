#----- helpers -----

#' @title
#' Assertions
#'
#' @description
#' Check whether provided arguments satisfy expectations.
#'
#' @param x
#' scalar, vector, matrix, or array to be checked
#'
#' @param dim
#' dimensionality:
#' \code{dim = 1} for a scalar,
#' \code{dim = Inf} for a vector of arbitrary length,
#' \code{dim = c(Inf, Inf)} for a matrix of arbitrary dimensions,
#' \code{dim = c(Inf, Inf, Inf)} for an array of arbitrary dimensions,
#' \code{dim = 100} for a vector of length \code{100},
#' \code{dim = c(Inf, 100)} for a matrix with \eqn{100} columns, etc.
#'
#' @param type
#' character \code{"numeric"}, \code{"integer"},
#' \code{"nominal"}, or \code{"logical"}
#'
#' @param na.rm
#' logical
#'
#' @param support
#' character vector (only used for \code{type = "nominal"})
#'
#' @param min
#' numerical value (not used for \code{type = "nominal"})
#'
#' @param max
#' numerical value (not used for \code{type = "nominal"})
#'
#' @seealso \code{\link{.validate}()}
#'
#' @examples
#' corila:::.check(x = NULL)
#' corila:::.check(x = rnorm(1), type = "numeric")
#' corila:::.check(x = "A", type = "nominal", support = LETTERS)
#'
#' @keywords internal
#'
.check <- function(x, type, dim = 1, na.rm = FALSE,
                   support = NULL, min = -Inf, max = Inf) {
  if (is.null(x)) {
    return(invisible(NULL))
  }
  type <- match.arg(arg = type,
                    choices = c("numeric", "integer", "nominal", "logical"))
  stopifnot(
    "invalid argument 'support'" =
      is.null(support) || is.character(support),
    "invalid argument 'support'" =
      type == "nominal" || is.null(support),
    "expected vector"  =
      length(dim) != 1 || is.vector(x) || inherits(x = x, what = "Surv"),
    "expected matrix" =
      length(dim) != 2 || is.matrix(x),
    "expected array" =
      length(dim) <= 2 || is.array(x),
    "expected vector with other length" =
      length(dim) != 1 || dim == Inf || length(x) == dim,
    "expected matrix/array with other dimensions" =
      length(dim) == 1 || length(dim) == length(dim(x)),
    "expected matrix/array with other dimensions" =
      length(dim) == 1 || all((dim == Inf | dim(x) == dim)),
    "expected no missing values" =
      na.rm || !anyNA(x),
    "expected numeric values" =
      !type %in% c("numeric", "integer") || is.numeric(x),
    "expected integer values" =
      type != "integer" || all(x %% 1 == 0, na.rm = TRUE),
    "expected nominal values" =
      type != "nominal" || is.character(x),
    "expected logical values" =
      type != "logical" || is.logical(x),
    "expected values inside support" =
      type != "nominal" || is.null(support) || all(x[!is.na(x)] %in% support),
    "expected values greater than or equal to minimum" =
      type == "nominal" || min == -Inf || all(x >= min, na.rm = TRUE),
    "expected values less than or equal to maximum" =
      type == "nominal" || max == Inf || all(x <= max, na.rm = TRUE)
  )
}

# function .validate: add check on groups!

#' @title
#' Validate data
#'
#' @description
#' Validates the predictor matrix x and the outcome vector y.
#'
#' @inheritParams corila
#'
#' @return
#' Returns \code{NULL} or an error message.
#'
#' @seealso \code{\link{.check}()}
#'
#' @examples
#' n <- 10
#' p <- 5
#' x <- matrix(rnorm(n * p), nrow = n, ncol = p)
#' y <- rnorm(n)
#' corila:::.validate(x = x, y = y, family = "gaussian")
#'
#' @keywords internal
#'
.validate <- function(x, y, family) {
  if (!is.character(family) || length(family) != 1) {
    stop("Argument 'family' must be a character string.")
  }
  #family <- match.arg(
  #arg = tolower(family),
  #choices = c("gaussian", "linear", "binomial", "logistic", "poisson", "cox"))
  #)
  family <- switch(family, linear = "gaussian", logistic = "binomial", family)
  if (!is.matrix(x) || !is.numeric(x)) {
    stop("Argument 'x' must be a numeric matrix.")
  }
  if (any(is.na(x))) {
    stop("Argument 'x' must not contain any missing values.")
  }
  if (any(is.na(y))) {
    stop("Argument 'y' must not contain any missing values.")
  }
  #cond_vector <- is.vector(y) && is.numeric(y)
  #cond_matrix <- is.matrix(y) && ncol(y) == 1
  cond_vector <- (is.vector(y) && is.atomic(y)) # || inherits(y, "Surv")
  cond_matrix <- is.matrix(y) && ncol(y) == 1 && is.numeric(y)
  if (!(identical(family, "cox") || cond_vector || cond_matrix)) {
    stop("Argument 'y' must be a numeric vector.")
  }
  if (nrow(x) != length(y)) {
    stop("For each observation, matrix 'x' must have one row,
         and vector 'y' must have one entry.")
  }
  # --- outcome vector ---
  if (identical(family, "gaussian")) {
    if (all(y %in% c(0, 1)) || all(y %in% c(-1, 1))) {
      stop("Gaussian family requires a numerical outcome.")
    }
  } else if (identical(family, "binomial")) {
    if (!all(y %in% c(0, 1))) {
      stop("Binomial family requires a binary outcome.")
    }
  } else if (identical(family, "poisson")) {
    if (any(y %% 1 != 0)) {
      stop("Poisson family requires a count outcome.")
    }
  } else if (identical(family, "cox")) {
    if (!inherits(x = y, what = "Surv")) {
      stop("Cox model requires a survival outcome.")
    }
  } else {
    stop("Invalid value for argument 'family'.")
  }
  # add tests for argument group
  invisible(NULL)
}

#' @title
#' Standardisation
#'
#' @description
#' Transforming variables to mean 0 and variance 1.
#'
#' @inheritParams corila
#'
#' @param y
#' \eqn{n_0}-dimensional response vector
#' (only required if \code{family="gaussian"})
#' or \code{NULL}
#'
#' @param family
#' character string \code{"gaussian"}, \code{"binomial"},
#' \code{"poisson"}, or \code{"cox"};
#' or \code{NULL} (if \code{pars} is provided)
#'
#' @param pars
#' list as defined in section \emph{Value},
#' or \code{NULL} (if \code{family} is provided)
#'
#' @return
#' \itemize{
#' \item standardised \eqn{n_0 \times p} predictor matrix \eqn{x}
#' \item standardised \eqn{n_0}-dimensional response vector \eqn{y}
#' (only if \eqn{y} is provided and \code{family = "gaussian"}
#' or \code{pars$family = "gaussian"}; otherwise output equals input)
#' \item character string \code{family} indicates the model (\code{"gaussian"},
#' \code{"binomial"}, \code{"poisson"}, or \code{"cox"}),
#' determined by argument \code{family} or \code{pars$family}
#' \item list \code{pars} with slots \code{mu.x} and \code{sd.x}
#' (\eqn{p}-dimensional vectors of means and standard deviations
#' of the predictor variables),
#' and \code{mu.y} and \code{sd.y}
#' (mean and standard deviation of response variable for Gaussian family,
#' 0 and 1 for other families)
#' }
#'
#' @seealso Use function \code{\link{.backscale}()}
#' to bring coefficients and predictions back to original scale.
#'
#' @inherit .backscale examples
#'
#' @keywords internal
#'
.forescale <- function(x, y = NULL, family = NULL, pars = NULL) {
  # --- check arguments ---
  families <- c("gaussian", "binomial", "poisson", "cox")
  slots <- c("family", "sd.x", "mu.x", "sd.y", "mu.y")
  .check(x = x, type = "numeric", dim = c(Inf, Inf))
  .check(x = y, type = "numeric", dim = nrow(x))
  if (is.null(family) == is.null(pars)) {
    stop('Expect either "family" or "pars".')
  }
  .check(x = family, type = "nominal", support = families)
  .check(x = names(pars), type = "nominal", dim = length(slots),
         support = slots)
  .check(x = pars$family, type = "nominal", support = families)
  .check(x = pars$mu.x, type = "numeric", dim = ncol(x))
  .check(x = pars$sd.x, type = "numeric", dim = ncol(x), min = 0)
  .check(x = pars$mu.y, type = "numeric")
  .check(x = pars$sd.y, type = "numeric", min = 0)
  # --- estimate parameters ---
  if (is.null(family)) {
    family <- pars$family
  } else {
    pars <- list()
    pars$family <- family
    #if (identical(family, "cox")) {
    #  cond <- y[, 2] == 1
    #} else {
    cond <- rep(x = TRUE, times = length(y))
    #}
    pars$mu.x <- apply(X = x[cond, ],
                       MARGIN = 2,
                       FUN = base::mean, na.rm = TRUE)
    pars$sd.x <- apply(X = x[cond, ],
                       MARGIN = 2,
                       FUN = stats::sd, na.rm = TRUE)
    if (!is.null(y) && identical(family, "gaussian")) {
      pars$mu.y <- mean(y, na.rm = TRUE)
      pars$sd.y <- stats::sd(y, na.rm = TRUE)
    } else if (!is.null(y)) {
      pars$mu.y <- 0
      pars$sd.y <- 1
    }
  }
  # --- standardise variables ---
  x_scaled <- t((t(x) - pars$mu.x) / pars$sd.x)
  x_scaled[, pars$sd.x == 0] <- 0
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
#' Inverse Standardisation
#'
#' @description
#' Transforms response variable back to original scale
#' or transforms coefficients for predictor variables and response variable
#' on original scales.
#'
#' @inheritParams .forescale
#'
#' @param y
#' \eqn{n_1}-dimensional response vector
#' or response matrix with \eqn{n_1} rows and multiple columns
#' (for multiple values of the regularisation parameter)
#'
#' @param coef
#' \eqn{(1 + p)-dimensional vector}
#' containing the estimated intercept
#' and the estimated slopes or \code{NULL} (default)
#'
#' @return
#' Returns a list with slots \code{y_original} or \code{coef}.
#'
#' @seealso \code{\link{.forescale}()}
#'
#' @examples
#' \donttest{
#' # simulate data
#' family <- "gaussian"
#' n0 <- 100; n1 <- 50; p <- 3
#' n <- n0 + n1
#' fold <- rep(c(0, 1), times = c(n0, n1))
#' sd <- stats::rpois(n = p, lambda = 5)
#' x <- data.frame(x = sapply(X = sd,
#'                            FUN = function(x) stats::rnorm(n = n, sd = x)))
#' beta <- stats::rnorm(n = p)
#' eta <- as.matrix(x) %*% beta
#' if (identical(family, "gaussian")) {
#'   y <- stats::rnorm(n = n, mean = eta)
#' } else if (identical(family, "binomial")) {
#'   y <- stats::rbinom(n = n, size = 1, prob = 1 / (1 + exp(-eta)))
#' } else if (identical(family, "poisson")) {
#'   y <- stats::rpois(n = n, lambda = exp(eta))
#' } else if (identical(family, "cox")) {
#'   time <- stats::rexp(n = n, rate = exp(eta))
#'   status <- stats::rbinom(n = n, prob = 0.5, size = 1)
#'   y <- survival::Surv(time = time, event = status)
#' }
#'
#' # regression without standardisation
#' if (identical(family, "cox")) {
#'   lm1 <- survival::coxph(y[fold == 0]~., data=x[fold == 0, ])
#' } else {
#'   lm1 <- stats::glm(y[fold == 0]~., data=x[fold == 0, ], family=family)
#' }
#' coef1 <- stats::coef(lm1)
#' yhat1 <- predict(lm1, newdata = x[fold == 1, ])
#'
#' # regression with standardisation
#' scale <- corila:::.forescale(x = as.matrix(x)[fold == 0, ],
#'                    y = y[fold == 0],
#'                    family = family)
#' if (identical(family, "cox")) {
#'   lm2 <- survival::coxph(scale$y~., data = data.frame(scale$x))
#' } else {
#'   lm2 <- stats::glm(scale$y~., data = data.frame(scale$x), family = family)
#' }
#' coef_temp <- stats::coef(lm2)
#' newx_temp <- .forescale(x = as.matrix(x)[fold == 1, ], pars = scale$pars)$x
#' yhat_temp <- predict(object = lm2, newdata = data.frame(newx_temp))
#' result <- corila:::.backscale(pars = scale$pars,
#'                               y = yhat_temp,
#'                               coef = coef_temp)
#' coef2 <- result$coef
#' yhat2 <- result$y_original
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
  slots <- c("family", "sd.x", "mu.x", "sd.y", "mu.y")
  .check(x = names(pars), type = "nominal", dim = length(slots),
         support = slots)
  families <- c("gaussian", "binomial", "poisson", "cox")
  .check(x = pars$family, type = "nominal", support = families)
  .check(x = pars$mu.x, type = "numeric", dim = Inf)
  .check(x = pars$sd.x, type = "numeric", dim = length(pars$mu.x), min = 0)
  .check(x = pars$mu.y, type = "numeric")
  .check(x = pars$sd.y, type = "numeric", min = 0)
  dim <- rep(x = Inf, times = 1 + is.matrix(y))
  .check(x = y, type = "numeric", dim = dim)
  dim <- length(pars$mu.x) + !identical(pars$family, "cox")
  .check(x = coef, type = "numeric", dim = dim)
  # --- transform target ---
  list <- list()
  if (!is.null(y) && identical(pars$family, "gaussian")) {
    list$y_original <- pars$mu.y + pars$sd.y * y
  } else if (!is.null(y)) {
    list$y_original <- y
  }
  # --- transform coefficients ---
  if (!is.null(coef)) {
    if (identical(pars$family, "cox")) {
      alpha <- NULL
      beta <- coef * ifelse(test = pars$sd.x == 0,
                            yes = 0,
                            no = pars$sd.y / pars$sd.x)
    } else {
      factor <- ifelse(test = pars$sd.x == 0,
                       yes = 0,
                       no = pars$mu.x / pars$sd.x)
      alpha <- pars$mu.y + pars$sd.y * (coef[1] - sum(coef[-1] * factor))
      beta <- coef[-1] * ifelse(test = pars$sd.x == 0,
                                yes = 0,
                                no = pars$sd.y / pars$sd.x)
    }
    list$coef <- c(alpha, beta)
  }
  list
}

#' @title
#' Fold Identifiers
#'
#' @description
#' Splits observations into balanced and stratified folds
#'
#' @inheritParams cv.corila
#'
#' @return
#' Returns an \eqn{n_1}-dimensional vector
#' with entries \eqn{\{1, \ldots, }\code{nfolds}\eqn{\}}
#'
#' @details
#' Randomly splits observations into balanced folds
#' (approximately the same number of observations per fold)
#' and stratified folds
#' (separate splitting for both classes in binomial family
#' or censored/uncensored observations in Cox model).
#'
#' @examples
#' # Gaussian and Poisson families
#' y <- stats::rnorm(n = 100)
#' y <- stats::rpois(n = 100, lambda = 4)
#' foldid <- corila:::.folds(y = y, family = "gaussian", nfolds = 10)
#' table(foldid)
#'
#' # binomial family
#' y <- stats::rbinom(n = 100, prob = 0.2, size = 1)
#' foldid <- corila:::.folds(y = y, family = "binomial", nfolds = 10)
#' table(y, foldid)
#'
#' \donttest{
#' # Cox model
#' time <- stats::rexp(n = 100, rate = 5)
#' status <- stats::rbinom(n = 100, prob = 0.2, size = 1)
#' y <- survival::Surv(time = time, event = status)
#' foldid <- corila:::.folds(y = y, family = "cox", nfolds = 10)
#' table(y[, "status"], foldid)
#' }
#'
#' @keywords internal
#'
.folds <- function(y, family, nfolds) {
  # --- check arguments ---
  .check(x = y, type = "numeric", dim = Inf)
  support <- c("gaussian", "linear", "binomial", "logistic", "poisson", "cox")
  .check(x = family, type = "nominal", support = support)
  #if(identical(family, "cox") && !inherits(y, "Surv")){
  #  stop("Require object of class 'Surv'.")
  #}
  .check(x = nfolds, type = "integer", min = 2, max = length(y))
  # --- set fold identifiers ---
  if (family %in% c("binomial", "logistic", "cox")) {
    if (identical(family, "cox")) {
      y <- y[, "status"]
    }
    foldid <- rep(x = NA, times = length(y))
    foldid[y == 0] <- sample(x = rep(x = sample(seq_len(nfolds)),
                                     length.out = sum(y == 0)))
    foldid[y == 1] <- sample(x = rep(x = sample(seq_len(nfolds)),
                                     length.out = sum(y == 1)))
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
#' @param x
#' numeric vector
#'
#' @param family
#' character
#'
#' @examples
#' x <- rnorm(10)
#' corila:::.mean_function(x, family = "binomial")
#' corila:::.mean_function(x, family = "poisson")
#'
#' @keywords internal
#'
.mean_function <- function(x, family) {
  # --- check arguments ---
  support <- c("gaussian", "binomial", "poisson", "cox")
  .check(x = x, type = "numeric", dim = Inf)
  .check(x = family, type = "nominal", support = support)
  # --- transform target ---
  if (family %in% c("gaussian", "cox")) {
    x
  } else if (identical(family, "binomial")) {
    1 / (1 + exp(-x))
  } else if (identical(family, "poisson")) {
    exp(x)
  } else {
    stop("Family not implemented.")
  }
}

#' @title
#' Deviance
#'
#' @description
#' Calculates the deviance.
#'
#' @param y
#' response:
#' numeric vector of length \code{n}
#'
#' @param y_hat
#' predicted response:
#' numeric vector of length \code{n}
#'
#' @param family
#' character
#'
#' @examples
#' n <- 10
#'
#' y <- rnorm(n)
#' y_hat <- rnorm(n)
#' corila:::.deviance(y = y , y_hat = y_hat, family = "gaussian")
#'
#' y <- rbinom(n = n, size = 1, prob = 0.5)
#' y_hat <- runif(n)
#' corila:::.deviance(y = y , y_hat = y_hat, family = "binomial")
#'
#' y <- rpois(n = n, lambda = 4)
#' y_hat <- rexp(n)
#' corila:::.deviance(y = y , y_hat = y_hat, family = "poisson")
#'
#' @keywords internal
#'
.deviance <- function(y, y_hat, family) {
  # --- check arguments ---
  support <- c("gaussian", "binomial", "poisson", "cox")
  .check(x = family, type = "nominal", support = support)
  if (identical(family, "binomial")) {
    .check(x = y, type = "integer", dim = Inf, min = 0, max = 1)
    .check(x = y_hat, type = "numeric", dim = length(y), min = 0, max = 1)
  } else if (identical(family, "poisson")) {
    .check(x = y, type = "integer", dim = Inf, min = 0)
    .check(x = y_hat, type = "numeric", dim = length(y), min = 0)
  } else {
    .check(x = y, type = "numeric", dim = Inf)
    .check(x = y_hat, type = "numeric", dim = length(y))
  }
  # --- calculate deviance ---
  eps <- 1e-06
  if (identical(family, "gaussian")) {
    mean((y - y_hat)^2)
  } else if (identical(family, "binomial")) {
    mean(
      -y * log(pmax(y_hat, eps)) - (1 - y) * log(1 - pmin(y_hat, 1 - eps))
    )
  } else if (identical(family, "poisson")) {
    mean(2 * (ifelse(y == 0, 0, y * log(y / y_hat)) - y + y_hat))
  } else if (identical(family, "cox")) {
    glmnet::coxnet.deviance(pred = y_hat, y = y)
  } else {
    stop(paste("Family", family, "not implemented."))
  }
}
