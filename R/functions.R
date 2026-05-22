# This file contains the functions of the R package "corila".

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
#' @examples
#' .check(x = NULL)
#' .check(x = rnorm(1), type = "numeric")
#' .check(x = "A", type = "nominal", support = LETTERS)
#'
#' @keywords internal
#'
#' @export
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

# add check on groups!
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
#' @export
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
#' scale <- .forescale(x = as.matrix(x)[fold == 0, ],
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
#' result <- .backscale(pars = scale$pars, y = yhat_temp, coef = coef_temp)
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
#' @export
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
#' foldid <- .folds(y = y, family = "gaussian", nfolds = 10)
#' table(foldid)
#'
#' # binomial family
#' y <- stats::rbinom(n = 100, prob = 0.2, size = 1)
#' foldid <- .folds(y = y, family = "binomial", nfolds = 10)
#' table(y, foldid)
#'
#' \donttest{
#' # Cox model
#' time <- stats::rexp(n = 100, rate = 5)
#' status <- stats::rbinom(n = 100, prob = 0.2, size = 1)
#' y <- survival::Surv(time = time, event = status)
#' foldid <- .folds(y = y, family = "cox", nfolds = 10)
#' table(y[, "status"], foldid)
#' }
#'
#' @keywords internal
#'
#' @export
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

#' @title Mean function
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
#' .mean_function(x, family = "binomial")
#' .mean_function(x, family = "poisson")
#'
#' @keywords internal
#'
#' @export
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

#' @title Deviance
#'
#' @description
#' Calculate the deviance
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
#' .deviance(y = y , y_hat = y_hat, family = "gaussian")
#'
#' y <- rbinom(n = n, size = 1, prob = 0.5)
#' y_hat <- runif(n)
#' .deviance(y = y , y_hat = y_hat, family = "binomial")
#'
#' y <- rpois(n = n, lambda = 4)
#' y_hat <- rexp(n)
#' .deviance(y = y , y_hat = y_hat, family = "poisson")
#'
#' @keywords internal
#'
#' @export
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

#----- group-ridge -----

#' @title
#' Multi-Penalty Ridge Regression
#'
#' @description
#' Fits multi-penalty ridge regression
#' (tuning regularisation hyperparameters
#' and estimating regression coefficients).
#'
#' @param x
#' predictors:
#' \eqn{n \times p} matrix,
#' or list of length \eqn{q} of \eqn{n \times p_k} matrices,
#' with \eqn{k} in \eqn{\{1, \ldots, q\}}.
#'
#' @param y
#' response:
#' \eqn{n}-dimensional vector
#'
#' @param z
#' groups:
#' \eqn{p}-dimensional vector with entries in \eqn{\{1, \ldots, q\}}
#' (if \code{x} is a matrix),
#' or \code{NULL}
#' (if \code{x} is a list of matrices)
#'
#' @param family
#' character \code{"linear"} (or \code{"gaussian"}),
#' \code{"logistic"} (or \code{"binomial"}),
#' or \code{"cox"}
#'
#' @param penalties
#' \eqn{q}-dimensional vector of penalty parameters,
#' or \code{NULL} (cross-validation)
#'
#' @inheritParams corila
#'
#' @inherit corila details
#'
#' @references
#' \href{https://orcid.org/0000-0003-4780-8472}{Mark A. van de Wiel},
#' \href{https://orcid.org/0000-0001-7715-1446}{Mirrelijn M. van Nee},
#' and
#' \href{https://orcid.org/0000-0001-6498-4801}{Armin Rauschenberger}
#' (2021).
#' "Fast cross-validation for multi-penalty high-dimensional ridge regression"
#' \emph{Journal of Computational and Graphical Statistics}
#' 30(4):835-847.
#' \href{https://doi.org/10.1080/10618600.2021.1904962}{doi: 10.1080/10618600.2021.1904962}. # nolint: line_length_linter.
#'
#' @return
#' Returns an object of class \code{"multiridge"},
#' a list with the following slots:
#' \itemize{
#' \item slots from \code{\link[multiridge]{IWLSridge}()} or
#' \code{\link[multiridge]{IWLSCoxridge}()}
#' \item character \code{family}
#' with value \code{"gaussian"} (also for \code{"linear"}),
#' \code{"binomial"} (also for \code{"logistic"}),
#' \code{"poisson"}, or \code{"cox"}
#' \item \eqn{q}-dimensional vector \code{penalties}
#' containing optimised regularisation hyperparameters
#' (one for each variable group)
#' \item list \code{datablocks}
#' with \eqn{q} slots (one for each variable group),
#' each containing an \eqn{n_0 \times p_k} matrix,
#' where \eqn{k \in \{1, \ldots, q\}}
#' \item \eqn{p}-dimensional group vector \code{z} (see argument)
#' \item list \code{pars} with slots \code{family} (see above),
#' the \eqn{n_0}-dimensional vectors \code{mu.x} and \code{sd.x}
#' and the scalars \code{mu.y} and \code{sd.y}
#' }
#'
#' @seealso
#' Extract coefficients with \code{\link[=coef.multiridge]{coef}()}
#' or make predictions with \code{\link[=predict.multiridge]{predict}()}.
#' Use \code{\link{cv.corila}()} to estimate sparse models.
#'
#' This wrapper function calls various functions from the
#' \code{\link[multiridge]{multiridge-package}},
#' namely
#' \code{\link[multiridge]{createXXblocks}()},
#' \code{\link[multiridge]{fastCV2}()},
#' \code{\link[multiridge]{CVfolds}()},
#' \code{\link[multiridge]{optLambdasWrap}()},
#' \code{\link[multiridge]{SigmaFromBlocks}()},
#' \code{\link[multiridge]{IWLSridge}()}, and
#' \code{\link[multiridge]{IWLSCoxridge}()}.
#'
#' @examples
#' # minimal example
#' n <- 50; p <- 20; q <- 5
#' x <- matrix(rnorm(n * p), nrow = n , ncol = p)
#' y <- rnorm(n)
#' z <- rep(seq_len(q), length.out = p)
#' multiridge(x = x, y = y, z = z)
#'
#' \donttest{
#' # simulation
#' set.seed(1)
#' n0 <- 100
#' n1 <- 10000
#' n <- n0 + n1
#' p <- c(100, 50)
#' z <- rep(x = seq_along(p), times = p)
#' x <- sapply(X = z, FUN = function(x) stats::rnorm(n = n, sd = x))
#' beta <- stats::rnorm(n = sum(p), mean = 1, sd = 0) *
#'         stats::rbinom(n = sum(p), size = 1, prob = 0.2)
#' eta <- x %*% beta
#' family <- "gaussian"
#' if (identical(family, "gaussian")) {
#'   y <- eta + 0.5 * stats::rnorm(n = n, sd = stats::sd(eta))
#' } else if (identical(family, "binomial")) {
#'   y <- stats::rbinom(n = n, size = 1, prob = 1 / (1 + exp(-eta)))
#' } else if (identical(family, "cox")) {
#'   time <- stats::rexp(n = n, rate = exp(eta))
#'   status <- stats::rbinom(n = n, prob = 0.5, size = 1)
#'   y <- survival::Surv(time = time, event = status)
#' }
#' cond <- rep(x = c(TRUE, FALSE), times = c(n0, n1))
#'
#' y_hat <- coef <- list()
#'
#' # standard ridge regression
#' object <- glmnet::cv.glmnet(x = x[cond, ], y = y[cond],
#'                            family = family, alpha = 0)
#' coef$glmnet <- stats::coef(object = object, s = "lambda.min")
#' y_hat$glmnet <- stats::predict(object = object, newx = x[!cond, ],
#'                               type = "response", s = "lambda.min")
#'
#' # multi-penalty ridge regression
#' object <- multiridge(x = x[cond, ], y = y[cond], z = z, family = family)
#' coef$multiridge <- stats::coef(object = object)
#' y_hat$multiridge <- stats::predict(object = object, newx = x[!cond, ])
#'
#' # estimation performance
#' sapply(coef, function(x) stats::cor(beta, x[-1]))
#' sapply(coef, function(x) mean((beta-x[-1])^2))
#'
#' # predictive performance
#' if (identical(family, "gaussian")) {
#'   metric <- sapply(X = y_hat, FUN = function(x)
#'     mean((x-y[!cond])^2))
#' } else if (identical(family, "binomial")) {
#'   metric <- sapply(X = y_hat, FUN = function(x)
#'     pROC::auc(response = y[!cond],
#'               predictor = as.vector(x),
#'               levels = c(0, 1),
#'               direction = "<"))
#' } else if (identical(family, "cox")) {
#'   metric <- sapply(X = y_hat, FUN = function(x)
#'     survival::concordance(y[!cond]~I(-x))$concordance)
#' }
#' metric
#' }
#'
#' @keywords models, regression, classif
#'
#' @export
multiridge <- function(x, y, z, family = "gaussian", foldid = NULL, nfolds = 10,
                       penalties = NULL) {
  # --- check arguments ---
  .check(x = x, type = "numeric", dim = c(Inf, Inf))
  .check(x = y, type = "numeric", dim = nrow(x))
  .check(x = z, type = "integer", dim = ncol(x),
         min = 1, max = length(unique(z)))
  .check(x = family, type = "nominal",
         support = c("gaussian", "binomial", "cox"))
  .check(x = foldid, type = "integer", dim = nrow(x), min = 1, max = nrow(x))
  .check(x = nfolds, type = "integer", min = 1, max = nrow(x))
  .check(x = penalties, type = "numeric", dim = length(unique(z)), min = 0)
  .validate(x = x, y = y, family = family)
  if (identical(family, "poisson")) {
    stop("Argument family='poisson' is not implemented.")
  }
  if (is.matrix(x) && ncol(x) != length(z)) {
    stop(paste(
      "For each variable,",
      "'x' should have one column",
      "and 'z' should have one entry."
    ))
  }
  cond <- !is.null(penalties) && !is.null(z) &&
    length(unique(z)) != length(penalties)
  if (cond) {
    stop("Argument 'penalties' must have one entry for each group.")
  }
  # --- initial regression ---
  scale <- .forescale(x = x, y = y, family = family)
  model <- ifelse(identical(family, "gaussian"),
                  yes = "linear",
                  no = ifelse(identical(family, "binomial"),
                              yes = "logistic",
                              no = family))
  xx <- lapply(X = unique(z), FUN = function(i) scale$x[, z == i])
  xxblocks <- multiridge::createXXblocks(datablocks = xx)
  invisible(utils::capture.output(
    init <- multiridge::fastCV2(XXblocks = xxblocks,
                                Y = scale$y,
                                model = model)
  ))
  # --- cross-validation ---
  if (is.null(penalties)) {
    if (is.null(foldid)) {
      indices <- multiridge::CVfolds(Y = scale$y, model = model, kfold = nfolds)
    } else {
      indices <- lapply(X = seq_len(nfolds),
                        FUN = function(x) which(foldid == x))
    }
    invisible(utils::capture.output(
      final <- multiridge::optLambdasWrap(penaltiesinit = init$lambdas,
                                          XXblocks = xxblocks,
                                          Y = scale$y,
                                          folds = indices)
    ))
    penalties <- final$optpen
  }
  # --- refit ---
  xxt <- multiridge::SigmaFromBlocks(XXblocks = xxblocks,
                                     penalties = penalties)
  if (identical(family, "cox")) {
    object <- multiridge::IWLSCoxridge(XXT = xxt,
                                       Y = scale$y)
  } else {
    object <- multiridge::IWLSridge(XXT = xxt,
                                    Y = scale$y,
                                    model = model)
  }
  object$family <- ifelse(identical(family, "linear"),
                          yes = "gaussian",
                          no = ifelse(identical(family, "logistic"),
                                      yes = "binomial",
                                      no = family))
  object$penalties <- penalties
  object$datablocks <- xx
  object$z <- z
  object$pars <- scale$pars
  class(object) <- "multiridge"
  object
}

#' @title
#' Make Predictions
#'
#' @description
#' Makes predictions from a multi-penalty ridge regression model.
#'
#' @inheritParams coef.multiridge
#' @inheritParams predict.corila
#'
#' @inherit multiridge references
#'
#' @return
#' Returns an \eqn{n_0}-dimensional vector of fitted values
#' or an \eqn{n_1}-dimensional vector of predicted values.
#'
#' @seealso
#' Fit models with \code{\link{multiridge}()}
#' and extract coefficients with \code{\link{coef.multiridge}()}.
#'
#' @inherit multiridge examples
#'
#' @keywords methods
#'
#' @export
predict.multiridge <- function(object, newx, ...) {
  # --- check arguments ---
  .check(x = newx, type = "numeric", dim = c(Inf, length(object$z)))
  if (length(object$z) != ncol(newx)) {
    stop(paste(
      "Argument 'newx' must have one column",
      "for each variable used in model fitting."
    ))
  }
  # --- make prediction s---
  scale <- .forescale(x = newx, pars = object$pars)
  newxx <- lapply(X = unique(object$z),
                  FUN = function(x) scale$x[, object$z == x])
  xxblocks <- multiridge::createXXblocks(datablocks = object$datablocks,
                                         datablocksnew = newxx)
  sigmanew <- multiridge::SigmaFromBlocks(XXblocks = xxblocks,
                                          penalties = object$penalties)
  eta <- drop(multiridge::predictIWLS(IWLSfit = object, Sigmanew = sigmanew))
  if (identical(object$family, "cox")) {
    y_hat <- exp(eta)
  } else {
    y_hat <- .mean_function(x = eta, family = object$family)
  }
  .backscale(pars = object$pars, y = y_hat)$y
}

#' @title
#' Extract Coefficients
#'
#' @description
#' Extracts coefficients from a multi-penalty ridge regression model.
#'
#' @param object
#' object of class \code{"multiridge"}
#'
#' @param ...
#' (not used)
#'
#' @inherit multiridge references
#'
#' @return
#' Returns an \eqn{(1 + p)}-dimensional vector of estimated coefficients
#' (estimated intercept and estimated slopes).
#'
#' @seealso
#' Fit models with \code{\link{multiridge}()}
#' and make predictions with \code{\link{predict.multiridge}()}.
#'
#' @inherit multiridge examples
#'
#' @keywords methods
#'
#' @export
coef.multiridge <- function(object, ...) {
  xblocks <- multiridge::createXblocks(datablocks = object$datablocks)
  coef <- multiridge::betasout(object,
                               Xblocks = xblocks,
                               penalties = object$penalties)
  #if (identical(object$family, "cox") & is.null(coef[[1]])) {
  #  coef[[1]] <- NA # was 0
  #}
  .backscale(pars = object$pars, coef = unlist(coef))$coef
}

#----- group-lasso -----

#' @title Initial coefficients
#'
#' @description
#' Estimate initial coefficients.
#'
#' @inheritParams corila
#'
#' @examples
#' n <- 20
#' p <- 10
#' x <- matrix(rnorm(n * p), nrow = n, ncol = p)
#' beta <- rbinom(n = p, size = 1, prob = 0.5) * rnorm(p)
#' y <- drop(x %*% beta)
#' .estim_initial_coefs(x = x,
#'                      y = y,
#'                      family = "gaussian",
#'                      alpha = "spearman",
#'                      group = NULL,
#'                      foldid = NULL,
#'                      nfolds = 10,
#'                      lambda = NULL)
#'
#' @keywords internal
#'
#' @export
.estim_initial_coefs <- function(x, y, family, alpha, group,
                                 foldid, nfolds, lambda) {
  # --- check arguments ---
  methods <- c("pearson", "spearman", "kendall")
  .check(x = x, type = "numeric", dim = c(Inf, Inf))
  .check(x = y, type = "numeric", dim = nrow(x))
  .check(x = family, type = "nominal",
         support = c("gaussian", "binomial", "poisson", "cox"))
  if (is.character(alpha)) {
    .check(x = alpha, type = "nominal",
           support = methods)
  } else {
    .check(x = alpha, type = "numeric", min = 0, max = 1, na.rm = TRUE)
  }
  .check(x = foldid, type = "integer", dim = nrow(x), min = 1, max = nrow(x))
  .check(x = nfolds, type = "integer", min = 1, max = nrow(x))
  .check(x = lambda, type = "numeric", min = 0)
  # --- estimate initial coefficients ---
  p <- ncol(x)
  if (all(is.na(alpha))) {
    coef <- rep(x = 1, times = p) # Remove this confusing option?
  } else if (is.character(alpha) && identical(alpha, "multiridge")) {
    if (is.null(lambda)) {
      model <- multiridge(x = x,
                          y = y,
                          z = group,
                          family = family,
                          foldid = foldid,
                          nfolds = nfolds)
      coef <- stats::coef(object = model, s = "lambda.min")[-1]
      lambda <- model$penalties
    } else {
      model <- multiridge(x = x,
                          y = y,
                          z = group,
                          family = family,
                          penalties = lambda)
      coef <- stats::coef(object = model)[-1]
    }
  } else if (is.character(alpha) && alpha %in% methods) {
    coef <- stats::cor(x = x,
                       y = y,
                       method = alpha,
                       use = "pairwise.complete")
    coef[is.na(coef)] <- 0
  } else if (is.numeric(alpha) && alpha >= 0 && alpha <= 1) {
    cond <- rep(c(FALSE, TRUE), times = c(family != "cox", p))
    if (is.null(lambda)) {
      model <- glmnet::cv.glmnet(x = x,
                                 y = y,
                                 family = family,
                                 alpha = alpha,
                                 foldid = foldid,
                                 nfolds = nfolds)
      coef <- stats::coef(object = model, s = "lambda.min")[cond]
      lambda <- model$lambda.min
    } else {
      model <- glmnet::glmnet(x = x,
                              y = y,
                              family = family,
                              alpha = alpha)
      coef <- stats::coef(object = model, s = lambda)[cond]
    }
  } else {
    stop("Invalid value for agrument 'alpha'.")
  }
  list(coef = drop(coef), lambda = lambda)
}

#' @title
#' Group lasso
#'
#' @description
#' Fits an initial ridge regression to obtain weights
#' for an adaptive lasso regression
#' that allows for heterogeneous, overlapping and unknown groups
#' of correlated variables.
#'
#' @param x
#' \eqn{n_0 \times p} predictor matrix,
#' where \eqn{n_0} is the number of observations used for model training
#' and \eqn{p} is the number of variables
#'
#' @param y
#' \eqn{n_0}-dimensional response vector,
#' where \eqn{n_0} is the number of observations used for model training
#'
#' @param group
#' \emph{(i)} \eqn{p}-dimensional vector of group indices
#' (in \eqn{\{1, \ldots, q\}}) or labels,
#' \emph{(ii)} list with \eqn{q} slots containing the variable indices
#' (in \eqn{\{1, \ldots, p\}}) or labels,
#' or \emph{(iii)} \eqn{p \times p} matrix,
#' where the entry in the \eqn{j^{\text{th}}} row
#' and the \eqn{k^{\text{th}}} column
#' indicates whether information should be transferred
#' from the \eqn{j^{\text{th}}} to the \eqn{k^{\text{th}}} variable
#'
#' @param include
#' \eqn{p}-dimensional logical vector
#' indicating whether a predictor may be included in the final model
#' (\code{TRUE}, "primary predictors")
#' or must be excluded from the final model
#' (\code{FALSE}, "auxiliary predictors")
#'
#' @param alpha_init
#' elastic net mixing parameter
#' (\eqn{0 \leq} \code{alpha_init} \eqn{\leq 1})
#' for initial regression
#' (default: ridge penalisation with \code{alpha_init}=0);
#' alternative choices are
#' "pearson", "spearman", or "kendall"
#' to use initial correlation coefficients
#' (not implemented for \code{family="cox"}),
#' "multiridge" for multi-penalty ridge regression
#' with one penalty for each group
#' (not implemented for \code{family="poisson"} or overlapping groups),
#' or \code{NA} to set all initial coefficients equal to 1
#'
#' @param alpha_final
#' elastic net mixing parameter for final regression
#' (default: lasso penalisation with \code{alpha_final}=1)
#'
#' @param family
#' character string \code{"gaussian"}, \code{"binomial"},
#' \code{"poisson"}, or \code{"cox"}
#'
#' @param foldid
#' \eqn{n}-dimensional vector containing the fold identifiers
#'
#' @param nfolds
#' integer specifying the number of folds
#'
#' @param hyper
#' list of of \eqn{m}-dimensional vectors
#' or a data frame with \eqn{m} rows
#' containing candidate values
#' for the regularisation and mixing hyperparameters
#'
#' @param cor
#' character string \code{"pearson"},
#' \code{"spearman"} (default),
#' or \code{"kendall"};
#' or \eqn{p \times p} correlation matrix
#'
#' @param lambda_init
#' regularisation hyperparameter(s),
#' or \code{NULL} (cross-validation)
#'
#' @details
#' The number of observations (samples) for training or testing
#' are indicated by \eqn{n_0} and \eqn{n_1}, respectively,
#' the number of variables (features) is indicated by \eqn{p},
#' and the number of variable groups is indicated by \eqn{q}.
#'
#' Observations (samples) are indexed by \eqn{i} in \eqn{\{1, \ldots, n\}},
#' variables (features) are indexed by \eqn{j} in \eqn{\{1, \ldots, p\}},
#' and variable groups are indexed by \eqn{k} in \eqn{\{1, \ldots, q\}}.
#'
#' The number of variables in the \eqn{k^{\text{th}}} group
#' is indicated by \eqn{p_k}, with \eqn{\sum_{k=1}^q p_k = p}.
#'
#' @return
#' Returns an object of class \code{"corila"}.
#'
#' @inherit corila-package references
#'
#' @seealso
#' Estimate parameters and tune hyperparameters (using cross-validation)
#' with \code{\link{cv.corila}()}.
#' Make predictions for a range of hyperparameters
#' with \code{\link{predict.corila}()}.
#'
#' This function calls
#' \code{\link{.forescale}()} and \code{\link{.backscale}()}
#' for standardising data and bringing results back to the original scale,
#' respectively,
#' \code{\link{multiridge}()} for obtaining initial group penalties,
#' and \code{\link[glmnet]{cv.glmnet}()} and \code{\link[glmnet]{glmnet}()}
#' for adaptive lasso regression.
#'
#' @examples
#' \donttest{
#' # simulation
#' n <- 100
#' p <- 50
#' group <- rep(x = 1:10, each = 5)
#' include <- NULL
#' x <- matrix(data = rnorm(n * p), nrow = n, ncol = p)
#' y <- rnorm(n = n)
#'
#' # model fitting
#' hyper <- data.frame(exp_local = 1, wgt_local = 0.5,
#'                     exp_global = 1, wgt_global = 0.5)
#' object <- corila(x, y, group, include, family = "gaussian", hyper = hyper)
#'
#' y_hat <- stats::predict(object, newx = x, index = 1, s = 0)
#' }
#'
#' @keywords models, regression, classif
#'
#' @export
corila <- function(x, y, group, include, family, hyper, alpha_init = 0,
                   alpha_final = 1, cor = "spearman", foldid = NULL,
                   nfolds = 10, lambda_init = NULL) {
  # --- check arguments ---
  .check(x = x, type = "numeric", dim = c(Inf, Inf))
  n <- nrow(x) # sample size
  p <- ncol(x) # number of features
  .check(x = y, type = "numeric", dim = n)
  if (is.vector(group) && is.atomic(group)) {
    if (is.numeric(group)) {
      .check(x = group, type = "integer", dim = p, min = 1, max = p)
    } else if (is.character(group)) {
      .check(x = group, type = "nominal", dim = p, support = colnames(x))
    } else {
      stop("If argument 'group' is a vector, ",
           "it should be a numeric or character vector.")
    }
  } else if (is.list(group)) {
    for (i in seq_along(group)) {
      if (is.numeric(group[[i]])) {
        .check(x = group[[i]], type = "integer", dim = Inf, min = 1, max = p)
      } else if (is.character(group[[i]])) {
        .check(x = group[[i]], type = "nominal", dim = Inf,
               support = colnames(x))
      } else {
        stop("If argument 'group' is a list, ",
             "it should be a list of numeric or character vectors.")
      }
    }
  } else if (is.matrix(group)) {
    .check(x = group, type = "integer", dim = c(p, p), min = -1, max = 1)
  } else {
    stop("Argument 'group' should be a vector, a list, or a matrix.")
  }
  .check(x = include, type = "logical", dim = p)
  .check(x = family, type = "nominal",
         support = c("gaussian", "binomial", "poisson", "cox"))
  slots <- c("wgt_local", "wgt_global", "exp_local", "exp_global")
  .check(x = names(hyper), type = "nominal", dim = length(slots),
         support = slots)
  .check(x = as.matrix(hyper), type = "numeric",
         dim = c(Inf, length(slots)), min = 0)
  if (is.character(alpha_init)) {
    .check(x = alpha_init, type = "nominal",
           support = c("pearson", "spearman", "kendall"))
  } else {
    .check(x = alpha_init, type = "numeric", min = 0, max = 1, na.rm = TRUE)
  }
  .check(x = alpha_final, type = "numeric", min = 0, max = 1)
  if (is.character(cor)) {
    .check(x = cor, type = "nominal",
           support = c("pearson", "spearman", "kendall"))
  } else {
    .check(x = cor, type = "numeric", dim = c(p, p), min = 0, max = 1)
  }
  .check(x = foldid, type = "integer", dim = n, min = 1, max = n)
  .check(x = nfolds, type = "integer", min = 1, max = n)
  .check(x = lambda_init, type = "numeric", min = 0)
  .validate(x = x, y = y, family = family)

  if (identical(alpha_init, "multiridge") && identical(family, "poisson")) {
    warning("Setting alpha_init=0 due to family='poisson'.")
    alpha_init <- 0
  }
  if (is.null(group)) {
    group <- seq_len(p)
  }
  if (is.null(include)) {
    include <- rep(x = TRUE, times = p)
  }

  #if (length(group) != p) {
  # stop("Argument 'group' must be a vector of length p.")
  #}
  if (is.numeric(group) && !is.array(group)) {
    q <- length(unique(group)) # number of groups = number of unique values
  } else if (is.list(group)) {
    q <- length(group) # number of groups = number of slots
  } else {
    q <- NA
  }
  if (is.numeric(group) && !is.array(group)) {
    if (length(group) != p ||
          max(group) != q ||
          any(sort(unique(group)) != seq(from = 1, to = max(group), by = 1))) {
      stop(paste("Argument 'group' should be of length p,",
                 "with all entries in {1, ..., q}."))
    }
  } else {
    if (is.character(group[[1]])) {
      #test <- lapply(group, function(slot)
      # sapply(slot, function(entry) which(colnames(x) == entry)))
      warning("Implement this.")
    }
  }
  args <- mget(setdiff(c("n", "p", "q", names(formals(corila))), c("x", "y")))
  scale <- .forescale(x = x, y = y, family = family)
  rm(x, y)
  # --- fold identifiers ---
  if (is.null(lambda_init) && is.null(foldid)) {
    foldid <- .folds(y = scale$y, family = family, nfolds = nfolds)
  }
  # --- initial coefficients ---
  init <- .estim_initial_coefs(x = scale$x,
                               y = scale$y,
                               family = family,
                               alpha = alpha_init,
                               group = group,
                               foldid = foldid,
                               nfolds = nfolds,
                               lambda = lambda_init)
  #--- feature correlation ---
  if (!is.matrix(cor)) {
    cor <- stats::cor(x = scale$x, method = cor, use = "pairwise.complete")
  }
  cor[is.na(cor)] <- 0
  #--- regression ---
  object <- list()
  for (i in seq_len(nrow(hyper))) {
    weight <- list()
    weight$global <- weight$local <- rep(x = NA, times = p)
    # rename to weight$local and weight$global
    for (j in seq_len(p)) {
      # features in same group
      #if (is.numeric(group) && !is.array(group)) {
      if (is.vector(group) && is.atomic(group)) {
        adjacent <- group[j] == group
      } else if (is.list(group)) {
        if (is.numeric(unlist(group))) {
          group_cond <- vapply(X = group,
                               FUN = function(slot) j %in% slot,
                               FUN.VALUE = logical(1))
          adjacent <- seq_len(p) %in% unlist(group[group_cond])
        } else {
          group_cond <- vapply(
            X = group,
            FUN = function(slot) colnames(scale$x)[j] %in% slot,
            FUN.VALUE = logical(1)
          )
          adjacent <- colnames(scale$x) %in% unlist(group[group_cond])
        }
        #names(group_index) <- group
      } else if (is.matrix(group)) {
        adjacent <- group[, j] == 1
      } else {
        stop("Argument 'group' should be a vector, a list, or a matrix.")
      }
      cor_trans <- sign(cor[, j]) * abs(cor[, j])^hyper$exp_local[i]
      temp <-  cor_trans * init$coef * adjacent
      weight$local[j] <- sum(pmax(0, temp)[adjacent]) / sum(adjacent)
      weight$local[p + j] <- sum(pmax(0, -temp)[adjacent]) / sum(adjacent)

      # ad-hoc solution for features that are in no group:
      weight$local[is.na(weight$local)] <- 0 # Consider 0 and weight$ind

      # all features
      temp <- sign(cor[, j]) * abs(cor[, j])^hyper$exp_global[i] * init$coef
      weight$global[j] <- sum(pmax(0, temp)) / p
      weight$global[p + j] <- sum(pmax(0, -temp)) / p
    }
    # # temporary code with beta distribution:
    # temp <- sign(cor[, j]) *
    # stats::qbeta(p = abs(cor[, j]),
    # shape1 = hyper$alpha[i],
    # shape2 = hyper$beta[i]) * init$coef * adjacent
    weight <- lapply(weight, function(x) p * ifelse(x == 0, 0, x / sum(x)))
    pf_ext <- 1 / (weight$local * hyper$wgt_local[i] +
                     weight$global * hyper$wgt_global[i])
    # To obtain standard lasso set pf_ext equal to 1.
    pf_ext[!c(include, include)] <- Inf # excluded features
    if (any(is.na(pf_ext))) {
      stop("missing pf: ", sum(is.na(pf_ext)))
    }
    if (any(pf_ext < 0)) {
      stop(paste0("negative pf:", min(pf_ext)))
    }
    object[[i]] <- glmnet::glmnet(x = cbind(scale$x, -scale$x),
                                  y = scale$y,
                                  family = family,
                                  penalty.factor = pf_ext,
                                  lower.limits = 0,
                                  alpha = alpha_final)
  }
  structure(
    list(
      model = object,
      lambda_init = init$lambda,
      scale = scale$pars,
      args = args
    ),
    class = "corila"
  )
}

#' @title
#' predict (S3 method)
#'
#' @description
#' Makes prediction from an object of class \code{"corila"}.
#'
#' @inheritParams predict.cv.corila
#'
#' @param object
#' object of class \code{"corila"}
#'
#' @param index
#' integer scalar specifying the index of the mixing hyperparameter(s)
#'
#' @param s
#' numeric vector specifying the values of the regularisation hyperparameter
#'
#' @param ... (not used)
#'
#' @return
#' Returns fitted or predicted values in an
#' \eqn{n_0}-dimensional or \eqn{n_1}-dimensional vector, respectively.
#'
#' @inherit corila-package references
#'
#' @seealso
#' Estimate parameters with \code{\link{corila}()},
#' or estimate parameters and tune hyperparameters
#' with \code{\link{cv.corila}()}.
#'
#' @inherit corila examples
#'
#' @keywords methods
#'
#' @export
predict.corila <- function(object, newx, index, s, ...) {
  # --- check arguments ---
  .check(x = newx, type = "numeric", dim = c(Inf, length(object$scale$mu.x)))
  .check(x = index, type = "integer", min = 1, max = length(object$model))
  .check(x = s, type = "numeric", dim = Inf, min = 0)
  # --- make predictions ---
  newx_stand <- .forescale(x = newx, pars = object$scale)$x
  y_hat_stand <- stats::predict(object = object$model[[index]],
                                newx = cbind(newx_stand, -newx_stand),
                                s = s,
                                type = "response")
  #type = ifelse(object$scale$family == "cox", "link", "response"))
  y_hat <- .backscale(y = y_hat_stand, pars = object$scale)$y
  y_hat
}

.set_candidates <- function(tune) {
  .check(x = tune, type = "nominal")
  #if (FALSE) {
  #  cand <- seq(from = 0, to = 1, by = 0.1)
  #  hyper <- data.frame(weight.local = cand,
  #                      weight.global = 1 - cand,
  #                      exp_local = 1,
  #                      exp_global = 1)
  #  cand <- seq(from = 0, to = 2, by = 0.2)
  #  hyper <- data.frame(weight.local = 0,
  #                      weight.global = 1,
  #                      exp_local = 0,
  #                      exp_global = cand)
  #}
  if (identical(tune, "none")) {
    hyper <- data.frame(wgt_local = 1,
                        exp_local = 1,
                        wgt_global = 0,
                        exp_global = Inf)
  } else if (identical(tune, "trial")) {
    wgt_cand <- seq(from = 0, to = 1, by = 0.1)
    hyper <- data.frame(wgt_local = wgt_cand,
                        exp_local = 0,
                        wgt_global = 1 - wgt_cand,
                        exp_global = 1)
  } else if (identical(tune, "wgt")) {
    wgt_cand <- seq(from = 0, to = 1, by = 0.1) # for weighted sums
    hyper <- data.frame(wgt_local = wgt_cand,
                        exp_local = 1,
                        wgt_global = 1 - wgt_cand,
                        exp_global = 1)
  } else if (identical(tune, "exp")) {
    exp_cand <- c(0, 0.1, 0.25, 1 / 3, 0.5, 1, 2, 3, 4, 10, Inf)
    hyper <- data.frame(wgt_local = 1,
                        exp_local = exp_cand,
                        wgt_global = 0,
                        exp_global = exp_cand)
  } else if (identical(tune, "sep")) {
    exp_cand <- c(0.1, 0.5, 1, 2, 10)
    hyper <- expand.grid(exp_local = exp_cand,
                         exp_global = exp_cand)
    hyper$wgt_local <- hyper$wgt_global <- 0.5
  } else if (identical(tune, "both")) {
    #wgt_cand <- seq(from = 0, to = 1, by = 0.25) # original
    wgt_cand <- seq(from = 0, to = 1, by = 0.1) # trial
    hyper <- data.frame(wgt_local = wgt_cand,
                        exp_local = NA,
                        wgt_global = 1 - wgt_cand,
                        exp_global = NA)
    #exp_cand <- c(0.1, 0.5, 1, 2, 10) # original
    exp_cand <- c(0.1, 0.5, 0.8, 1, 1.25, 2, 10)
    hyper <- hyper[rep(seq_len(nrow(hyper)), each = length(exp_cand)), ]
    hyper$exp_local <- hyper$exp_global <- exp_cand
  } else if (identical(tune, "all")) {
    wgt_cand <- seq(from = 0, to = 1, by = 0.25)
    exp_cand <- c(0.1, 0.5, 1, 2, 10)
    hyper <- expand.grid(wgt_local = wgt_cand,
                         exp_local = exp_cand,
                         exp_global = exp_cand)
    hyper$wgt_global <- 1 - hyper$wgt_local
    hyper$exp_local[hyper$wgt_local == 0] <- Inf
    hyper$exp_global[hyper$wgt_global == 0] <- Inf
  } else {
    stop()
  }
  hyper <- unique(hyper)
  rownames(hyper) <- seq_len(nrow(hyper))
  hyper
}

#' @title
#' Sparse Group Lasso
#'
#' @description
#' Optimises the parameters and the hyperparameters of the sparse group lasso.
#'
#' @inheritParams corila
#'
#' @param tune
#' character \code{"wgt"}, \code{"exp"}, or \code{"both"}
#' for determining the candidate values for the hyperparameters;
#' or list with slots \code{wgt_local}, \code{wgt_global}, \code{exp_local},
#' and \code{exp_global} (not yet implemented)
#'
#' @inherit corila details
#'
#' @return
#' Returns an object of class \code{cv.corila},
#' a list with the following slots:
#' \itemize{
#' \item \code{object}:
#' list with one slot for each combination of hyperparameters,
#' each slot contains an object of class \code{"glmnet"}
#' \item \code{hyper}:
#' data frame with one row for each combination of hyperparameters,
#' four columns for the values of the hyperparameters
#' (\code{wgt_local}, \code{wgt_global},
#' \code{exp_global}, and \code{exp_local})
#' and a column for the cross-validated loss (\code{cvm})
#' \item \code{id_hyper}:
#' index of combination of hyperparameters
#' leading to the lowest cross-validated loss
#' \item \code{lambda.min}
#' optimised regularisation hyperparameter
#' \item \code{scale}:
#' output from \code{\link{.forescale}()}
#' }
#'
#' @inherit corila-package references
#'
#' @seealso
#' Extract coefficients with \code{\link[=coef.cv.corila]{coef}()}
#' and make predictions with \code{\link[=predict.cv.corila]{predict}()}.
#'
#' This user function repeatedly calls \code{\link{corila}()}
#' with different values for the regularisation and mixing hyperparameters.
#'
#' @examples
#' # minimal example
#' n <- 50; p <- 20; q <- 5
#' x <- matrix(rnorm(n * p), nrow = n , ncol = p)
#' y <- rnorm(n)
#' group <- rep(seq_len(q), length.out = p)
#' include <- as.logical(rbinom(n = p, size = 1, prob = 0.5))
#' cv.corila(x = x, y = y, group = group, include = include, tune = "none")
#'
#' \donttest{
#' # simulation
#' set.seed(1)
#' n0 <- 100
#' n1 <- 10000
#' n <- n0 + n1
#' p <- c(100, 50)
#' z <- rep(x = seq_along(p), times = p)
#' x <- sapply(X = z, FUN = function(x) stats::rnorm(n = n, sd = x))
#' beta <- stats::rnorm(n = sum(p), mean = 1, sd = 0) *
#'         stats::rbinom(n = sum(p), size = 1, prob = 0.2)
#' eta <- x %*% beta
#' family <- "gaussian"
#' if (identical(family, "gaussian")) {
#'   y <- eta + 0.5 * stats::rnorm(n = n, sd = stats::sd(eta))
#' } else if (identical(family, "binomial")) {
#'   y <- stats::rbinom(n = n, size = 1, prob = 1 / (1 + exp(-eta)))
#' } else if (identical(family, "cox")) {
#'   time <- stats::rexp(n = n, rate = exp(eta))
#'   status <- stats::rbinom(n = n, prob = 0.5, size = 1)
#'   y <- survival::Surv(time = time, event = status)
#' }
#' cond <- rep(x = c(TRUE, FALSE), times = c(n0, n1))
#'
#' y_hat <- coef <- list()
#'
#' # standard lasso regression
#' object <- glmnet::cv.glmnet(x = x[cond, ], y = y[cond],
#'                             family = family, alpha = 1)
#' coef$glmnet <- stats::coef(object = object, s = "lambda.min")
#' y_hat$glmnet <- stats::predict(object = object, newx = x[!cond, ],
#'                                type = "response", s = "lambda.min")
#'
#' # flexible group lasso regression
#' object <- cv.corila(x = x[cond, ], y = y[cond], group = z, family = family)
#' coef$corila <- stats::coef(object = object)
#' y_hat$corila <- stats::predict(object = object, newx = x[!cond, ])
#'
#' # selection performance
#' sapply(coef, function(x) mean(sign(x[-1]) == sign(beta)))
#' sapply(coef, function(x) {
#'   sum(sign(x[-1]) != 0 & sign(x[-1]) == sign(beta)) / sum(x[-1] != 0)
#' })
#'
#' # predictive performance
#' if (identical(family, "gaussian")) {
#'   metric <- sapply(X = y_hat, FUN = function(x)
#'     mean((x-y[!cond])^2))
#' } else if (identical(family, "binomial")) {
#'   metric <- sapply(X = y_hat, FUN = function(x)
#'     pROC::auc(response = y[!cond],
#'               predictor = as.vector(x),
#'               levels = c(0, 1),
#'               direction = "<"))
#' } else if (identical(family, "cox")) {
#'   metric <- sapply(X = y_hat, FUN = function(x)
#'     survival::concordance(y[!cond]~I(-x))$concordance)
#' }
#' metric
#'
#' # privileged information
#' #include <- stats::rbinom(n = sum(p), size = 1, prob = 0.5) == 1
#' #object <- cv.corila(x = x[cond, ], y = y[cond], group = z,
#' #                     include = include, family = family)
#' }
#'
#' @keywords models, regression, classif
#'
#' @export
cv.corila <- function(x, y, group, include = NULL, alpha_init = 0,
                      alpha_final = 1, family = "gaussian",
                      nfolds = 10, cor = "spearman", tune = "both",
                      foldid = NULL) {
  # match arguments
  family <- match.arg(arg = tolower(family),
                      choices = c("gaussian", "binomial", "poisson", "cox"))
  if (is.character(cor)) {
    cor <- match.arg(arg = tolower(cor),
                     choices = c("pearson", "spearman", "kendall"))
  }
  # set default parameters
  .validate(x = x, y = y, family = family)
  if (is.null(include)) {
    include <- rep(x = TRUE, times = ncol(x))
  }
  if (is.null(foldid)) {
    foldid <- .folds(y = y, family = family, nfolds = nfolds)
  }
  hyper <- .set_candidates(tune = tune)
  # fit model on all folds
  object_ext <- corila(x = x,
                       y = y,
                       group = group,
                       include = include,
                       alpha_init = alpha_init,
                       alpha_final = alpha_final,
                       family = family,
                       cor = cor,
                       foldid = foldid,
                       hyper = hyper)
  lambda <- lapply(X = object_ext$model, FUN = function(x) x$lambda)
  # initialise matrices for predictions
  pred <- list()
  n <- nrow(x)
  for (j in seq_len(nrow(hyper))) {
    pred[[j]] <- matrix(data = NA,
                        nrow = n,
                        ncol = length(object_ext$model[[j]]$lambda))
  }
  # repeatedly train without and test for held-out fold
  for (i in seq_len(nfolds)) {
    object_int <- corila(x = x[foldid !=  i, ],
                         y = y[foldid != i],
                         group = group,
                         include = include,
                         alpha_init = alpha_init,
                         alpha_final = alpha_final,
                         family = family,
                         cor = cor,
                         hyper = hyper,
                         lambda_init = object_ext$lambda_init)
    for (j in seq_len(nrow(hyper))) {
      pred[[j]][foldid == i, ] <- stats::predict(
        object = object_int,
        newx = x[foldid == i, , drop = FALSE],
        index = j,
        s = lambda[[j]]
      )
    }
  }
  # select the hyperparameters
  cvm <- list()
  for (l in seq_len(nrow(hyper))) {
    cvm[[l]] <- apply(
      X = pred[[l]],
      MARGIN = 2,
      FUN = function(x) .deviance(y_hat = x, y =  y, family = family)
    )
  }
  hyper$cvm <- cvm_min <- vapply(X = cvm,
                                 FUN = base::min,
                                 FUN.VALUE = numeric(1))
  id_hyper <- which.min(cvm_min)
  lambda.min <- object_ext$model[[id_hyper]]$lambda[which.min(cvm[[id_hyper]])]
  # return fitted model
  object <- object_ext
  object$id_hyper <- id_hyper
  object$lambda.min <- lambda.min
  class(object) <- "cv.corila"
  object
}

#' @title
#' print (S3 method)
#'
#' @description
#' Print method for class \code{"cv.corila"}.
#'
#' @param x
#' object of class \code{"cv.corila"}
#'
#' @param ...
#' (not used)
#'
#' @return
#' Prints "object of class 'cv.corila'" to the console.
#'
#' @seealso summary.cv.corila
#'
#' @inherit summary.cv.corila examples
#'
#' @export
print.cv.corila <- function(x, ...) {
  cat("object of class", sQuote("cv.corila"), "\n")
  content <- ifelse(length(x$object) == 1, "an object", "multiple objects")
  cat("(contains ", content, " of class ", sQuote("cv.glmnet"), ")", sep = "")
  invisible(x)
}

#' @title
#' Summarising Sparse Group Lasso (S3 method)
#'
#' @description
#' Summary method for class \code{"cv.corila"}.
#'
#' @param object
#' object of class \code{"cv.corila"}
#'
#' @param x
#' object of class \code{"summary.cv.corila"}
#'
#' @param ...
#' (not used)
#'
#' @return
#' The function \code{summary.cv.corila} returns
#' an invisible list with multiple slots.
#'
#' @examples
#' n <- 12 # decrease to 10 to check LOOCV
#' p <- 20
#' q <- 5
#' x <- matrix(rnorm(n * p), nrow = n, ncol = p)
#' y <- rnorm(n)
#' group <- rep(seq_len(q), length.out = p)
#' include <- as.logical(rbinom(n = p, size = 1, prob = 0.5))
#' object <- cv.corila(x = x, y = y, group = group, include = include)
#' print(object)
#' summary(object)
#' plot(object)
#'
#' @seealso print.corila
#'
#' @export
summary.cv.corila <- function(object, ...) {
  list <- list()
  list$family <- object$args$family
  list$n <- Inf # replace by object$n
  list$p <- object$args$p
  list$p_primary <- sum(object$args$include)
  list$p_auxiliary <- sum(!object$args$include)
  list$alpha_init <- object$args$alpha_init
  list$alpha_final <- object$args$alpha_final
  list$lambda.min <- object$lambda.min
  list$wgt_local <- object$args$hyper$wgt_local[object$id_hyper]
  list$wgt_global <- object$args$hyper$wgt_global[object$id_hyper]
  list$exp_local <- object$args$hyper$exp_local[object$id_hyper]
  list$exp_global <- object$args$hyper$exp_global[object$id_hyper]
  list$nzero <- sum(stats::coef(object, s = "lambda.min") != 0)
  class(list) <- "summary.cv.corila"
  list
}

.type <- function(x) {
  if (is.na(x)) {
    "none"
  } else if (is.numeric(x)) {
    if (x == 0) {
      "ridge regression"
    } else if (x == 1) {
      "lasso regression"
    } else if (x > 0 && x < 1) {
      "elastic net regression"
    } else {
      stop("If argument 'x' is numeric, ",
           "it should be in the unit interval.")
    }
  } else {
    if (identical(x, "multiridge")) {
      "multi-penalty ridge regression"
    } else if (x %in% c("pearson", "spearman", "kendall")) {
      paste0(toupper(substr(x = x, start = 1, stop = 1)),
             tolower(substr(x = x, start = 2, stop = nchar(x))),
             " correlation")
    } else {
      x
    }
  }
}

#' @rdname summary.cv.corila
#' @export
print.summary.cv.corila <- function(x, ...) {
  cat("--- object of class", dQuote("cv.corila"), "---", "\n")
  if (identical(x$family, "cox")) {
    cat("Cox proportional hazards model", "\n")
  } else {
    cat("generalised linear model with", x$family, "family", "\n")
  }
  cat(x$p, " features (", x$p_primary, " primary and ",
      x$p_auxiliary, " auxiliary features)", "\n", sep = "")
  cat("initial coefficients:", .type(x = x$alpha_init), "\n")
  cat("final coefficients: adaptive", .type(x = x$alpha_final), "\n")
  cat("optimised regularisation parameter: lambda.min =",
      signif(x$lambda.min, digits = 4), "\n")
  cat("selected weights: local = ", x$wgt_local,
      ", global = ", x$wgt_global, "\n", sep = "")
  cat("selected exponents: local = ", x$exp_local,
      ", global = ", x$exp_global, "\n", sep = "")
  cat(x$nzero, "non-zero coefficients",
      "(including intercept)"[x$family != "cox"])
  invisible(NULL)
}


#' @title
#' Plot Sparse Group Lasso (S3 method)
#'
#' @description
#' Plot method for class \code{"cv.corila"}.
#'
#' @param x
#' object of class \code{"cv.corila"}
#'
#' @param ...
#' (not used)
#'
#' @return
#' Returns NULL (invisible).
#'
#' @inherit summary.cv.corila examples
#'
#' @export
#'
plot.cv.corila <- function(x, ...) {
  # observed vs fitted values
  # estimated coefficient per group (if vector)
  # cvm as a functions of weights and exponents
  invisible(NULL)
}


#' @title
#' Expand auxiliary features
#'
#' @description
#' Add empty columns for auxiliary features.
#'
#' @param x
#' matrix with \eqn{n} rows and either
#' \eqn{p_0} or \eqn{p_0 + p_1} features
#'
#' @param include
#' logical vector of length \eqn{p_0 + p_1}
#' with \eqn{p_0} entries equal to \code{TRUE} (primary features)
#' and \eqn{p_1} entries equal to \code{FALSE} (auxiliary features)
#'
#' @return
#' matrix with \eqn{n} rows and \eqn{p_0 + p_1} columns
#'
#' @examples
#' n <- 5
#' p <- 10
#' x <- matrix(data = rnorm(n * p), nrow = n, ncol = p)
#' include <- as.logical(rbinom(n = p, size = 1, prob = 0.5))
#' x_primary <- x[,include]
#' x_expanded <- expand_auxiliary(x = x_primary, include = include)
#' all(x_expanded[, include] == x[, include])
#' all(x_expanded[, !include] == 0)
#'
#' @export
#'
expand_auxiliary <- function(x, include) {
  .check(x = x, type = "numeric", dim = c(Inf, Inf), na.rm = TRUE)
  .check(x = include, type = "logical", dim = Inf)
  if (ncol(x) == length(include)) {
    x
  } else if (ncol(x) == sum(include)) {
    full <- matrix(data = 0, nrow = nrow(x), ncol = length(include))
    full[, include] <- x
    full
  } else {
    stop("incompatible number of (primary) features")
  }
}


#' @title
#' predict (S3 method)
#'
#' @description
#' Makes predictions from an object of class \code{"cv.corila"}.
#'
#' @param object
#' object of class \code{"cv.corila"}
#'
#' @param newx
#' \eqn{n_0 \times p} predictor matrix (training data)
#' to obtain fitted values,
#' \eqn{n_1 \times p} predictor matrix (testing data)
#' to obtain predicted values
#'
#' @param s
#' character \code{"lambda.min"} or numeric value
#'
#' @param ...
#' (not used)
#'
#' @inherit predict.corila return
#'
#' @inherit corila-package references
#'
#' @seealso
#' Fit models with \code{\link{cv.corila}()}
#' and extract coefficients with \code{\link{coef.cv.corila}()}.
#'
#' @inherit cv.corila examples
#'
#' @keywords methods
#'
#' @export
predict.cv.corila <- function(object, newx, s = "lambda.min", ...) {
  # --- check arguments ---
  if (identical(s, "lambda.min")) {
    s <- object$lambda.min
  } else if (!is.numeric(s) || length(s) != 1 || s < 0) {
    stop("Set s='lambda.min' or provide non-negative value.")
  }
  # --- handle auxiliary predictors ---
  #if(any(object$args$include == 0) && sum(object$args$include) == ncol(newx)){
  #  full <- matrix(data = 0,
  #                 nrow = nrow(newx),
  #                 ncol = length(object$args$include))
  #  full[, object$args$include] <- newx
  #  newx <- full
  #}
  newx_full <- expand_auxiliary(x = newx, include = object$args$include)
  # --- make predictions ---
  newx_stand <- .forescale(x = newx_full, pars = object$scale)$x
  x_all <- cbind(newx_stand, -newx_stand)
  y_hat_stand <- stats::predict(object = object$model[[object$id_hyper]],
                                newx = x_all,
                                s = s,
                                type = "response")
  .backscale(y = y_hat_stand, pars = object$scale)$y
}

#' @title
#' Combine coefficients
#'
#' @description
#' Combine estimated coefficients for positive effects
#' and estimated coefficients for negative effects.
#'
#' @param alpha
#' estimated intercept:
#' scalar
#'
#' @param beta
#' estimated slopes:
#' numeric vector of length \eqn{2 * p} with non-negative entries,
#' namely of \eqn{p} estimated coefficients for positive effects
#' and \eqn{p} estimated coefficients for negative effects
#'
#' @return
#' numeric vector of length \eqn{1 + p}
#'
#' @examples
#' p <- 10
#' alpha <- rnorm(1)
#' temp <- rnorm(p)
#' beta <- pmax(c(temp, -temp), 0)
#' .combine(alpha = alpha, beta = beta)
#'
#' @export
.combine <- function(alpha, beta) {
  .check(x = alpha, type = "numeric")
  .check(x = beta, type = "numeric", dim = Inf, min = 0)
  beta_positive <- beta[1:(length(beta) / 2)]
  beta_negative <- beta[(length(beta) / 2 + 1):(length(beta))]
  eps <- 1e-06
  if (any(beta_positive > eps & beta_negative > eps)) {
    stop("The coefficient for a predictor cannot be positive and negative.")
  }
  beta_combined <- beta_positive  - beta_negative
  c(alpha, beta_combined)
}

#' @title
#' Extract coefficients
#'
#' @description
#' Extracts coefficients from an object of class \code{"cv.corila"}.
#'
#' @inheritParams predict.cv.corila
#'
#' @return
#' Returns an \eqn{(1 + p)}-dimensional vector of the estimated coefficients.
#' The first entry is the estimated intercept,
#' and the other \eqn{p} entries are the estimated slopes.
#'
#' @inherit corila-package references
#'
#' @seealso
#' Fit models with \code{\link{cv.corila}()}
#' and make predictions with \code{\link{predict.cv.corila}()}.
#'
#' @inherit cv.corila examples
#'
#' @keywords methods
#'
#' @export
coef.cv.corila <- function(object, s = "lambda.min", ...) {
  if (identical(s, "lambda.min")) {
    s <- object$lambda.min
  } else if (!is.numeric(s) || length(s) != 1 || s < 0) {
    stop("Set s='lambda.min' or provide numeric value.")
  }
  coef_stand <- as.numeric(
    stats::coef(object = object$model[[object$id_hyper]], s = s)
  )
  if (identical(object$scale$family, "cox")) {
    alpha <- NULL
    beta <- coef_stand
  } else {
    alpha <- coef_stand[1]
    beta <- coef_stand[-1]
  }
  coef <- .combine(alpha = alpha, beta = beta)
  coef <- .backscale(coef = coef, pars = object$scale)$coef
  if (any(coef[c(FALSE[object$scale$family != "cox"],
                 !object$args$include == 1)] != 0)) {
    stop("Excluded coefs must equal zero.")
  }
  coef[c(TRUE[object$scale$family != "cox"], object$args$include == 1)]
}

#----- extra -----

# Other functions for the manuscript are in the folder "scripts".

#' @title
#' Calculates precision for sign variable
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
#' or \code{NA} if all estimated signs equal 0.
#'
#' @examples
#' truth <- sample(x = c(-1, 0, 1), size = 10, replace = TRUE)
#' estim <- sample(x = c(-1, 0, 1), size = 10, replace = TRUE)
#' calc_sign_prec(truth = truth, estim = estim) # observed value
#' calc_sign_prec(truth = truth, estim = -truth) # lower limit 0
#' calc_sign_prec(truth = truth, estim = truth) # upper limit 1
#' calc_sign_prec(truth = truth, estim = 0 * estim) # not defined
#'
#' @keywords internal
#'
#' @keywords utilities
#'
#' @export
calc_sign_prec <- function(truth, estim) {
  .check(x = truth, type = "integer", dim = Inf, na.rm = TRUE)
  .check(x = estim, type = "integer", dim = length(truth), na.rm = TRUE)
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
#' Simulates data with grouped predictor variables
#'
#' @param family
#' character \code{"gaussian"}, \code{"binomial"},
#' \code{"poisson"} or \code{"cox"}
#'
#' @param n0
#' number of training observations
#'
#' @param n1
#' number of testing observations
#'
#' @param n_group
#' number of variable groups
#'
#' @param n_type
#' number of variable types
#'
#' @param size_group
#' size of variable groups (per variable type)
#'
#' @param effect_size
#' effect sizes (per variable type)
#'
#' @param corfac_feature
#' decrease of correlation if different variable
#'
#' @param corfac_type
#' decrease of correlation if different type
#'
#' @param corfac_group
#' decrease of correlation if different group
#'
#' @param n_group_causal
#' number of causal groups
#'
#' @param prop_causal
#' proportion of causal features within causal groups
#'
#' @param noise_factor
#' noise factor
#'
#' @param plot
#' Attempt to visualise effects of and correlation between variables?
#' (\code{TRUE} or \code{FALSE})
#'
#' @param trial
#' logical (groups of negatively correlated subgroups)
#'
#' @return
#' Returns a list with the following slots:
#' \itemize{
#' \item \eqn{n_0 \times p} matrix \code{x_train}
#' \item \eqn{p}-dimensional vector \code{type}
#' \item \eqn{p}-dimensional vector \code{group}
#' \item \eqn{n_0}-dimensional vector \code{y_train}
#' \item \eqn{n_1 \times p} matrix \code{x_test}
#' \item \eqn{n_1}-dimensional vector \code{y_test}
#' \item \eqn{p}-dimensional vector \code{beta}
#' \item data frame \code{info} with entries
#' \eqn{n_0}, \eqn{n_1}, \eqn{p}, \code{n_type},
#' \code{n_group}, and \code{family}
#' }
#'
#' @examples
#' data <- simulate()
#' dims <- function(x) {
#'    if (is.matrix(x)||is.data.frame(x)) {
#'      paste(base::dim(x), collapse = " x ")
#'    } else {
#'      paste0(base::length(x))
#'    }
#' }
#' sapply(X = data, FUN = dims)
#'
#' @keywords distribution
#'
#' @export
simulate <- function(family = "gaussian", n0 = 100, n1 = 10000, n_group = 20,
                     n_type = 2, size_group = c(5, 3), effect_size = c(1, 1),
                     corfac_feature = 0.5, corfac_type = 0.5,
                     corfac_group = 0.25, n_group_causal = 2,
                     prop_causal = 0.5, noise_factor = 1,
                     plot = TRUE, trial = FALSE) {
  # --- check arguments ---
  .check(x = family, type = "nominal",
         support = c("gaussian", "binomial", "poisson", "cox"))
  .check(x = n0, type = "integer", min = 2)
  .check(x = n1, type = "integer", min = 2)
  .check(x = n_group, type = "integer", min = 2)
  .check(x = n_type, type = "integer", min = 2)

  # family = "gaussian";n0 = 100;n1 = 10000;n_group = 20;n_type = 2;
  # size_group = c(5, 3);effect_size = c(1, 1);corfac_feature = 0.5;
  # corfac_type = 0.5;corfac_group = 0.25;n_group_causal = 2;
  # prop_causal = 0.5; noise_factor = 1; plot = TRUE
  n <- n0 + n1

  if (n_type != length(size_group)) {
    stop("Wrong length.")
  }

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
  var_binom <- stats::rbinom(n = sum(cond), size = 1, prob = prop_causal)
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
    tryCatch(graphics::image(x = sigma[, p:1]), error = function(x) NULL)
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
    y <- stats::rbinom(n = n, size = 1, prob = 1 / (1 + exp(-2 * eta)))
    # NB: was without 2*
  } else if (identical(family, "cox")) {
    time <- stats::rexp(n = n, rate = exp(eta))
    status <- stats::rbinom(n = n, prob = 0.5, size = 1)
    #y <- cbind(time = time, status = status)
    y <- survival::Surv(time = time, event = status)
  } else if (identical(family, "poisson")) {
    y <- stats::rpois(n = n, lambda = exp(eta))
  } else {
    stop(paste("Family", family, "not implemented."))
  }

  #- - - outputs - - -
  fold <- rep(x = c(0, 1), times = c(n0, n1))
  x_train <- x[fold == 0, ]
  y_train <- y[fold == 0]
  x_test <- x[fold == 1, ]
  y_test <- y[fold == 1]
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
