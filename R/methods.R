
#----- list of S3 methods -----

#' @title
#' List of methods for class `"cv.corila"`
#'
#' @description
#' Implemented S3 methods for objects of class `"cv.corila"`:
#'
#' - [coef()][coef.cv.corila]:
#'   extracts estimated coefficients
#' - [predict()][predict.cv.corila]:
#'   calculates predicted values
#' - [fitted()][fitted.cv.corila]:
#'   extracts fitted values
#' - [residuals()][residuals.cv.corila]:
#'   calculates deviance residuals
#' - [plot()][plot.cv.corila]:
#'   visualises observed vs fitted values and estimated coefficients
#' - [print()][print.cv.corila]:
#'   prints information to the console
#' - [summary()][summary.cv.corila]:
#'   summarises the fitted model
#' - [deviance()][deviance.cv.corila]:
#'   extracts the deviance
#' - [nobs()][nobs.cv.corila]:
#'   extracts the number of observations
#'
#' @return
#' [coef()][coef.cv.corila] returns a \eqn{(1 +) p}-dimensional vector,
#' [predict()][predict.cv.corila] returns an \eqn{n_1}-dimensional vector,
#' [fitted()][fitted.cv.corila] and [residuals()][residuals.cv.corila]
#' return an \eqn{n_0}-dimensional vector.
#' See individual methods for details.
#'
#' @seealso
#' Use [cv.corila()] to fit the model.
#'
#' @examples
#' # listing S3 methods
#' methods(class = "cv.corila")
#'
#' # fitting the model
#' n <- 10; p <- 20; q <- 5
#' x <- matrix(rnorm(n * p), nrow = n , ncol = p)
#' y <- rnorm(n)
#' group <- rep(seq_len(q), length.out = p)
#' primary <- as.logical(rbinom(n = p, size = 1, prob = 0.5))
#' object <- cv.corila(x = x, y = y, group = group, primary = primary)
#'
#' # using S3 methods
#' coef(object)
#' predict(object, newx = x)
#' fitted(object)
#' residuals(object)
#' plot(object)
#' print(object)
#' summary(object)
#'
#' @name methods
NULL

#----- S3 method coef -----

#' @title
#' Extract coefficients
#'
#' @description
#' Extracts coefficients from an object of class `"cv.corila"`.
#'
#' @inheritParams predict.cv.corila object s
#'
#' @param ...
#' (for compatibility with [stats::coef])
#'
#' @return
#' Returns an \eqn{(1 + p)}-dimensional vector of the estimated coefficients.
#' The first entry is the estimated intercept,
#' and the other \eqn{p} entries are the estimated slopes.
#'
#' @inherit corila-package references
#'
#' @seealso
#' Fit models with [cv.corila()]
#' and make predictions with [predict()][predict.cv.corila].
#'
#' @details
#' This function calls [.combine_slopes()]
#' to combine positive and negative coefficients
#' and [.backscale()] to bring coefficients back to the original scale.
#'
#' @inherit cv.corila examples
#'
#' @keywords methods
#'
#' @export
#'
#' @srrstats {RE4.2} *extracts model coefficients via S3 method*
#'
coef.cv.corila <- function(object, s = "lambda.min", ...) {
  if (identical(s, "lambda.min")) {
    s <- object$lambda.min
  } else if (!is.numeric(s) || length(s) != 1L || s < 0) {
    stop("Set s='lambda.min' or provide non-negative scalar.")
  }
  coef_stand <- as.numeric(
    stats::coef(object = object$model[[object$id_hyper]], s = s)
  )
  if (identical(object$scale$family, "cox")) {
    alpha <- NULL
    beta <- coef_stand
  } else {
    alpha <- coef_stand[1L]
    beta <- coef_stand[-1L]
  }
  coef <- .combine_slopes(alpha = alpha, beta = beta)
  coef <- .backscale(coef = coef, pars = object$scale)$coef
  is_primary <- c(FALSE[object$scale$family != "cox"], !object$args$primary)
  if (any(coef[is_primary] != 0)) {
    # nocov start (invariant check)
    stop("Excluded coefficients must equal zero.")
    # nocov end
  }
  #coef[c(TRUE[object$scale$family != "cox"], object$args$primary)] # ?
  coef
}

#----- S3 method predict -----

#' @title
#' predict (S3 method)
#'
#' @description
#' Makes predictions from an object of class `"cv.corila"`.
#'
#' @param object
#' object of class `"cv.corila"`
#'
#' @param newx
#' \eqn{n_0 \times p} predictor matrix (training data)
#' to obtain fitted values,
#' \eqn{n_1 \times p} predictor matrix (testing data)
#' to obtain predicted values
#'
#' @param s
#' character `"lambda.min"` or numeric value
#'
#' @param ...
#' (for compatibility with [stats::predict])
#'
#' @inherit predict.corila return
#'
#' @inherit corila-package references
#'
#' @seealso
#' Fit models with [cv.corila()],
#' extract coefficients with [coef()][coef.cv.corila],
#' and extract fitted values with [fitted()][fitted.cv.corila].
#'
#' @details
#' This function calls
#' [.expand_auxiliary()] for handling auxiliary predictors,
#' [.forescale()]
#' for standardising the predictor matrix,
#' and [.backscale()]
#' for bringing predicted values back to the original scale
#' (if `family="gaussian"`).
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
  } else if (!is.numeric(s) || length(s) != 1L || s < 0) {
    stop("Set s='lambda.min' or provide non-negative value.")
  }
  # --- handle auxiliary predictors ---
  newx_full <- .expand_auxiliary(x = newx, primary = object$args$primary)
  # --- make predictions ---
  newx_stand <- .forescale(x = newx_full, pars = object$scale)$x
  x_all <- cbind(newx_stand, -newx_stand)
  y_hat_stand <- stats::predict(object = object$model[[object$id_hyper]],
                                newx = x_all,
                                s = s,
                                type = "response")
  .backscale(y = drop(y_hat_stand), pars = object$scale)$y
}


#----- other S3 methods -----

#' @title
#' Fitted values
#'
#' @description
#' Extracts fitted values.
#'
#' @inheritParams predict.cv.corila object
#'
#' @param ...
#' (for compatibility with [stats::fitted])
#'
#' @return
#' Returns a numeric vector of length \eqn{n_0}
#' (one entry for each training observation).
#'
#' @seealso
#' Use [predict()][predict.cv.corila] to obtain predicted values
#' (i.e., for testing observations).
#'
#' @inherit methods examples
#'
#' @export
#'
#' @srrstats {RE4.9} *access fitted values*
#'
fitted.cv.corila <- function(object, ...) {
  object$y_fit
}

#' @title
#' Residuals
#'
#' @inheritParams predict.cv.corila object
#'
#' @param ...
#' (for compatibility with [stats::residuals])
#'
#' @details
#' This function extracts the observed and fitted values from the fitted model
#' and calls the internal function [.residuals()] to calculate the residuals.
#'
#' @inherit methods examples
#'
#' @importFrom stats residuals
#'
#' @export
#'
#' @return
#' Returns a numeric vector of length \eqn{n_0}
#' (one entry for each training observation).
#'
#' @srrstats {RE4.10} *model residuals*
#'
residuals.cv.corila <- function(object, ...) {
  .residuals(y_obs = object$y_obs,
             y_fit = as.numeric(object$y_fit),
             family = object$args$family)
}

#' @title
#' Plot Sparse Group Lasso (S3 method)
#'
#' @description
#' Plot method for class `"cv.corila"`.
#'
#' @param x
#' object of class `"cv.corila"`
#'
#' @param ...
#' (for compatibility with [base::plot])
#'
#' @details
#' This function generates two figures:
#' - a scatter plot of fitted versus observed values
#' for the Gaussian and the Poisson families,
#' a box plot of predicted probabilities for the two classes
#' for the binomial family,
#' or a histogram of fitted relative risks for the Cox model
#' - estimated coefficients versus indices of predictors
#'
#' @return
#' Returns `NULL` invisibly.
#'
#' @inherit summary.cv.corila examples
#'
#' @export
#'
#' @srrstats {RE6.0} *default plot method*
#' @srrstats {RE6.2} *plot fitted values*
#'
plot.cv.corila <- function(x, ...) {
  y_obs <- x$y_obs
  y_fit <- x$y_fit
  beta <- stats::coef(x, s = "lambda.min")[-1L]
  graphics::par(mfrow = c(1L, 2L))
  if (x$args$family %in% c("gaussian", "poisson")) {
    max <- max(abs(c(y_obs, y_fit)))
    lim <- c(-max, max)
    graphics::plot(x = y_obs,
                   y = y_fit,
                   xlab = "observed values",
                   ylab = "fitted values",
                   xlim = lim, ylim = lim)
    graphics::abline(a = 0, b = 1, lty = 2)
  } else if (identical(x$args$family, "binomial")) {
    graphics::boxplot(y_fit ~ y_obs,
                      xlab = "observed class",
                      ylab = "fitted probabilities", ylim = c(0, 1))
  } else if (identical(x$args$family, "cox")) {
    graphics::hist(y_fit,
                   xlab = "fitted relative risks",
                   ylab = "frequency",
                   main = "")
  }
  max <- max(abs(beta))
  lim <- c(-max, max)
  graphics::plot(y = beta,
                 x = seq_along(beta),
                 xlab = "predictor",
                 ylab = "coefficient", type = "h", lwd = 2, ylim = lim)
  invisible(NULL)
}

#' @title
#' print (S3 method)
#'
#' @description
#' Print method for class `"cv.corila"`.
#'
#' @inheritParams plot.cv.corila x
#'
#' @param ...
#' (for compatibility with [base::print])
#'
#' @return
#' Prints `"object of class 'cv.corila'"` to the console
#' (with a note on the number of cross-validated models).
#' Returns `x` invisibly.
#'
#' @seealso
#' [summary()][summary.cv.corila]
#'
#' @inherit summary.cv.corila examples
#'
#' @keywords internal
#'
#' @export
#'
#' @srrstats {RE4.17} *print method returns number of (selected) predictors*
#'
print.cv.corila <- function(x, ...) {
  cat("object of class", sQuote("cv.corila"), "\n")
  content <- ifelse(length(x$model) == 1L, "an object", "multiple objects")
  cat("(contains ", content, " of class ", sQuote("cv.glmnet"), ")\n", sep = "")
  nzero <- sum(stats::coef(x, s = "lambda.min")[-1L] != 0)
  cat("selected", nzero, "from", x$args$p, "predictors")
  invisible(x)
}

#' @title
#' Summarising sparse group lasso (S3 method)
#'
#' @description
#' Summary method for class `"cv.corila"`.
#'
#' @inheritParams predict.cv.corila object
#'
#' @param x
#' object of class `"summary.cv.corila"`
#'
#' @param ...
#' (for compatibility with [base::summary])
#'
#' @return
#' Returns an invisible list with multiple slots.
#'
#' @examples
#' n <- 12 # decrease to 10 to check LOOCV
#' p <- 20
#' q <- 5
#' x <- matrix(rnorm(n * p), nrow = n, ncol = p)
#' y <- rnorm(n)
#' group <- rep(seq_len(q), length.out = p)
#' primary <- as.logical(rbinom(n = p, size = 1, prob = 0.5))
#' object <- cv.corila(x = x, y = y, group = group, primary = primary)
#' print(object)
#' summary(object)
#'
#' @seealso
#' [print.cv.corila()]
#'
#' @details
#' [print.summary.cv.corila()] uses the output from [summary.cv.corila()]
#' to print readable information to the console.
#' It calls the helper function [.type()] to name
#' the methods used to estimate initial and final coefficients.
#'
#' @export
#'
#' @srrstats{RE4.18} *summary method*
#'
summary.cv.corila <- function(object, ...) {
  list <- list()
  list$family <- object$args$family
  list$n <- object$args$n
  list$p <- object$args$p
  list$p_primary <- sum(object$args$primary)
  list$p_auxiliary <- sum(!object$args$primary)
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

#' @rdname summary.cv.corila
#' @keywords internal
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
  cat("initial coefficients:", .type(alpha = x$alpha_init), "\n")
  cat("final coefficients: adaptive", .type(alpha = x$alpha_final), "\n")
  cat("optimised regularisation parameter: lambda.min =",
      signif(x$lambda.min, digits = 4L), "\n")
  cat("selected weights: local = ", x$wgt_local,
      ", global = ", x$wgt_global, "\n", sep = "")
  cat("selected exponents: local = ", x$exp_local,
      ", global = ", x$exp_global, "\n", sep = "")
  cat(x$nzero, "non-zero coefficients",
      "(including intercept)"[x$family != "cox"])
  invisible(NULL)
}

#' @title
#' Deviance
#'
#' @description
#' Calculates the deviance.
#'
#' @inheritParams predict.cv.corila object
#'
#' @param ...
#' (for compatibility with [stats::deviance])
#'
#' @details
#' Returns the deviance calculated by [glmnet::deviance.glmnet()]
#' for the model with the optimised mixing and regularisation hyperparameters.
#'
#' @return
#' Returns a scalar.
#'
#' @seealso
#' The internal function [.deviance()] calculates
#' the deviance from fitted and observed values.
#'
#' @inherit methods examples
#'
#' @export
#'
#' @srrstats {RE4.11} *extract goodness-of-fit*
#'
deviance.cv.corila <- function(object, ...) {
  model <- object$model[[object$id_hyper]]
  id_lambda_min <- which.min(abs(model$lambda - object$lambda.min))
  stats::deviance(model)[id_lambda_min]
}

#' @title
#' Observation Count
#'
#' @description
#' Extracts the number of observations.
#'
#' @inheritParams predict.cv.corila object
#'
#' @param ...
#' (for compatibility with [stats::nobs])
#'
#' @return
#' Returns a positive integer.
#'
#' @inherit methods examples
#'
#' @importFrom stats nobs
#'
#' @export
#'
#' @srrstats {RE4.5} *number of observations*
#'
nobs.cv.corila <- function(object, ...) {
  object$args$n
}

#----- helper functions specific to S3 methods -----

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
#' Returns a numeric vector of length \eqn{1 + p}.
#'
#' @seealso
#' This function is called by [coef()][coef.cv.corila].
#'
#' @examples
#' \dontshow{.combine_slopes <- corila:::.combine_slopes}
#' p <- 10
#' alpha <- rnorm(1)
#' temp <- rnorm(p)
#' beta <- pmax(c(temp, -temp), 0)
#' .combine_slopes(alpha = alpha, beta = beta)
#'
#' @keywords internal
#'
.combine_slopes <- function(alpha, beta) {
  .assert(x = alpha, type = "numeric")
  .assert(x = beta, type = "numeric", dim = Inf, min = 0)
  beta_positive <- beta[1L:(length(beta) / 2L)]
  beta_negative <- beta[(length(beta) / 2L + 1L):(length(beta))]
  eps <- 1e-06
  if (any(beta_positive > eps & beta_negative > eps)) {
    # nocov start (invariant check)
    stop("No predictor may have a positive and a negative coefficient.")
    # nocov end
  }
  beta_combined <- beta_positive  - beta_negative
  c(alpha, beta_combined)
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
#' @param primary
#' logical vector of length \eqn{p_0 + p_1}
#' with \eqn{p_0} entries equal to `TRUE` (primary features)
#' and \eqn{p_1} entries equal to `FALSE` (auxiliary features)
#'
#' @return
#' Returns a matrix with \eqn{n} rows and \eqn{p_0 + p_1} columns.
#'
#' @seealso
#' This function is called by [predict()][predict.cv.corila()].
#'
#' @examples
#' \dontshow{.expand_auxiliary <- corila:::.expand_auxiliary}
#' n <- 5
#' p <- 10
#' x <- matrix(data = rnorm(n * p), nrow = n, ncol = p)
#' primary <- as.logical(rbinom(n = p, size = 1, prob = 0.5))
#' x_primary <- x[, primary]
#' x_expanded <- .expand_auxiliary(x = x_primary, primary = primary)
#' all(x_expanded[, primary] == x[, primary])
#' all(x_expanded[, !primary] == 0)
#'
#' @keywords internal
#'
.expand_auxiliary <- function(x, primary) {
  .assert(x = x, type = "numeric", dim = c(Inf, Inf), na.rm = TRUE)
  .assert(x = primary, type = "logical", dim = Inf)
  if (ncol(x) == length(primary)) {
    x[, !primary] <- 0
    x
  } else if (ncol(x) == sum(primary)) {
    full <- matrix(data = 0, nrow = nrow(x), ncol = length(primary))
    full[, primary] <- x
    full
  } else {
    stop("incompatible number of (primary) features")
  }
}

#' @title
#' Deviance Residuals
#'
#' @description
#' Calculates the deviance residuals.
#'
#' @param y_obs
#' \eqn{n}-dimensional vector of observed values
#'
#' @param y_fit
#' \eqn{n}-dimensional vector of fitted values or probabilities
#'
#' @param family
#' character `"gaussian"`, `"binomial"`, or `"poisson"`
#'
#' @details
#' This function is called by [residuals.cv.corila()].
#'
#' @return
#' Returns an \eqn{n}-dimensional vector.
#'
#' @examples
#' \dontshow{.residuals <- corila:::.residuals}
#' n <- 10L
#'
#' y_obs <- stats::rnorm(n = n)
#' y_fit <- stats::rnorm(n = n)
#' .residuals(y_obs = y_obs, y_fit = y_fit, family = "gaussian")
#'
#' y_obs <- stats::rbinom(n = n, size = 1L, prob = 0.2)
#' y_fit <- stats::runif(n = n)
#' .residuals(y_obs = y_obs, y_fit = y_fit, family = "binomial")
#'
#' y_obs <- stats::rpois(n = n, lambda = 4)
#' y_fit <- stats::rexp(n = n, rate = 0.25)
#' .residuals(y_obs = y_obs, y_fit = y_fit, family = "poisson")
#'
#' @keywords internal
#'
.residuals <- function(y_obs, y_fit, family) {
  if (is.character(family)) family <- tolower(family)
  .assert(x = family, type = "nominal",
          support = c("gaussian", "binomial", "poisson"))
  eps <- 1e-06
  if (identical(family, "gaussian")) {
    .assert(x = y_obs, type = "numeric", dim = Inf)
    .assert(x = y_fit, type = "numeric", dim = length(y_obs))
    y_obs - y_fit
  } else if (identical(family, "binomial")) {
    .assert(x = y_obs, type = "integer", dim = Inf, min = 0L, max = 1L)
    .assert(x = y_fit, type = "numeric", dim = length(y_obs), min = 0, max = 1)
    y_fit <- pmax(eps, pmin(y_fit, 1 - eps))
    sign(y_obs - y_fit) * sqrt(2) *
      sqrt(- y_obs * log(y_fit) - (1 - y_obs) * log(1 - y_fit))
  } else if (identical(family, "poisson")) {
    .assert(x = y_obs, type = "integer", dim = Inf, min = 0L)
    .assert(x = y_fit, type = "numeric", dim = length(y_obs), min = 0)
    sign(y_obs - y_fit) *
      sqrt((2 * (ifelse(test = abs(y_obs) < .Machine$double.eps,
                        yes = 0,
                        no = y_obs * log(y_obs / y_fit)) - y_obs + y_fit)))
  }
}

#' @title
#' Name method (helper function)
#'
#' @description
#' Names the method used for obtaining initial or final coefficients.
#'
#' @param alpha
#' elastic net mixing parameter
#' (numeric scalar, minimum \eqn{0} for ridge, maximum \eqn{1} for lasso)
#' or character string
#' (see `alpha_init` and `alpha_final` in [cv.corila()])
#'
#' @return
#' Returns a character string
#' (`"ridge regression"`, `"lasso regression"`, `"elastic net regression"`,
#' `"multi-penalty ridge regression"`,
#' or `"Pearson/Spearman/Kendall correlation"`)
#'
#' @seealso
#' This function is called by [print.summary.cv.corila()].
#'
#' @examples
#' \dontshow{.type <- corila:::.type}
#' .type(alpha = 0.0)
#'
#' @keywords internal
#'
.type <- function(alpha) {
  if (is.na(alpha)) {
    "none"
  } else if (is.numeric(alpha)) {
    if (alpha == 0.0) {
      "ridge regression"
    } else if (alpha == 1.0) {
      "lasso regression"
    } else if (alpha > 0.0 && alpha < 1.0) {
      "elastic net regression"
    } else {
      stop("If argument 'alpha' is numeric, ",
           "it should be in the unit interval.")
    }
  } else {
    if (identical(alpha, "multiridge")) {
      "multi-penalty ridge regression"
    } else if (alpha %in% c("pearson", "spearman", "kendall")) {
      paste0(toupper(substr(x = alpha, start = 1L, stop = 1L)),
             tolower(substr(x = alpha, start = 2L, stop = nchar(alpha))),
             " correlation")
    } else {
      stop("If argument 'alpha' is of type 'character', ",
           "it should equal ",
           "'pearson', 'spearman', 'kendall', or 'multiridge'.")
    }
  }
}
