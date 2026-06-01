
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
  .backscale(pars = object$pars, y = y_hat)$y_original
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
