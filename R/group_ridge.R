
#----- group-ridge -----

#' @title
#' Multi-penalty ridge regression
#'
#' @description
#' Fits multi-penalty ridge regression
#' (tuning regularisation hyperparameters
#' and estimating regression coefficients).
#' This is a wrapper function of some functions
#' from the [multiridge-package][multiridge::multiridge-package].
#'
#' @srrstats {G1.1} *simplified interface for an implemented algorithm*
#'
#' @inheritParams corila foldid nfolds
#'
#' @param x
#' predictors:
#' \eqn{n \times p} matrix
#'
#' @param y
#' response:
#' \eqn{n}-dimensional vector
#'
#' @param z
#' \eqn{p}-dimensional vector with entries in \eqn{\{1, \ldots, q\}}
#'
#' @param family
#' character `"linear"` (or `"gaussian"`),
#' `"logistic"` (or `"binomial"`), or `"cox"`
#'
#' @param penalties
#' \eqn{q}-dimensional vector of penalty parameters,
#' or `NULL` (cross-validation)
#'
#' @inherit corila details
#'
#' @references
#' [Mark A. van de Wiel](https://orcid.org/0000-0003-4780-8472),
#' [Mirrelijn M. van Nee](https://orcid.org/0000-0001-7715-1446)
#' and
#' [Armin Rauschenberger](https://orcid.org/0000-0001-6498-4801)
#' (2021).
#' "Fast cross-validation for multi-penalty high-dimensional ridge regression"
#' *Journal of Computational and Graphical Statistics*
#' 30(4):835-847.
#' [doi: 10.1080/10618600.2021.1904962](https://doi.org/10.1080/10618600.2021.1904962). # nolint: line_length_linter.
#' @srrstats {G1.0} *primary reference*
#'
#' @return
#' Returns an object of class `"multiridge"`,
#' a list with the following slots:
#' - slots from [IWLSridge()][multiridge::IWLSridge] or
#' [IWLSCoxridge()][multiridge::IWLSCoxridge]
#' - character `family`
#' with value `"gaussian"` (also for `"linear"`),
#' `"binomial"` (also for `"logistic"`), or `"cox"`
#' - \eqn{q}-dimensional vector `penalties`
#' containing optimised regularisation hyperparameters
#' (one for each predictor group)
#' - list `indices`
#' with `nfolds` slots (one for each cross-validation fold),
#' each containing the indices of the observations
#' - list `datablocks`
#' with \eqn{q} slots (one for each predictor group),
#' each containing an \eqn{n_0 \times p_k} matrix,
#' where \eqn{k \in \{1, \ldots, q\}}
#' - \eqn{p}-dimensional group vector `z` (see argument)
#' - list `pars` with slots `family` (see above),
#' the \eqn{p}-dimensional vectors `mu.x` and `sd.x`
#' and the scalars `mu.y` and `sd.y`
#'
#' @seealso
#' Extract coefficients with [coef()][coef.multiridge]
#' or make predictions with [predict()][predict.multiridge].
#' Use [cv.corila()] to estimate sparse models.
#'
#' This wrapper function calls various functions from the
#' [multiridge-package][multiridge::multiridge-package],
#' namely
#' [createXXblocks()][multiridge::createXXblocks],
#' [fastCV2()][multiridge::fastCV2],
#' [CVfolds()][multiridge::CVfolds],
#' [optLambdasWrap()][multiridge::optLambdasWrap],
#' [SigmaFromBlocks()][multiridge::SigmaFromBlocks],
#' [IWLSridge()][multiridge::IWLSridge], and
#' [IWLSCoxridge()][multiridge::IWLSCoxridge].
#'
#' The [multiridge-package][multiridge::multiridge-package]
#' accepts not only
#' an \eqn{n \times p} matrix but also
#' a list of length \eqn{q} of \eqn{n \times p_k} matrices,
#' with \eqn{k} in \eqn{\{1, \ldots, q\}}.
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
#' @keywords methods models regression classif
#'
#' @export
multiridge <- function(x, y, z, family = "gaussian", foldid = NULL, nfolds = 10,
                       penalties = NULL) {
  # --- check arguments ---
  if (is.matrix(x) && ncol(x) != length(z)) {
    stop("For each variable, 'x' should have one column, ",
         "and 'z' should have one entry.")
  }
  cond <- !is.null(penalties) && !is.null(z) &&
    length(unique(z)) != length(penalties)
  if (cond) {
    stop("Argument 'penalties' must have one entry for each group.")
  }
  .assert(x = x, type = "numeric", dim = c(Inf, Inf))
  .assert(x = y, type = "numeric", dim = nrow(x))
  .assert(x = z, type = "integer", dim = ncol(x),
          min = 1, max = length(unique(z)))
  .assert(x = family, type = "nominal",
          support = c("gaussian", "binomial", "cox"))
  .assert(x = foldid, type = "integer", dim = nrow(x),
          min = 1, max = nrow(x))
  .assert(x = nfolds, type = "integer", min = 2, max = nrow(x))
  .assert(x = penalties, type = "numeric", dim = length(unique(z)), min = 0)
  #.validate(x = x, y = y, group = NULL, family = family)
  # --- initial regression ---
  scale <- .forescale(x = x, y = y, family = family)
  table <- c(gaussian = "linear", binomial = "logistic")
  model <- if (family %in% names(table)) table[[family]] else family
  xx <- lapply(X = unique(z), FUN = function(i) scale$x[, z == i])
  xxblocks <- multiridge::createXXblocks(datablocks = xx)
  invisible(utils::capture.output({
    init <- multiridge::fastCV2(XXblocks = xxblocks,
                                Y = scale$y,
                                model = model)
  }))
  # --- cross-validation ---
  if (is.null(penalties)) {
    if (is.null(foldid)) {
      indices <- multiridge::CVfolds(Y = scale$y, model = model, kfold = nfolds)
    } else {
      indices <- lapply(X = seq_len(nfolds),
                        FUN = function(x) which(foldid == x))
    }
    invisible(utils::capture.output({
      final <- multiridge::optLambdasWrap(penaltiesinit = init$lambdas,
                                          XXblocks = xxblocks,
                                          Y = scale$y,
                                          folds = indices)
    }))
    penalties <- final$optpen
  } else {
    indices <- NULL
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
  object$family <- family
  object$penalties <- penalties
  object$datablocks <- xx
  object$indices <- indices
  object$z <- z
  object$pars <- scale$pars
  class(object) <- "multiridge"
  object
}

#' @title
#' Make predictions
#'
#' @description
#' Makes predictions from a multi-penalty ridge regression model.
#'
#' @inheritParams predict.cv.corila
#'
#' @param object
#' object of type `"multiridge"`
#'
#' @inherit multiridge references
#'
#' @return
#' Returns an \eqn{n_0}-dimensional vector of fitted values
#' or an \eqn{n_1}-dimensional vector of predicted values.
#'
#' @seealso
#' Fit models with [multiridge()]
#' and extract coefficients with [coef()][coef.multiridge()].
#'
#' @inherit multiridge examples
#'
#' @keywords methods
#'
#' @export
predict.multiridge <- function(object, newx, ...) {
  # --- check arguments ---
  if (length(object$z) != ncol(newx)) {
    stop("Argument 'newx' must have one column",
         "for each variable used in model fitting.")
  }
  .assert(x = newx, type = "numeric", dim = c(Inf, length(object$z)))
  # --- make predictions ---
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
#' Extract coefficients
#'
#' @description
#' Extracts coefficients from a multi-penalty ridge regression model.
#'
#' @inheritParams predict.multiridge object
#' 
#' @inheritParams coef.cv.corila
#'
#' @inherit multiridge references
#'
#' @return
#' Returns an \eqn{(1 + p)}-dimensional vector of estimated coefficients
#' (estimated intercept and estimated slopes).
#'
#' @seealso
#' Fit models with [multiridge()]
#' and make predictions with [predict()][predict.multiridge()].
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
