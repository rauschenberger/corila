
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
#' \eqn{n \times p} numeric matrix
#'
#' @param y
#' response:
#' \eqn{n}-dimensional vector
#'
#' @param group
#' \eqn{p}-dimensional integer vector with entries in \eqn{\{1, \ldots, q\}}
#'
#' @param family
#' character `"linear"` (or `"gaussian"`),
#' `"logistic"` (or `"binomial"`), or `"cox"`
#'
#' @param penalties
#' \eqn{q}-dimensional vector of non-negative penalty parameters,
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
#' \doi{10.1080/10618600.2021.1904962}.
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
#' - \eqn{p}-dimensional group vector `group` (see argument)
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
#' data <- simulate_data()
#' 
#' ## standard model fitting
#' #model <- multiridge(x = data$x_train, y = data$y_train, group = data$group)
#' 
#' ## fitting with given folds
#' #foldid <- sample(seq_len(10L), size = nrow(data$x_train), replace = TRUE)
#' #model <- multiridge(x = data$x_train, y = data$y_train, group = data$group,
#' #                    foldid = foldid)
#' 
#' ## fitting with given penalties
#' #penalties <- abs(rnorm(length(unique(data$group))))
#' #model <- multiridge(x = data$x_train, y = data$y_train, group = data$group,
#' #                    penalties = penalties)
#'
#' @keywords methods models regression classif
#'
#' @export
multiridge <- function(x, y, group, family = "gaussian", foldid = NULL,
                       nfolds = 10L, penalties = NULL) {
  # --- check arguments ---
  if (is.character(family)) family <- tolower(family)
  if (is.matrix(x) && ncol(x) != length(group)) {
    stop("For each variable, 'x' should have one column, ",
         "and 'group' should have one entry.")
  }
  cond <- !is.null(penalties) && !is.null(group) &&
    length(unique(group)) != length(penalties)
  if (cond) {
    stop("Argument 'penalties' must have one entry for each group.")
  }
  .assert(x = x, type = "numeric", dim = c(Inf, Inf))
  .assert(x = y, type = "numeric", dim = nrow(x))
  .assert(x = group, type = "integer", dim = ncol(x),
          min = 1L, max = length(unique(group)))
  group <- as.integer(group)
  .assert(x = family, type = "nominal",
          support = c("gaussian", "binomial", "cox"))
  .assert(x = foldid, type = "integer", dim = nrow(x),
          min = 1L, max = nrow(x))
  if (!is.null(foldid)) foldid <- as.integer(foldid)
  .assert(x = nfolds, type = "integer", min = 2L, max = nrow(x))
  nfolds <- as.integer(nfolds)
  .assert(x = penalties, type = "numeric", dim = length(unique(group)), min = 0)
  #.validate(x = x, y = y, group = NULL, family = family)
  # --- initial regression ---
  scale <- .forescale(x = x, y = y, family = family)
  table <- c(gaussian = "linear", binomial = "logistic")
  model <- if (family %in% names(table)) table[[family]] else family
  xx <- lapply(X = unique(group),
               FUN = function(i) scale$x[, group == i, drop = FALSE])
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
  object$group <- group
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
  if (length(object$group) != ncol(newx)) {
    stop("Argument 'newx' must have one column",
         "for each variable used in model fitting.")
  }
  .assert(x = newx, type = "numeric", dim = c(Inf, length(object$group)))
  # --- make predictions ---
  scale <- .forescale(x = newx, pars = object$pars)
  newxx <- lapply(X = unique(object$group),
                  FUN = function(x) scale$x[, object$group == x])
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
  #if (identical(object$family, "cox") & is.null(coef[[1L]])) {
  #  coef[[1L]] <- NA # was 0
  #}
  .backscale(pars = object$pars, coef = unlist(coef))$coef
}
