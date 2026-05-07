
#'@title
#'Standardisation
#'
#'@description
#'Transforming variables to mean 0 and variance 1.
#'
#'@inheritParams corila
#'@param y \eqn{n_0}-dimensional response vector or \code{NULL},
#'only for Gaussian family
#'@param family character string \code{"gaussian"}, \code{"binomial"},
#'\code{"poisson"}, or \code{"cox"};
#'or \code{NULL} (if \code{pars} is provided)
#'@param pars list as defined in section \emph{Value},
#'or \code{NULL} (if \code{family} is provided)
#'
#'@return
#'\itemize{
#'\item standardised \eqn{n_0 \times p} predictor matrix \eqn{x}
#'\item standardised \eqn{n_0}-dimensional response vector \eqn{y}
#'(only if \eqn{y} is provided and \code{family = "gaussian"}
#'or \code{pars$family = "gaussian"}; otherwise output equals input)
#'\item character string \code{family} indicates the model (\code{"gaussian"},
#'\code{"binomial"}, \code{"poisson"}, or \code{"cox"}),
#'determined by argument \code{family} or \code{pars$family}
#'\item list \code{pars} with slots \code{mu.x} and \code{sd.x}
#'(\eqn{p}-dimensional vectors of means and standard deviations
#'of the predictor variables),
#'and \code{mu.y} and \code{sd.y}
#'(mean and standard deviation of response variable for Gaussian family,
#'0 and 1 for other families)
#'}
#'
#'@seealso Use function \code{\link{backscale}()}
#'to bring coefficients and predictions back to original scale.
#'
#'@inherit backscale examples
#'@export
forescale <- function(x, y = NULL, family = NULL, pars = NULL) {
  if (!is.null(y)) {
    if (nrow(x) != length(y)) {
      stop(paste(
        "For each observation,",
        "\"x\" should have one row and \"y\" should have one entry."
      ))
    }
  }
  if (is.null(family) == is.null(pars)) {
    stop("Provide either family or pars.")
  } else {
    families <- c("gaussian", "binomial", "poisson", "cox")
    if (!c(family, pars$family) %in% families) {
      stop(paste(
        "Argument \"family\" must equal",
        "\"gaussian\", \"binomial\", \"poisson\", or \"cox\"."
      ))
    }
  }
  if (is.null(family)) {
    family <- pars$family
  }
  if (is.null(pars)) {
    pars <- list()
    pars$family <- family
    #if (family == "cox") {
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
    if (!is.null(y) && family == "gaussian") {
      pars$mu.y <- mean(y, na.rm = TRUE)
      pars$sd.y <- stats::sd(y, na.rm = TRUE)
    } else if (!is.null(y)) {
      pars$mu.y <- 0
      pars$sd.y <- 1
    }
  }
  x_scaled <- t((t(x) - pars$mu.x) / pars$sd.x)
  x_scaled[, pars$sd.x == 0] <- 0
  if (!is.null(y) && family == "gaussian") {
    y_scaled <- (y - pars$mu.y) / pars$sd.y
  } else if (!is.null(y)) {
    y_scaled <- y
  } else {
    y_scaled <- NULL
  }
  list(x = x_scaled, y = y_scaled, family = family, pars = pars)
}

#'@title
#'Inverse Standardisation
#'
#'@description
#'Transforms response variable back to original scale
#'or transforms coefficients for predictor variables and response variable
#'on original scales.
#'
#'@inheritParams forescale
#'@param y
#'\eqn{n_1}-dimensional response vector
#'@param coef
#'\eqn{(1 + p)-dimensional vector}
#'containing the estimated intercept
#'and the estimated slopes or \code{NULL} (default)
#'
#'@return
#'Returns a list with slots \code{y_original} or \code{coef}.
#'
#'@seealso \code{\link{forescale}()}
#'
#'@examples
#'# simulate data
#'family <- "cox"
#'n0 <- 100; n1 <- 50; p <- 3
#'n <- n0 + n1
#'fold <- rep(c(0, 1), times = c(n0, n1))
#'sd <- stats::rpois(n = p, lambda = 5)
#'x <- data.frame(x = sapply(X = sd,
#'                           FUN = function(x) stats::rnorm(n = n, sd = x)))
#'beta <- stats::rnorm(n = p)
#'eta <- as.matrix(x) %*% beta
#'if (family == "gaussian") {
#'  y <- stats::rnorm(n = n, mean = eta)
#'} else if (family == "binomial") {
#'  y <- stats::rbinom(n = n, size = 1, prob = 1 / (1 + exp(-eta)))
#'} else if (family == "poisson") {
#'  y <- stats::rpois(n = n, lambda = exp(eta))
#'} else if (family == "cox") {
#'  time <- stats::rexp(n = n, rate = exp(eta))
#'  status <- stats::rbinom(n = n, prob = 0.5, size = 1)
#'  y <- survival::Surv(time = time, event = status)
#'}
#'
#'# regression without standardisation
#'if (family == "cox") {
#'  lm1 <- survival::coxph(y[fold == 0]~., data=x[fold == 0, ])
#'} else {
#'  lm1 <- stats::glm(y[fold == 0]~., data=x[fold == 0, ], family=family)
#'}
#'coef1 <- stats::coef(lm1)
#'yhat1 <- predict(lm1, newdata = x[fold == 1, ])
#'
#'# regression with standardisation
#'scale <- forescale(x = x[fold == 0, ], y = y[fold == 0], family = family)
#'if (family == "cox") {
#'  lm2 <- survival::coxph(scale$y~., data = data.frame(scale$x))
#'} else {
#'  lm2 <- stats::glm(scale$y~., data = data.frame(scale$x), family = family)
#'}
#'coef_temp <- stats::coef(lm2)
#'newx_temp <- forescale(x = x[fold == 1, ], pars = scale$pars)$x
#'yhat_temp <- predict(object = lm2, newdata = data.frame(newx_temp))
#'result <- backscale(pars = scale$pars, y = yhat_temp, coef = coef_temp)
#'coef2 <- result$coef
#'yhat2 <- result$y_original
#'
#'# equality
#'all.equal(coef1, coef2, check.attributes = FALSE)
#'all.equal(yhat1, yhat2)
#'@export
backscale <- function(pars, y = NULL, coef = NULL) {
  list <- list()
  if (!is.null(y) && pars$family == "gaussian") {
    list$y_original <- pars$mu.y + pars$sd.y * y
  } else if (!is.null(y)) {
    list$y_original <- y
  }
  if (!is.null(coef)) {
    if (pars$family == "cox") {
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


#'@title
#'Fold Identifiers
#'
#'@description
#'Splits observations into balanced and stratified folds
#'
#'@inheritParams cv.corila
#'
#'@return
#'Returns an \eqn{n_1}-dimensional vector
#'with entries \eqn{\{1, \ldots, }\code{nfolds}\eqn{\}}
#'
#'@details
#'Randomly splits observations into balanced folds
#'(approximately the same number of observations per fold)
#'and stratified folds
#'(separate splitting for both classes in binomial family
#'or censored/uncensored observations in Cox model).
#'
#'@examples
#'# Gaussian and Poisson families
#'y <- stats::rnorm(n = 100)
#'foldid <- folds(y = y, family = "gaussian", nfolds = 10)
#'table(foldid)
#'
#'# binomial families
#'y <- stats::rbinom(n = 100, prob = 0.2, size = 1)
#'foldid <- folds(y = y, family = "binomial", nfolds = 10)
#'table(y, foldid)
#'
#'# Cox model
#'time <- stats::rexp(n = 100, rate = 5)
#'status <- stats::rbinom(n = 100, prob = 0.2, size = 1)
#'y <- survival::Surv(time = time, event = status)
#'foldid <- folds(y = y, family = "cox", nfolds = 10)
#'table(y[, "status"], foldid)
#'
#'@export
folds <- function(y, family, nfolds) {
  if (nfolds < 2) {
    stop("There must be at least two cross-validation folds.")
  } else if (length(y) < nfolds) {
    stop("There must be more observations than cross-validation folds.")
  }
  if (family %in% c("binomial", "logistic", "cox")) {
    if (family == "cox") {
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

check_args <- function(x, y, family) {
  if (!is.character(family) || length(family) != 1) {
    stop("Argument \"family\" must be a character string.")
  }
  if (!is.matrix(x)) {
    stop("Argument \"x\" must be a matrix.")
  }
  cond_vector <- is.vector(y) && is.numeric(y)
  cond_matrix <- is.matrix(y) && ncol(y) == 1
  if (!(family == "cox" || cond_vector || cond_matrix)) {
    stop("Argument \"y\" must be a vector.")
  }
  if (nrow(x) != length(y)) {
    stop("For each observation, matrix \"x\" must have one row,
         and vector \"y\" must have one entry.")
  }
  if (family %in% c("gaussian", "linear")) {
    if (all(y %in% c(0, 1)) || all(y %in% c(-1, 1))) {
      stop("Gaussian family requires a numeric outcome.")
    }
  } else if (family %in% c("binomial", "logistic")) {
    if (!all(y %in% c(0, 1))) {
      stop("Binomial family requires a binary outcome.")
    }
  } else if (family == "poisson") {
    if (any(y %% 1 != 0)) {
      stop("Poisson family requires a count outcome.")
    }
  } else if (family == "cox") {
    if (!inherits(x = y, what = "Surv")) {
      stop("Cox model requires a survival outcome.")
    }
  } else {
    stop("Invalid value for argument \"family\".")
  }
  NULL
}

#----- group-ridge -----

.mean_function <- function(x, family) {
  if (family %in% c("gaussian", "cox")) {
    x
  } else if (family == "binomial") {
    1 / (1 + exp(-x))
  } else if (family == "poisson") {
    exp(x)
  } else {
    stop("Family not implemented.")
  }
}

#'@title
#'Multi-Penalty Ridge Regression
#'
#'@description
#'Fits multi-penalty ridge regression
#'(tuning regularisation hyperparameters
#'and estimating regression coefficients).
#'
#'@param x
#'predictors:
#'\eqn{n \times p} matrix,
#'or list of length \eqn{q} of \eqn{n \times p_k} matrices,
#'with \eqn{k} in \eqn{\{1, \ldots, q\}}.
#'@param y
#'response:
#'\eqn{n}-dimensional vector
#'@param z
#'groups:
#'\eqn{p}-dimensional vector with entries in \eqn{\{1, \ldots, q\}}
#'(if \code{x} is a matrix),
#'or \code{NULL}
#'(if \code{x} is a list of matrices)
#'@param family
#'character \code{"linear"} (or \code{"gaussian"}),
#'\code{"logistic"} (or \code{"binomial"}),
#'or \code{"cox"}
#'@param penalties
#'\eqn{q}-dimensional vector of penalty parameters,
#'or \code{NULL} (cross-validation)
#'
#'@inherit corila details
#'
#'@references
#'\href{https://orcid.org/0000-0003-4780-8472}{Mark A. van de Wiel},
#'\href{https://orcid.org/0000-0001-7715-1446}{Mirrelijn M. van Nee},
#'and
#'\href{https://orcid.org/0000-0001-6498-4801}{Armin Rauschenberger}
#'(2021).
#'"Fast cross-validation for multi-penalty high-dimensional ridge regression"
#'\emph{Journal of Computational and Graphical Statistics}
#'30(4):835-847.
#'\href{https://doi.org/10.1080/10618600.2021.1904962}{doi: 10.1080/10618600.2021.1904962}. # nolint: line_length_linter.
#'
#'@return
#'Returns an object of class \code{"multiridge"},
#'a list with the following slots:
#'\itemize{
#'\item slots from \code{\link[multiridge]{IWLSridge}()} or
#'\code{\link[multiridge]{IWLSCoxridge}()}
#'\item character \code{family}
#'with value \code{"gaussian"} (also for \code{"linear"}),
#'\code{"binomial"} (also for \code{"logistic"}),
#'\code{"poisson"}, or \code{"cox"}
#'\item \eqn{q}-dimensional vector \code{penalties}
#'containing optimised regularisation hyperparameters
#'(one for each variable group)
#'\item list \code{datablocks}
#'with \eqn{q} slots (one for each variable group),
#'each containing an \eqn{n_0 \times p_k} matrix,
#'where \eqn{k \in \{1, \ldots, q\}}
#'\item \eqn{p}-dimensional group vector \code{z} (see argument)
#'\item list \code{pars} with slots \code{family} (see above),
#'the \eqn{n_0}-dimensional vectors \code{mu.x} and \code{sd.x}
#'and the scalars \code{mu.y} and \code{sd.y}
#'}
#'
#'@seealso
#'Extract coefficients with \code{\link[=coef.multiridge]{coef}()}
#'or make predictions with \code{\link[=predict.multiridge]{predict}()}.
#'Use \code{\link{cv.corila}()} to estimate sparse models.
#'
#'This wrapper function calls various functions from the
#'\code{\link[multiridge]{multiridge-package}},
#'namely
#'\code{\link[multiridge]{createXXblocks}()},
#'\code{\link[multiridge]{fastCV2}()},
#'\code{\link[multiridge]{CVfolds}()},
#'\code{\link[multiridge]{optLambdasWrap}()},
#'\code{\link[multiridge]{SigmaFromBlocks}()},
#'\code{\link[multiridge]{IWLSridge}()}, and
#'\code{\link[multiridge]{IWLSCoxridge}()}.
#'
#'@examples
#'\donttest{
#'# simulation
#'set.seed(1)
#'n0 <- 100
#'n1 <- 10000
#'n <- n0 + n1
#'p <- c(100, 50)
#'z <- rep(x = seq_along(p), times = p)
#'x <- sapply(X = z, FUN = function(x) stats::rnorm(n = n, sd = x))
#'beta <- stats::rnorm(n = sum(p), mean = 1, sd = 0) *
#'        stats::rbinom(n = sum(p), size = 1, prob = 0.2)
#'eta <- x %*% beta
#'family <- "gaussian"
#'if (family == "gaussian") {
#'  y <- eta + 0.5 * stats::rnorm(n = n, sd = stats::sd(eta))
#'} else if (family == "binomial") {
#'  y <- stats::rbinom(n = n, size = 1, prob = 1 / (1 + exp(-eta)))
#'} else if (family == "cox") {
#'  time <- stats::rexp(n = n, rate = exp(eta))
#'  status <- stats::rbinom(n = n, prob = 0.5, size = 1)
#'  y <- survival::Surv(time = time, event = status)
#'}
#'cond <- rep(x = c(TRUE, FALSE), times = c(n0, n1))
#'
#'y_hat <- coef <- list()
#'
#'# standard ridge regression
#'object <- glmnet::cv.glmnet(x = x[cond, ], y = y[cond],
#'                            family = family, alpha = 0)
#'coef$glmnet <- stats::coef(object = object, s = "lambda.min")
#'y_hat$glmnet <- stats::predict(object = object, newx = x[!cond, ],
#'                               type = "response", s = "lambda.min")
#'
#'# multi-penalty ridge regression
#'object <- multiridge(x = x[cond, ], y = y[cond], z = z, family = family)
#'coef$multiridge <- stats::coef(object = object)
#'y_hat$multiridge <- stats::predict(object = object, newx = x[!cond, ])
#'
#'# estimation performance
#'sapply(coef, function(x) stats::cor(beta, x[-1]))
#'sapply(coef, function(x) mean((beta-x[-1])^2))
#'
#'# predictive performance
#'if (family == "gaussian") {
#'  metric <- sapply(X = y_hat, FUN = function(x)
#'    mean((x-y[!cond])^2))
#'} else if (family == "binomial") {
#'  metric <- sapply(X = y_hat, FUN = function(x)
#'    pROC::auc(response = y[!cond],
#'              predictor = as.vector(x),
#'              levels = c(0, 1),
#'              direction = "<"))
#'} else if (family == "cox") {
#'  metric <- sapply(X = y_hat, FUN = function(x)
#'    survival::concordance(y[!cond]~I(-x))$concordance)
#'}
#'metric
#'}
#'@export
multiridge <- function(x, y, z, family, penalties = NULL) {
  check_args(x = x, y = y, family = family)
  if(family == "poisson"){
    stop("Argument family=\"poisson\" is not implemented.")
  }
  if (is.matrix(x) && ncol(x) != length(z)) {
    stop(paste(
      "For each variable,",
      "\"x\" should have one column",
      "and \"z\" should have one entry."
    ))
  }
  cond <- !is.null(penalties) && !is.null(z) &&
    length(unique(z)) != length(penalties)
  if (cond) {
    stop("Argument \"penalties\" must have one entry for each group.")
  }
  scale <- forescale(x = x, y = y, family = family)
  model <- ifelse(family == "gaussian",
                  yes = "linear",
                  no = ifelse(family == "binomial",
                              yes = "logistic",
                              no = family))
  xx <- lapply(X = unique(z), FUN = function(i) scale$x[, z == i])
  xxblocks <- multiridge::createXXblocks(datablocks = xx)
  invisible(utils::capture.output(
    init <- multiridge::fastCV2(XXblocks = xxblocks,
                                Y = scale$y,
                                model = model)
  ))
  if (is.null(penalties)) {
    folds <- multiridge::CVfolds(Y = scale$y)
    invisible(utils::capture.output(
      final <- multiridge::optLambdasWrap(penaltiesinit = init$lambdas,
                                          XXblocks = xxblocks,
                                          Y = scale$y,
                                          folds = folds)
    ))
    penalties <- final$optpen
  }
  xxt <- multiridge::SigmaFromBlocks(XXblocks = xxblocks,
                                     penalties = penalties)
  if (family == "cox") {
    object <- multiridge::IWLSCoxridge(XXT = xxt,
                                       Y = scale$y)
  } else {
    object <- multiridge::IWLSridge(XXT = xxt,
                                    Y = scale$y,
                                    model = model)
  }
  object$family <- ifelse(family == "linear",
                          yes = "gaussian",
                          no = ifelse(family == "logistic",
                                      yes = "binomial",
                                      no = family))
  object$penalties <- penalties
  object$datablocks <- xx
  object$z <- z
  object$pars <- scale$pars
  class(object) <- "multiridge"
  object
}

#'@title
#'Make Predictions
#'
#'@description
#'Makes predictions from a multi-penalty ridge regression model.
#'
#'@inheritParams coef.multiridge
#'@inheritParams predict.corila
#'
#'@inherit multiridge references
#'
#'@return
#'Returns an \eqn{n_0}-dimensional vector of fitted values
#'or an \eqn{n_1}-dimensional vector of predicted values.
#'
#'@seealso
#'Fit models with \code{\link{multiridge}()}
#'and extract coefficients with \code{\link{coef.multiridge}()}.
#'
#'@inherit multiridge examples
#'
#'@export
predict.multiridge <- function(object, newx, ...) {
  if (length(object$z) != ncol(newx)) {
    stop(paste(
      "Argument \"newx\" must have one column",
      "for each variable used in model fitting."
    ))
  }
  scale <- forescale(x = newx, pars = object$pars)
  newxx <- lapply(X = unique(object$z),
                  FUN = function(x) scale$x[, object$z == x])
  xxblocks <- multiridge::createXXblocks(datablocks = object$datablocks,
                                         datablocksnew = newxx)
  sigmanew <- multiridge::SigmaFromBlocks(XXblocks = xxblocks,
                                          penalties = object$penalties)
  eta <- multiridge::predictIWLS(IWLSfit = object, Sigmanew = sigmanew)
  if (object$family == "cox") {
    y_hat <- exp(eta)
  } else {
    y_hat <- .mean_function(x = eta, family = object$family)
  }
  y_hat <- backscale(pars = object$pars, y = y_hat)$y
  y_hat
}

#'@title
#'Extract Coefficients
#'
#'@description
#'Extracts coefficients from a multi-penalty ridge regression model.
#'
#'@param object object of class \code{"multiridge"}
#'@param ... (not used)
#'
#'@inherit multiridge references
#'
#'@return
#'Returns an \eqn{(1 + p)}-dimensional vector of estimated coefficients
#'(estimated intercept and estimated slopes).
#'
#'@seealso
#'Fit models with \code{\link{multiridge}()}
#'and make predictions with \code{\link{predict.multiridge}()}.
#'
#'@inherit multiridge examples
#'
#'@export
coef.multiridge <- function(object, ...) {
  xblocks <- multiridge::createXblocks(datablocks = object$datablocks)
  coef <- multiridge::betasout(object,
                               Xblocks = xblocks,
                               penalties = object$penalties)
  #if (object$family == "cox" & is.null(coef[[1]])) {
  #  coef[[1]] <- NA # was 0
  #}
  backscale(pars = object$pars, coef = unlist(coef))$coef
}

#----- group-lasso -----

.deviance <- function(y_hat, y, family) {
  eps <- 1e-06
  if (family == "gaussian") {
    mean((y_hat - y)^2)
  } else if (family == "binomial") {
    mean(
      -y * log(pmax(y_hat, eps)) - (1 - y) * log(1 - pmin(y_hat, 1 - eps))
    )
  } else if (family == "cox") {
    glmnet::coxnet.deviance(pred = y_hat, y = y)
  } else if (family == "poisson") {
    mean(2 * (ifelse(y == 0, 0, y * log(y / y_hat)) - y + y_hat))
  } else {
    stop(paste0("Family \"", family, "\" is not implemented."))
  }
}


#'@title
#'Group lasso
#'
#'@description
#'Fits an initial ridge regression to obtain weights
#'for an adaptive lasso regression
#' that allows for heterogeneous, overlapping and unknown groups
#' of correlated variables.
#'
#'@param x
#'\eqn{n_0 \times p} predictor matrix,
#'where \eqn{n_0} is the number of observations used for model training
#'and \eqn{p} is the number of variables
#'@param y
#'\eqn{n_0}-dimensional response vector,
#'where \eqn{n_0} is the number of observations used for model training
#'@param group
#'\emph{(i)} \eqn{p}-dimensional vector of group indices
#'(in \eqn{\{1, \ldots, q\}}) or labels,
#'\emph{(ii)} list with \eqn{q} slots containing the variable indices
#'(in \eqn{\{1, \ldots, p\}}) or labels,
#'or \emph{(iii)} \eqn{p \times p} matrix,
#'where the entry in the \eqn{j^{\text{th}}} row
#'and the \eqn{k^{\text{th}}} column
#'indicates whether information should be transferred
#'from the \eqn{j^{\text{th}}} to the \eqn{k^{\text{th}}} variable
#'@param include
#'\eqn{p}-dimensional logical vector
#'indicating whether a predictor may be included in the final model
#'(\code{TRUE}, "primary predictors")
#'or must be excluded from the final model
#'(\code{FALSE}, "auxiliary predictors") 
#'@param alpha_init
#'elastic net mixing parameter
#'(\eqn{0 \leq} \code{alpha_init} \eqn{\leq 1})
#'for initial regression
#'(default: ridge penalisation with \code{alpha_init}=0);
#'alternative choices are
#'"pearson", "spearman", or "kendall"
#'to use initial correlation coefficients
#'(not implemented for \code{family="cox"}),
#'"multiridge" for multi-penalty ridge regression
#'with one penalty for each group
#'(not implemented for \code{family="poisson"} or overlapping groups),
#'or \code{NA} to set all initial coefficients equal to 1
#'@param alpha_final
#'elastic net mixing parameter for final regression
#'(default: lasso penalisation with \code{alpha_final}=1)
#'@param family
#'character string \code{"gaussian"}, \code{"binomial"},
#'\code{"poisson"}, or \code{"cox"}
#'@param hyper
#'list of of \eqn{m}-dimensional vectors
#'or a data frame with \eqn{m} rows
#'containing candidate values
#'for the regularisation and mixing hyperparameters
#'@param cor
#'character string \code{"pearson"},
#'\code{"spearman"} (default),
#'or \code{"kendall"};
#'or \eqn{p \times p} correlation matrix
#'@param lambda_init regularisation hyperparameter(s),
#'or \code{NULL} (cross-validation)
#'
#'@details
#'The number of observations (samples) for training or testing
#'are indicated by \eqn{n_0} and \eqn{n_1}, respectively,
#'the number of variables (features) is indicated by \eqn{p},
#'and the number of variable groups is indicated by \eqn{q}.
#'
#'Observations (samples) are indexed by \eqn{i} in \eqn{\{1, \ldots, n\}},
#'variables (features) are indexed by \eqn{j} in \eqn{\{1, \ldots, p\}},
#'and variable groups are indexed by \eqn{k} in \eqn{\{1, \ldots, q\}}.
#'
#'The number of variables in the \eqn{k^{\text{th}}} group
#'is indicated by \eqn{p_k}, with \eqn{\sum_{k=1}^q p_k = p}.
#'
#'@return
#'Returns an object of class \code{"corila"}.
#'
#'@inherit corila-package references
#'
#'@seealso
#'Estimate parameters and tune hyperparameters (using cross-validation)
#'with \code{\link{cv.corila}()}.
#'Make predictions for a range of hyperparameters
#'with \code{\link{predict.corila}()}.
#'
#'This function calls
#'\code{\link{forescale}()} and \code{\link{backscale}()}
#'for standardising data and bringing results back to the original scale,
#'respectively,
#'\code{\link{multiridge}()} for obtaining initial group penalties,
#'and \code{\link[glmnet]{cv.glmnet}()} and \code{\link[glmnet]{glmnet}()}
#'for adaptive lasso regression.
#'
#'@examples
#'\donttest{
#'# simulation
#'n <- 100
#'p <- 50
#'group <- rep(x = 1:10, each = 5)
#'include <- NULL
#'x <- matrix(data = rnorm(n * p), nrow = n, ncol = p)
#'y <- rnorm(n = n)
#'
#'# model fitting
#'hyper <- data.frame(exp_local = 1, wgt_local = 0.5,
#'                    exp_global = 1, wgt_global = 0.5)
#'object <- corila(x, y, group, include, family = "gaussian", hyper = hyper)
#'
#'y_hat <- stats::predict(object, newx = x, index = 1, s = 0)
#'}
#'@export
corila <- function(x, y, group, include, family, hyper, alpha_init = 0,
                   alpha_final = 1, cor = "spearman", lambda_init = NULL) {
  #lambda_init = NULL;mode<-"mean";cor = "spearman"
  if (is.character(alpha_init) &&
        alpha_init  == "multiridge" &&
        family == "poisson") {
    warning("Setting alpha_init=0 due to family=\"poisson\".")
    alpha_init <- 0
  }

  #n <- nrow(x) # sample size
  p <- ncol(x) # number of features

  if (is.null(group)) {
    group <- seq_len(p)
  }
  if (is.null(include)) {
    include <- rep(x = TRUE, times = p)
  } else {
    if (!is.logical(include)) {
      stop("Argument \"include\" should be a logical vector (or NULL).")
    }
  }
  if (!is.character(family) || length(family) != 1) {
    stop("Argument \"family\" must be a character vector of length 1.")
  }
  if (!family %in% c("gaussian", "binomial", "poisson", "cox")) {
    stop(paste("Argument \"family\" must equal",
               "\"gaussian\", \"binomial\", \"poisson\", or \"cox\"."))
  }
  #if (length(group) != p) {
  # stop("Argument \"group\" must be a vector of length p.")
  #}
  if (is.numeric(group) && !is.array(group)) {
    q <- length(unique(group)) # number of groups = number of unique values
  } else if (is.list(group)) {
    q <- length(group) # number of groups = number of slots
  }
  if (is.numeric(group) && !is.array(group)) {
    if (length(group) != p ||
          max(group) != q ||
          any(sort(unique(group)) != seq(from = 1, to = max(group), by = 1))) {
      stop(paste("Argument \"group\" should be of length p,",
                 "with all entries in {1, ..., q}."))
    }
  } else {
    if (is.character(group[[1]])) {
      #test <- lapply(group, function(slot)
      # sapply(slot, function(entry) which(colnames(x) == entry)))
      warning("Implement this.")
    }
  }

  scale <- forescale(x = x, y = y, family = family)
  rm(x, y)

  #--- initial coefficients ---
  fit_init <- NULL
  cor_methods <- c("pearson", "spearman", "kendall")
  if (all(is.na(alpha_init))) {
    coef_init <- rep(x = 1, times = p) # Remove this confusing option?
  } else if (is.character(alpha_init) && alpha_init == "multiridge") {
    if (is.null(lambda_init)) {
      fit_init <- multiridge(x = scale$x,
                             y = scale$y,
                             z = group,
                             family = family)
      coef_init <- stats::coef(object = fit_init,
                               s = "lambda.min")[-1]
      lambda_init <- fit_init$penalties
    } else {
      fit_init <- multiridge(x = scale$x,
                             y = scale$y,
                             z = group,
                             family = family,
                             penalties = lambda_init)
      coef_init <- stats::coef(object = fit_init)[-1]
    }
  } else if (is.character(alpha_init) && alpha_init %in% cor_methods) {
    coef_init <- stats::cor(x = scale$x,
                            y = scale$y,
                            method = alpha_init,
                            use = "pairwise.complete")
    coef_init[is.na(coef_init)] <- 0
  } else if (is.numeric(alpha_init) && alpha_init >= 0 && alpha_init <= 1) {
    cond_coef <- rep(c(FALSE, TRUE), times = c(family != "cox", p))
    if (is.null(lambda_init)) {
      fit_init <- glmnet::cv.glmnet(x = scale$x,
                                    y = scale$y,
                                    family = family,
                                    alpha = alpha_init)
      coef_init <- stats::coef(object = fit_init,
                               s = "lambda.min")[cond_coef]
      lambda_init <- fit_init$lambda.min
    } else {
      fit_init <- glmnet::glmnet(x = scale$x,
                                 y = scale$y,
                                 family = family,
                                 alpha = alpha_init)
      coef_init <- stats::coef(object = fit_init,
                               s = lambda_init)[cond_coef]
    }
  } else {
    stop("Invalid value for agrument \"alpha_init\".")
  }
  rm(fit_init)

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
      if (is.numeric(group) && !is.array(group)) {
        cond_temp <- group[j] == group
      } else if (is.list(group)) {
        group_index <- vapply(X = group,
                              FUN = function(x) j %in% x,
                              FUN.VALUE = logical(1))
        #names(group_index) <- group
        cond_temp <- seq_len(p) %in% unlist(group[group_index])
      } else if (is.matrix(group)) {
        cond_temp <- group[, j] == 1
      }
      cor_trans <- sign(cor[, j]) * abs(cor[, j])^hyper$exp_local[i]
      temp <-  cor_trans * coef_init * cond_temp
      weight$local[j] <- sum(pmax(0, temp)[cond_temp]) / sum(cond_temp)
      weight$local[p + j] <- sum(pmax(0, -temp)[cond_temp]) / sum(cond_temp)

      # ad-hoc solution for features that are in no group:
      weight$local[is.na(weight$com)] <- 0 # Consider 0 and weight$ind

      # all features
      temp <- sign(cor[, j]) * abs(cor[, j])^hyper$exp_global[i] * coef_init
      weight$global[j] <- sum(pmax(0, temp)) / p
      weight$global[p + j] <- sum(pmax(0, -temp)) / p
    }
    # # temporary code with beta distribution:
    # temp <- sign(cor[, j]) *
    # stats::qbeta(p = abs(cor[, j]),
    # shape1 = hyper$alpha[i],
    # shape2 = hyper$beta[i]) * coef_init * cond_temp
    weight <- lapply(weight, function(x) p * ifelse(x == 0, 0, x / sum(x)))
    pf_ext <- 1 / (weight$local * hyper$wgt_local[i] +
                     weight$global * hyper$wgt_global[i])
    # To obtain standard lasso set pf_ext equal to 1.
    pf_ext[!c(include, include)] <- Inf # excluded features
    if (any(is.na(pf_ext))) {
      stop("missing pf:", sum(is.na(pf_ext)))
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
      include = include,
      lambda_init = lambda_init,
      scale = scale$pars
    ),
    class = "corila"
  )
}

#'@title
#'predict (S3 method)
#'
#'@description
#'Makes prediction from an object of class \code{"corila"}.
#'
#'@inheritParams predict.cv.corila
#'
#'@param object
#'object of class \code{"corila"}
#'@param index
#'integer scalar specifying the index of the mixing hyperparameter(s)
#'@param s
#'numeric scalar specifying the value of the regularisation hyperparameter
#'@param ... (not used)
#'
#'@return
#'Returns fitted or predicted values in an
#'\eqn{n_0}-dimensional or \eqn{n_1}-dimensional vector, respectively.
#'
#'@inherit corila-package references
#'
#'@seealso
#'Estimate parameters with \code{\link{corila}()},
#'or estimate parameters and tune hyperparameters
#'with \code{\link{cv.corila}()}.
#'
#'@inherit corila examples
#'
#'@export
predict.corila <- function(object, newx, index, s, ...) {
  newx_stand <- forescale(x = newx, pars = object$scale)$x
  y_hat_stand <- stats::predict(object = object$model[[index]],
                                newx = cbind(newx_stand, -newx_stand),
                                s = s,
                                type = "response")
  #type = ifelse(object$scale$family == "cox", "link", "response"))
  y_hat <- backscale(y = y_hat_stand, pars = object$scale)$y
  y_hat
}

#'@title
#'Sparse Group Lasso
#'
#'@description
#'Optimises the parameters and the hyperparameters of the sparse group lasso.
#'
#'@inheritParams corila
#'@param foldid \eqn{n}-dimensional vector containing the fold identifiers
#'@param nfolds integer specifying the number of folds
#'@param tune character \code{"wgt"}, \code{"exp"}, or \code{"both"}
#'for determining the candidate values for the hyperparameters;
#'or list with slots \code{wgt_local}, \code{wgt_global}, \code{exp_local},
#'and \code{exp_global} (not yet implemented)
#'
#'@inherit corila details
#'
#'@return
#'Returns an object of class \code{cv.corila},
#'a list with the following slots:
#'\itemize{
#'\item \code{object}:
#'list with one slot for each combination of hyperparameters,
#'each slot contains an object of class \code{"glmnet"}
#'\item \code{hyper}:
#'data frame with one row for each combination of hyperparameters,
#'four columns for the values of the hyperparameters
#'(\code{wgt_local}, \code{wgt_global},
#'\code{exp_global}, and \code{exp_local})
#'and a column for the cross-validated loss (\code{cvm})
#'\item \code{id_hyper}:
#'index of combination of hyperparameters
#'leading to the lowest cross-validated loss
#'\item \code{lambda.min}
#'optimised regularisation hyperparameter
#'\item \code{scale}:
#'output from \code{\link{forescale}()}
#'}
#'
#'@inherit corila-package references
#'
#'@seealso
#'Extract coefficients with \code{\link[=coef.cv.corila]{coef}()}
#'and make predictions with \code{\link[=predict.cv.corila]{predict}()}.
#'
#'This user function repeatedly calls \code{\link{corila}()}
#'with different values for the regularisation and mixing hyperparameters.
#'
#'@examples
#'\donttest{
#'# simulation
#'set.seed(1)
#'n0 <- 100
#'n1 <- 10000
#'n <- n0 + n1
#'p <- c(100, 50)
#'z <- rep(x = seq_along(p), times = p)
#'x <- sapply(X = z, FUN = function(x) stats::rnorm(n = n, sd = x))
#'beta <- stats::rnorm(n = sum(p), mean = 1, sd = 0) *
#'        stats::rbinom(n = sum(p), size = 1, prob = 0.2)
#'eta <- x %*% beta
#'family <- "gaussian"
#'if (family == "gaussian") {
#'  y <- eta + 0.5 * stats::rnorm(n = n, sd = stats::sd(eta))
#'} else if (family == "binomial") {
#'  y <- stats::rbinom(n = n, size = 1, prob = 1 / (1 + exp(-eta)))
#'} else if (family == "cox") {
#'  time <- stats::rexp(n = n, rate = exp(eta))
#'  status <- stats::rbinom(n = n, prob = 0.5, size = 1)
#'  y <- survival::Surv(time = time, event = status)
#'}
#'cond <- rep(x = c(TRUE, FALSE), times = c(n0, n1))
#'
#'y_hat <- coef <- list()
#'
#'# standard lasso regression
#'object <- glmnet::cv.glmnet(x = x[cond, ], y = y[cond],
#'                            family = family, alpha = 1)
#'coef$glmnet <- stats::coef(object = object, s = "lambda.min")
#'y_hat$glmnet <- stats::predict(object = object, newx = x[!cond, ],
#'                               type = "response", s = "lambda.min")
#'
#'# flexible group lasso regression
#'object <- cv.corila(x = x[cond, ], y = y[cond], group = z, family = family)
#'coef$corila <- stats::coef(object = object)
#'y_hat$corila <- stats::predict(object = object, newx = x[!cond, ])
#'
#'# selection performance
#'sapply(coef, function(x) mean(sign(x[-1]) == sign(beta)))
#'sapply(coef, function(x) {
#'  sum(sign(x[-1]) != 0 & sign(x[-1]) == sign(beta)) / sum(x[-1] != 0)
#'})
#'
#'# predictive performance
#'if (family == "gaussian") {
#'  metric <- sapply(X = y_hat, FUN = function(x)
#'    mean((x-y[!cond])^2))
#'} else if (family == "binomial") {
#'  metric <- sapply(X = y_hat, FUN = function(x)
#'    pROC::auc(response = y[!cond],
#'              predictor = as.vector(x),
#'              levels = c(0, 1),
#'              direction = "<"))
#'} else if (family == "cox") {
#'  metric <- sapply(X = y_hat, FUN = function(x)
#'    survival::concordance(y[!cond]~I(-x))$concordance)
#'}
#'metric
#'
#'# privileged information
#'#include <- stats::rbinom(n = sum(p), size = 1, prob = 0.5) == 1
#'#object <- cv.corila(x = x[cond, ], y = y[cond], group = z,
#'#                     include = include, family = family)
#'}
#'@export
cv.corila <- function(x, y, group, include = NULL, alpha_init = 0,
                      alpha_final = 1, family = "gaussian",
                      nfolds = 10, cor = "spearman", tune = "both",
                      foldid = NULL) {
  check_args(x = x, y = y, family = family)
  if (is.null(include)) {
    include <- rep(x = TRUE, times = ncol(x))
  }
  # family = "gaussian"; foldid = NULL
  n <- nrow(x) # sample size
  #p <- ncol(x) # number of features
  #p <- length(unique(group)) # number of groups
  # GENERAL FORMULATION
  if (tune == "none") {
    hyper <- data.frame(wgt_local = 1,
                        exp_local = 1,
                        wgt_global = 0,
                        exp_global = Inf)
  } else if (tune == "trial") {
    wgt_cand <- seq(from = 0, to = 1, by = 0.1)
    hyper <- data.frame(wgt_local = wgt_cand,
                        exp_local = 0,
                        wgt_global = 1 - wgt_cand,
                        exp_global = 1)
  } else if (tune == "wgt") {
    wgt_cand <- seq(from = 0, to = 1, by = 0.1) # for weighted sums
    hyper <- data.frame(wgt_local = wgt_cand,
                        exp_local = 1,
                        wgt_global = 1 - wgt_cand,
                        exp_global = 1)
  } else if (tune == "exp") {
    exp_cand <- c(0, 0.1, 0.25, 1 / 3, 0.5, 1, 2, 3, 4, 10, Inf)
    hyper <- data.frame(wgt_local = 1,
                        exp_local = exp_cand,
                        wgt_global = 0,
                        exp_global = exp_cand)
  } else if (tune == "sep") {
    exp_cand <- c(0.1, 0.5, 1, 2, 10)
    hyper <- expand.grid(exp_local = exp_cand,
                         exp_global = exp_cand)
    hyper$wgt_local <- hyper$wgt_global <- 0.5
  } else if (tune == "both") {
    #wgt_cand <- seq(from = 0, to = 1, by = 0.25) # original
    wgt_cand <- seq(from = 0, to = 1, by = 0.1) # trial
    hyper <- data.frame(wgt_local = wgt_cand,
                        wgt_global = 1 - wgt_cand)
    #exp_cand <- c(0.1, 0.5, 1, 2, 10) # original
    exp_cand <- c(0.1, 0.5, 0.8, 1, 1.25, 2, 10)
    hyper <- hyper[rep(seq_len(nrow(hyper)), each = length(exp_cand)), ]
    hyper$exp_local <- hyper$exp_global <- exp_cand
  } else if (tune == "all") {
    wgt_cand <- seq(from = 0, to = 1, by = 0.25)
    exp_cand <- c(0.1, 0.5, 1, 2, 10)
    hyper <- expand.grid(wgt_local = wgt_cand,
                         exp_local = exp_cand,
                         exp_global = exp_cand)
    hyper$wgt_global <- 1 - hyper$wgt_local
    hyper$exp_local[hyper$wgt_local == 0] <- Inf
    hyper$exp_global[hyper$wgt_global == 0] <- Inf
    hyper <- unique(hyper)
    rownames(hyper) <- seq_len(nrow(hyper))
  }

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

  if (is.null(foldid)) {
    #foldid <- sample(rep(x = seq_len(nfolds), length.out = n))
    # balanced/stratified folds
    foldid <- folds(y = y, family = family, nfolds = nfolds)
  }

  # Use foldid already for full run?
  object_ext <- corila(x = x,
                       y = y,
                       group = group,
                       include = include,
                       alpha_init = alpha_init,
                       alpha_final = alpha_final,
                       family = family,
                       cor = cor,
                       hyper = hyper)
  lambda <- lapply(X = object_ext$model, FUN = function(x) x$lambda)

  hat <- list()
  for (j in seq_len(nrow(hyper))) {
    hat[[j]] <- matrix(data = NA,
                       nrow = n,
                       ncol = length(object_ext$model[[j]]$lambda))
  }

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
      hat[[j]][foldid == i, ] <- stats::predict(object = object_int,
                                                newx = x[foldid == i, ],
                                                index = j,
                                                s = lambda[[j]])
    }
  }

  cvm <- list()

  for (l in seq_len(nrow(hyper))) {
    cvm[[l]] <- apply(
      X = hat[[l]],
      MARGIN = 2,
      FUN = function(x) .deviance(y_hat = x, y =  y, family = family)
    )
  }
  hyper$cvm <- cvm_min <- vapply(X = cvm,
                                 FUN = base::min,
                                 FUN.VALUE = numeric(1))
  id_hyper <- which.min(cvm_min)
  lambda.min <- object_ext$model[[id_hyper]]$lambda[which.min(cvm[[id_hyper]])]

  structure(
    list(
      object = object_ext$model,
      include = include,
      hyper = hyper,
      id_hyper = id_hyper,
      lambda.min = lambda.min,
      scale = object_ext$scale
    ),
    class = "cv.corila"
  )
}

#'@title
#'predict (S3 method)
#'
#'@description
#'Makes predictions from an object of class \code{"cv.corila"}.
#'
#'@param object
#'object of class \code{"cv.corila"}
#'@param newx
#'\eqn{n_0 \times p} predictor matrix (training data)
#'to obtain fitted values,
#'\eqn{n_1 \times p} predictor matrix (testing data)
#'to obtain predicted values
#'@param s character \code{"lambda.min"} or numeric value
#'@param ... (not used)
#'
#'@inherit predict.corila return
#'
#'@inherit corila-package references
#'
#'@seealso
#'Fit models with \code{\link{cv.corila}()}
#'and extract coefficients with \code{\link{coef.cv.corila}()}.
#'
#'@inherit cv.corila examples
#'@export
predict.cv.corila <- function(object, newx, s = "lambda.min", ...) {
  if (s == "lambda.min") {
    s <- object$lambda.min
  } else if (!is.numeric(s) || length(s) != 1) {
    stop("Set s=\"lambda.min\" or provide numeric value.")
  }
  if (any(object$include == 0) && sum(object$include) == ncol(newx)) {
    full <- matrix(data = 0, nrow = nrow(newx), ncol = length(object$include))
    full[, object$include] <- newx
    newx <- full
  }
  newx_stand <- forescale(x = newx, pars = object$scale)$x
  x_all <- cbind(newx_stand, -newx_stand)
  y_hat_stand <- stats::predict(object = object$object[[object$id_hyper]],
                                newx = x_all,
                                s = s,
                                type = "response")
  backscale(y = y_hat_stand, pars = object$scale)$y
}

#'@title
#'Extract coefficients
#'
#'@description
#'Extracts coefficients from an object of class \code{"cv.corila"}.
#'
#'@inheritParams predict.cv.corila
#'
#'@return
#'Returns an \eqn{(1 + p)}-dimensional vector of the estimated coefficients.
#'The first entry is the estimated intercept,
#'and the other \eqn{p} entries are the estimated slopes.
#'
#'@inherit corila-package references
#'
#'@seealso
#'Fit models with \code{\link{cv.corila}()}
#'and make predictions with \code{\link{predict.cv.corila}()}.
#'
#'@inherit cv.corila examples
#'@export
coef.cv.corila <- function(object, s = "lambda.min", ...) {
  if (s == "lambda.min") {
    s <- object$lambda.min
  } else if (!is.numeric(s) || length(s) != 1) {
    stop("Set s=\"lambda.min\" or provide numeric value.")
  }
  coef_stand <- stats::coef(object = object$object[[object$id_hyper]], s = s)
  if (object$scale$family == "cox") {
    alpha <- NULL
    beta <- coef_stand
  } else {
    alpha <- coef_stand[1]
    beta <- coef_stand[-1]
  }
  if (any(beta < 0)) {
    stop("negative values")
  }
  beta_positive <- beta[1:(length(beta) / 2)]
  beta_negative <- beta[(length(beta) / 2 + 1):(length(beta))]
  beta_combined <- beta_positive  - beta_negative
  coef <- c(alpha, beta_combined)
  coef <- backscale(coef = coef, pars = object$scale)$coef
  if (any(coef[c(FALSE[object$scale$family != "cox"],
                 !object$include == 1)] != 0)) {
    stop("Excluded coefs must equal zero.")
  }
  coef <- coef[c(TRUE[object$scale$family != "cox"], object$include == 1)]
  coef
}

#----- simulation -----

#'@title
#'Data simulation
#'
#'@description
#'Simulates data with grouped predictor variables
#'
#'@param family
#'character \code{"gaussian"}, \code{"binomial"},
#'\code{"poisson"} or \code{"cox"}
#'@param n0
#'number of training observations
#'@param n1
#'number of testing observations
#'@param n_group
#'number of variable groups
#'@param n_type
#'number of variable types
#'@param size_group
#'size of variable groups (per variable type)
#'@param effect_size
#'effect sizes (per variable type)
#'@param corfac_feature
#'decrease of correlation if different variable
#'@param corfac_type
#'decrease of correlation if different type
#'@param corfac_group
#'decrease of correlation if different group
#'@param n_group_causal
#'number of causal groups
#'@param prop_causal
#'proportion of causal features within causal groups
#'@param noise_factor
#'noise factor
#'@param plot
#'Attempt to visualise effects of and correlation between variables?
#'(\code{TRUE} or \code{FALSE})
#'@param trial logical (groups of negatively correlated subgroups)
#'
#'@return
#'Returns a list with the following slots:
#'\itemize{
#'\item \eqn{n_0 \times p} matrix \code{x_train}
#'\item \eqn{p}-dimensional vector \code{type}
#'\item \eqn{p}-dimensional vector \code{group}
#'\item \eqn{n_0}-dimensional vector \code{y_train}
#'\item \eqn{n_1 \times p} matrix \code{x_test}
#'\item \eqn{n_1}-dimensional vector \code{y_test}
#'\item \eqn{p}-dimensional vector \code{beta}
#'\item data frame \code{info} with entries
#'\eqn{n_0}, \eqn{n_1}, \eqn{p}, \code{n_type},
#'\code{n_group}, and \code{family}
#'}
#'
#'@examples
#'data <- simulate()
#'dims <- function(x) {
#'   if (is.matrix(x)||is.data.frame(x)) {
#'     paste(base::dim(x), collapse = " x ")
#'   } else {
#'     paste0(base::length(x))
#'   }
#'}
#'sapply(X = data, FUN = dims)
#'
#'@export
simulate <- function(family = "gaussian", n0 = 100, n1 = 10000, n_group = 20,
                     n_type = 2, size_group = c(5, 3), effect_size = c(1, 1),
                     corfac_feature = 0.5, corfac_type = 0.5,
                     corfac_group = 0.25, n_group_causal = 2,
                     prop_causal = 0.5, noise_factor = 1,
                     plot = TRUE, trial = FALSE) {
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
  if (family == "gaussian") {
    y <- eta + noise_factor * stats::rnorm(n = n, sd = stats::sd(eta))
    # NB: decrease/increase noise?
    if (stats::sd(y) == 0) {
      warning("Replacing constant y by random noise.")
      y <- stats::rnorm(n = n)
    }
  } else if (family == "binomial") {
    y <- stats::rbinom(n = n, size = 1, prob = 1 / (1 + exp(-2 * eta)))
    # NB: was without 2*
  } else if (family == "cox") {
    time <- stats::rexp(n = n, rate = exp(eta))
    status <- stats::rbinom(n = n, prob = 0.5, size = 1)
    #y <- cbind(time = time, status = status)
    y <- survival::Surv(time = time, event = status)
  } else if (family == "poisson") {
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

simulate_overlap <- function() {
  n0 <- 100
  n1 <- 10000
  n <- n0 + n1
  p <- 100
  n_group <- 20
  size_group <- rep(x = 5, times = n_group)
  # sample(x = 2:10, size = n_group, replace = TRUE)
  group <- lapply(X = seq_len(n_group),
                  FUN = function(i) {
                    sort(sample(x = seq_len(p), size = size_group[i]))
                  })
  # Inside corila, put each feature that is in no group in a separate group?
  mean <- rep(x = 0, times = p)
  sigma <- matrix(data = NA, nrow = p, ncol = p)
  #sigma <- 0.95^(abs(col(sigma)-row(sigma)))
  # alternative:
  for (i in seq_len(p)) {
    for (j in seq_len(p)) {
      group_i <- vapply(group, function(x) i %in% x, logical(1))
      group_j <- vapply(group, function(x) j %in% x, logical(1))
      sigma[i, j] <- 0.5^(i != j) * 0.25^(!any(group_i & group_j) & (i != j))
    }
  }
  sigma <- as.matrix(Matrix::nearPD(x = sigma)$mat)
  x <- mvtnorm::rmvnorm(n = n, mean = mean, sigma = sigma)
  sel_group <- sample(x = seq_len(n_group), size = 3)
  beta <- 1 * (seq_len(p) %in% unlist(group[sel_group]))
  # NB: multiply by abs(stats::rnorm(p))?
  eta <- x %*% beta
  y <- eta + 0.5 * stats::rnorm(n = n, sd = stats::sd(eta))
  fold <- rep(x = c(0, 1), times = c(n0, n1))
  x_train <- x[fold == 0, ]
  y_train <- y[fold == 0]
  x_test <- x[fold == 1, ]
  y_test <- y[fold == 1]
  info <- data.frame(n0 = n0, n1 = n1, p = p, n_group = n_group)
  list(x_train = x_train,
       group = group,
       y_train = y_train,
       x_test = x_test,
       y_test = y_test,
       beta = beta,
       info = info)
}

#----- comparison -----

#'@title
#'Calculates precision for sign variable
#'
#'@description
#'Calculates precision for ternary variables with support \eqn{\{-1, 0, 1\}},
#'i.e., the proportion of positive or negative estimated signs
#'that match the true sign.
#'
#'@param truth
#'integer vector with values in \eqn{\{-1, 0, 1\}}
#'@param estim
#'integer vector of same length with values in \eqn{\{-1, 0, 1\}}
#'
#'@return
#'Returns a scalar between 0 (minimum precision) and 1 (maximum precision),
#'or \code{NA} if all estimated signs equal 0.
#'
#'@examples
#'truth <- sample(x = c(-1, 0, 1), size = 10, replace = TRUE)
#'estim <- sample(x = c(-1, 0, 1), size = 10, replace = TRUE)
#'calc_sign_prec(truth = truth, estim = estim) # observed value
#'calc_sign_prec(truth = truth, estim = -truth) # lower limit 0
#'calc_sign_prec(truth = truth, estim = truth) # upper limit 1
#'calc_sign_prec(truth = truth, estim = 0 * estim) # not defined
#'
#'@export
calc_sign_prec <- function(truth, estim) {
  if (length(estim) != length(truth)) {
    stop("Arguments \"truth\" and \"estim\" must have the same length.")
  } else if (all(is.na(estim)) || all(estim == 0)) {
    NA
  } else {
    sum(estim != 0 & truth != 0 & sign(estim) == sign(truth)) / sum(estim != 0)
  }
}

#'@title
#'Comparison with hold-out
#'
#'@description
#'Compares methods using hold-out method
#'
#'@inheritParams cv.corila
#'
#'@param x_train
#'\eqn{n_0 \times p} matrix
#'@param y_train
#'\eqn{n_0}-dimensional vector
#'@param x_test
#'\eqn{n_1 \times p} matrix
#'@param y_test
#'\eqn{n_1}-dimensional vector
#'@param method
#'character vector listing all methods to be compared
#'@param nfolds
#'number of internal cross-validation folds (integer scalar)
#'@param foldid
#'internal cross-validation fold identifiers
#'(\eqn{p}-dimensional integer vector)
#'@param seed
#'random seed (integer scalar) for reproducibility, or \code{NULL}
#'
#'@return
#'Returns a list with the following slots:
#'\itemize{
#'\item \eqn{n_1}-dimensional vector \code{y_hat}
#'containing predicted values
#'\item \eqn{p}-dimensional vector \code{coef}
#'containing estimated coefficients
#'\item numerical vector \code{difftime}
#'indicating the computation time of each \code{method}
#'}
#'
#'@examples
#'\donttest{
#'data <- simulate()
#'results <- holdout(x_train = data$x_train,
#'                   y_train = data$y_train,
#'                   group = data$group,
#'                   include = rep(c(TRUE, FALSE), each = 80),
#'                   x_test = data$x_test,
#'                   y_test = data$y_test,
#'                   family = data$info$family,
#'                   method = c("mean", "ridge", "lasso", "corila"))
#'# Why does holdout require y_test? Try to remove this
#'}
#'@export
holdout <- function(x_train, y_train, group, include, family,
                    alpha_init = 0, alpha_final = 1,
                    x_test = NULL, y_test = NULL,
                    nfolds = 10, foldid = NULL, method = NULL,
                    seed = NULL, tune = "both") {
  # nfolds <- 10; foldid <- NULL; seed <- NULL

  if (!is.null(include) && any(include == 0) && !is.numeric(group)) {
    stop(paste0("Function holdout is not fully implemented",
                "for privileged learning with overlapping groups."))
  }

  p <- ncol(x_train)
  #n0 <- nrow(x_train)
  n1 <- nrow(x_test)

  if (is.null(include)) {
    include <- rep(x = TRUE, times = p)
  }

  if (is.null(x_test) != is.null(y_test)) {
    stop("Provide either both or none of x_test and y_test.")
  }

  if (is.null(foldid)) {
    #foldid <- sample(rep(x = seq_len(nfolds), length.out = n0))
    # balanced/stratified folds:
    foldid <- folds(y = y_train, family = family, nfolds = nfolds)
  }

  if (is.null(method)) {
    if (is.numeric(group)) {
      method <- c("mean", "ridge", "multiridge", "lasso", "gglasso", "grpreg",
                  "sparsegl", "SGL", "graper", "grpregOverlap", "scoop",
                  "ecpc", "squeezy", "MLGL", "pcLasso", "corila")
      # multiview is not for groups (only modalities)
    } else if (is.list(group)) {
      method <- c("mean", "ridge", "lasso", "grpregOverlap",
                  "ecpc", "squeezy", "corila")
      # overlapping groups (multiridge could also be adapted)
    }
    warning("omitting slow methods ...")
    method <- method[!method %in% c("SGL", "ecpc", "squeezy", "scoop")]
    #method <- method[method != "pcLasso"] # bug in application (singletons?)
  }

  if (!is.null(x_test)) {
    y_hat <- lapply(X = method, FUN = function(x) rep(x = NA, times = n1))
    names(y_hat) <- method
  } else {
    y_hat <- NULL
  }
  coef <- lapply(X = method, FUN = function(x) rep(x = NA, times = p + 1))
  names(coef) <- method
  #y_hat <- coef <- list()

  difftime <- numeric()

  for (i in method) {
    if (!is.null(seed)) {
      set.seed(seed)
    }
    start <- Sys.time()
    if (i == "mean") {
      #--- prediction by the mean ---
      if (!is.null(x_test)) {
        y_hat$mean <- rep(x = mean(y_train), times = n1)
      }
      if (family == "cox") {
        warning("Implement intercept-only model for Cox regression.")
      }
      coef$mean <- c(ifelse(test = family == "binomial",
                            yes = log(mean(y_train) / (1 - mean(y_train))),
                            no = ifelse(test = family == "poisson",
                                        yes = log(mean(y_train)),
                                        no = mean(y_train))),
                     rep(x = 0, times = sum(include)))
    } else if (i == "ridge") {
      #--- ridge ---
      object <- glmnet::cv.glmnet(x = x_train[, include],
                                  y = y_train,
                                  family = family,
                                  alpha = 0,
                                  foldid = foldid)
      if (!is.null(x_test)) {
        y_hat$ridge <- stats::predict(object = object,
                                      newx = x_test[, include],
                                      s = "lambda.min",
                                      type = "response")
      }
      coef$ridge <- c(NA[family == "cox"],
                      as.numeric(stats::coef(object = object,
                                             s = "lambda.min")))
    } else if (i == "multiridge") {
      if (family == "poisson") {
        next
      }
      #--- multiridge ---
      object <- multiridge(x = x_train[, include],
                           y = y_train,
                           z = group[include],
                           family = family)
      if (!is.null(x_test)) {
        y_hat$multiridge <- stats::predict(object = object,
                                           newx = x_test[, include])
      }
      coef$multiridge <- stats::coef(object = object)
    } else if (i == "lasso") {
      #--- lasso ---
      object <- glmnet::cv.glmnet(x = x_train[, include],
                                  y = y_train,
                                  family = family,
                                  alpha = 1,
                                  foldid = foldid)
      if (!is.null(x_test)) {
        y_hat$lasso <- stats::predict(object = object,
                                      newx = x_test[, include],
                                      s = "lambda.min",
                                      type = "response")
      }
      coef$lasso <- c(NA[family == "cox"],
                      as.numeric(stats::coef(object = object,
                                             s = "lambda.min")))
    } else if (i == "gglasso") {
      if (family %in% c("poisson", "cox")) {
        next
      }
      #--- group lasso (gglasso) ---
      if (family == "binomial") {
        temp_y_train <- 2 * y_train - 1
        temp_loss <- "logit"
      } else {
        temp_y_train <- y_train
        temp_loss <- "ls"
      }
      object <- gglasso::cv.gglasso(x = x_train[, include],
                                    y = temp_y_train,
                                    loss = temp_loss,
                                    group = group[include],
                                    foldid = foldid)
      if (!is.null(x_test)) {
        temp <- stats::predict(object = object,
                               newx = x_test[, include],
                               s = "lambda.min",
                               type = "link")
        if (family == "binomial") {
          y_hat$gglasso <- 1 / (1 + exp(-temp))
          #} else if (family == "poisson") {
          #  y_hat$gglasso <- exp(temp)
        } else {
          y_hat$gglasso <- temp
        }
      }
      coef$gglasso <- stats::coef(object, s = "lambda.min")
    } else if (i == "grpreg") {
      #if (family == "cox") {next}
      #--- group lasso (grpreg) ---
      if (family == "cox") {
        object <- grpreg::cv.grpsurv(X = x_train[, include],
                                     y = y_train,
                                     group = group[include],
                                     fold = foldid)
      } else {
        object <- grpreg::cv.grpreg(X = x_train[, include],
                                    y = y_train,
                                    family = family,
                                    group = group[include],
                                    fold = foldid)
      }
      if (!is.null(x_test)) {
        y_hat$grpreg <- stats::predict(object = object,
                                       X = x_test[, include],
                                       type = "response",
                                       lambda = object$lambda.min)
      }
      coef$grpreg <- c(NA[family == "cox"],
                       as.numeric(stats::coef(object = object,
                                              lambda = object$lambda.min)))
    } else if (i == "grplasso") {
      #--- group lasso (grplasso) ---
      ## This package requires the user to implement hyperparameter tuning.
      # if (family == "cox") {next}
      # if (family == "gaussian") {
      #   model <- grplasso::LinReg()
      # } else if (family == "binomial") {
      #   model <- grplasso::LogReg()
      # } else if (family == "poisson") {
      #   model <- grplasso::PoissReg()
      # }
      # lambda <- grplasso::lambdamax(x = cbind(1, x_train[, include]),
      # y = y_train, index = c(NA, group[include]),
      # penscale = base::sqrt, model = model) * 0.9^(0:100)
      # object <- grplasso::grplasso(x = cbind(1, x_train[, include]),
      #                              y = y_train,
      #                              index = c(NA, group[include]),
      #                              model = model,
      #                              lambda = lambda,
      # control = grplasso::grpl.control(update.hess = "lambda", trace = 0))
      # if (!is.null(x_test)) {
      #   y_hat$grplasso <- stats::predict(object = object,
      # newdata = cbind(1, x_test[, include]), type = "response")
      # }
      # coef$grplasso <- object$coefficients[, 1]
    } else if (i == "sparsegl") {
      if (family %in% c("poisson", "cox")) {
        next
      }
      #--- sparse group lasso (sparsegl) ---
      object <- sparsegl::cv.sparsegl(x = x_train[, include],
                                      y = y_train,
                                      group = group[include],
                                      family = family,
                                      foldid = foldid)
      if (!is.null(x_test)) {
        y_hat$sparsegl <- stats::predict(object = object,
                                         newx = x_test[, include],
                                         type = "response",
                                         s = "lambda.min")
      }
      coef$sparsegl <- stats::coef(object, s = "lambda.min")
    } else if (i == "SGL") {
      if (family == "poisson") {
        next
      }
      #--- sparse group lasso (SGL) ---
      family_temp <- ifelse(test = family == "gaussian",
                            yes = "linear",
                            no = ifelse(test = family == "binomial",
                                        yes = "logit",
                                        no = family))
      if (family == "cox") {
        data_temp <- list(x = x_train[, include],
                          time = as.matrix(y_train)[, "time"],
                          status = as.matrix(y_train)[, "status"])
      } else {
        data_temp <- list(x = x_train[, include], y = y_train)
      }
      cv_object <- SGL::cvSGL(data = data_temp,
                              index = group[include],
                              type = family_temp,
                              foldid = foldid)
      object <- SGL::SGL(data = data_temp,
                         index = group[include],
                         type = family_temp,
                         lambdas = cv_object$lambdas)
      if (!is.null(x_test)) {
        y_hat$SGL <- SGL::predictSGL(x = object,
                                     newX = x_test[, include],
                                     lam = which.min(cv_object$lldiff))
      }
      if (family == "gaussian") {
        coef$SGL <- c(object$intercept,
                      object$beta[, which.min(cv_object$lldiff)])
      } else {
        coef$SGL <- c(object$intercept[which.min(cv_object$lldiff)],
                      object$beta[, which.min(cv_object$lldiff)])
      }
    } else if (i == "graper") {
      if (!family %in% c("gaussian", "binomial")) {
        next
      }
      invisible(utils::capture.output(
        object <- suppressMessages(graper::graper(
          X = x_train[, include],
          y = y_train,
          annot = as.factor(group[include]),
          family = family
        ))
      ))
      if (!is.null(x_test)) {
        y_hat$graper <- stats::predict(object = object,
                                       newX = x_test[, include],
                                       type = "response")
      }
      coef$graper <- stats::coef(object = object)
    } else if (i == "grpregOverlap") {
      #--- grpregOverlap (only on GitHub) ---
      func <- grpregOverlap::expandX
      body(func)[[3]] <- quote(
        over_mat <- Matrix(incidence.mat %*% t(incidence.mat), sparse = TRUE)
      )
      utils::assignInNamespace(x = "expandX",
                               value = func,
                               ns = "grpregOverlap")
      if (is.numeric(group)) {
        list <- c(lapply(X = unique(group[include]),
                         FUN = function(z) which(group[include] == z)))
        #lapply(X = unique(type[include]),
        # FUN = function(z) which(type[include]  == z))
      } else {
        list <- group
      }
      if (family == "cox") {
        object <- grpregOverlap::cv.grpsurvOverlap(X = x_train[, include],
                                                   y = y_train,
                                                   group = list)
      } else {
        object <- grpregOverlap::cv.grpregOverlap(X = x_train[, include],
                                                  y = y_train,
                                                  group = list,
                                                  family = family)
      }
      if (!is.null(x_test)) {
        y_hat$grpregOverlap <- stats::predict(object = object,
                                              X = x_test[, include],
                                              type = "response",
                                              lambda = object$lambda.min)
      }
      coef$grpregOverlap <- c(if (family == "cox") NA,
                              stats::coef(object = object,
                                          lambda = object$lambda.min))
    } else if (i == "multiview") {
      #--- multiview (agreement between different modalities) ---
      object <- list()
      if (family == "gaussian") {
        temp <- stats::gaussian()
      } else if (family == "binomial") {
        temp <- stats::binomial()
      } else if (family == "poisson") {
        temp <- stats::poisson()
      }
      #rho <- c(0.00, 0.10, 0.25, 0.50, 1.00)
      #for (j in seq_along(rho)) {
      #  object[[j]] <- multiview::cv.multiview(
      # x_list = lapply(X = unique(type[include]),
      # FUN = function(z) x_train[, type[include] == z]),
      # y = y_train, family = temp, rho = rho[j], foldid = foldid)
      #}
      #id <- which.min(sapply(object, function(x) min(x$cvm)))
      #if (!is.null(x_test)) {
      #  y_hat$multiview <- stats::predict(object = object[[id]],
      # newx = lapply(X = unique(type[include]),
      # FUN = function(z) x_test[, type[include] == z]),
      # type = "response", s = "lambda.min")
      #}
      #coef$multiview <- stats::coef(object = object[[id]], s = "lambda.min")
    } else if (i == "scoop") {
      if (!family %in% c("gaussian", "binomial")) {
        next
      }
      if (all(table(group[include]) == 1)) {
        #group_temp <- rep(x = 1, times = length(group))
        object <- scoop::coop.lasso(x = x_train[, include],
                                    y = y_train,
                                    group = group,
                                    family = family)
      } else {
        object <- scoop::sparse.coop.lasso(x = x_train[, include],
                                           y = y_train,
                                           group = group[include],
                                           family = family)
      }
      object_cv <- scoop::crossval(object)
      id <- which(object_cv@lambda == object_cv@lambda.min)
      if (!is.null(x_test)) {
        y_hat$scoop <- scoop::predict(object = object,
                                      newx = x_test[, include])[, id]
      }
      coef$scoop <- object_cv@beta.min
    } else if (i == "MLGL") {
      #--- multi-layer group-lasso ---
      if (!family %in% c("gaussian", "binomial")) {
        next
      }
      loss <- ifelse(family == "gaussian", "ls", "logit")
      if (loss == "logit") {
        y_train_temp <- 2 * y_train - 1
      } else {
        y_train_temp <- y_train
      }
      cv <- MLGL::cv.MLGL(X = x_train[, include],
                          y = y_train_temp,
                          loss = loss)
      object <- MLGL::MLGL(X = x_train[, include],
                           y = y_train_temp,
                           loss = loss)
      if (!is.null(x_test)) {
        temp <- stats::predict(object = object,
                               newx = x_test[, include],
                               type = "fit",
                               s = cv$lambda.min)
        if (loss == "ls") {
          y_hat$MLGL <- temp
        } else {
          y_hat$MLGL <- 1 / (1 + exp(-temp))
        }
      }
      coef$MLGL <- stats::coef(object = object, s = cv$lambda.min)
    } else if (i == "ecpc") {
      if (family == "poisson") {
        next
      }
      if (family == "cox") {
        y_temp <- y_train
      } else {
        y_temp <- matrix(y_train, ncol = 1)
      }
      #--- ecpc ---
      model <- ifelse(test = family == "gaussian",
                      yes = "linear",
                      no = ifelse(test = family == "binomial",
                                  yes = "logistic",
                                  no = family))
      if (is.numeric(group[include])) {
        invisible(utils::capture.output(
          groupset <- ecpc::createGroupset(values = as.factor(group[include]))
        ))
      } else if (is.list(group)) {
        base <- lapply(group, function(x) as.integer(x))
        # first alternative
        #extra <- seq_len(p)[!seq_len(p) %in% unlist(base)]
        # second alternative
        extra <- list(seq_len(p)[!seq_len(p) %in% unlist(base)])
        groupset <- c(base, extra)
      }
      #invisible(utils::capture.output(
      #  typeset <- ecpc::createGroupset(values = as.factor(type))
      #))
      #datablocks <- lapply(X = unique(type[include]),
      #                     FUN = function(x) which(type[include] == x))
      invisible(
        tryCatch(
          utils::capture.output(
            object <- ecpc::ecpc(
              Y = y_temp,
              X = x_train[, include],
              groupsets = list(groupset),
              X2 = x_test[, include],
              model = model,
              fold = nfolds,
              datablocks = NULL
            )
          ),
          error = function(x) NULL
        )
      )
      # Currently typeset/datablocks is ignored!
      if (!is.null(object)) {
        coef$ecpc <- unlist(stats::coef(object))
      }
      if (!is.null(object) && !is.null(x_test)) {
        y_hat$ecpc <- object$Ypred
      }
    } else if (i == "gren") {
      #partitions <- list(group = lapply(X = unique(group),
      #                                  FUN = function(x) which(group == x)),
      #                   type = lapply(X = unique(type),
      #                                 FUN = function(x) which(type == x)))
      #object <- gren::cv.gren(x = x_train[, include],
      #                        y = y_train,
      #                        partitions = list(group = group, type = type),
      #                        trace = TRUE)
      warning("Implement GREN.")
    } else if (i == "squeezy") {
      if (family %in% c("poisson", "cox")) {
        next
      }
      if (is.numeric(group)) {
        groupset <- lapply(X = unique(group[include]),
                           FUN = function(x) which(group[include] == x))
      } else if (is.list(group)) {
        base <- lapply(group, function(x) as.integer(x))
        # 1st alternative
        #extra <- seq_len(p)[!seq_len(p) %in% unlist(base)]
        # 2nd alternative
        extra <- list(seq_len(p)[!seq_len(p) %in% unlist(base)])
        groupset <- c(base, extra)
      }
      object <- squeezy::squeezy(Y = y_train,
                                 X = x_train[, include],
                                 groupset = groupset,
                                 X2 = x_test[, include])
      # Check whether type can be included (e.g., as groups).
      y_hat$squeezy <- object$YpredApprox
      coef$squeezy <- c(object$a0Approx, object$betaApprox)
    } else if (i == "CBPE") {
      #if (FALSE) {
      #  n <- 100
      #  p <- 50
      #  x_train <- matrix(rnorm(n * p), n, p)
      #  beta_true <- c(0.5, -1, 2, 5, rep(0, times = p - 4))
      #  y_train <- rbinom(n, 1, 1 / (1 + exp(-x_train %*% beta_true)))
      #}
      #if (family == "gaussian") {
      #  cbpe <- CBPE::CBPLinearE
      #} else if (family == "binomial") {
      #  cbpe <- CBPE::CBPLogisticE
      #} else {
      #  next
      #}
      #lambda <- exp(seq(from = log(1e06), to = log(1e-06), length.out = 20))
      # no predict function implemented
      #for (i in seq_len(nfolds)) {
      #  coef <- CBPE(X = x_train[foldid !=  i, include],
      #               y = y_train[foldid != i],
      #               lambda = 0)
      #  x_train[foldid == i, include] %*% coef
      #}
      # internal cross-validation to tune lambda
      # refit on full training data with optimal lambda
      stop("Not yet implemented.")
    } else if (i == "pcLasso") {
      if (!family %in% c("gaussian", "binomial")) {
        next
      }
      group_temp <- lapply(X = unique(group[include]),
                           FUN = function(x) which(x == group[include]))
      # duplicating singletons:
      .duplicate_singletons <- function(x) {
        if (length(x) == 1) {
          rep(x, times = 2)
        } else {
          x
        }
      }
      group_temp <- lapply(X = group_temp, FUN = .duplicate_singletons)
      # combining remaining features
      indices <- seq_len(ncol(x_train[, include]))
      extra <- indices[!indices %in% unlist(group_temp)]
      if (length(extra) > 0) {
        group_temp <- c(group_temp, extra)
      }
      #group_temp <- c(group_temp[!cond], list(unlist(group_temp[cond])))
      if (!family %in% c("gaussian", "binomial")) {
        next
      }
      ratio <- c(seq(from = 0.25, to = 0.75, by = 0.25), 0.9, 0.95, 1)
      # NB: set is from paper
      object <- list()
      for (j in seq_along(ratio)) {
        invisible(utils::capture.output(
          object[[j]] <- pcLasso::cv.pcLasso(x = x_train[, include],
                                             y = y_train,
                                             family = family,
                                             groups = group_temp,
                                             ratio = ratio[j])
        ))
      }
      id <- which.min(vapply(X = object,
                             FUN = function(x) min(x$cvm),
                             FUN.VALUE = numeric(1)))
      object <- object[[id]]
      if (!is.null(x_test)) {
        y_hat$pcLasso <- pcLasso::predict.cv.pcLasso(object = object,
                                                     xnew = x_test[, include],
                                                     s = "lambda.min")
      }
      coef$pcLasso <- c(
        object$glmfit$a0[which(object$lambda == object$lambda.min)],
        object$glmfit$beta[, which(object$lambda == object$lambda.min)]
      )
      # Under overlapping groups, use object$glmfit$origbeta.
    } else if (i == "corila") {
      #--- lasso with feature groups and modalities ---
      object <- cv.corila(x = x_train,
                          y = y_train,
                          group = group,
                          include = include,
                          alpha_init = alpha_init,
                          alpha_final = alpha_final,
                          family = family,
                          foldid = foldid,
                          tune = tune)
      print(object$hyper[object$id_hyper, ])
      if (!is.null(x_test)) {
        y_hat$corila <- stats::predict(object = object,
                                       newx = x_test[, include])
      }
      coef$corila <- stats::coef(object = object)
    }
    end <- Sys.time()
    difftime[i] <- difftime(time1 = end, time2 = start, units = "secs")
  }
  #- - - checks - - -
  if (family != "cox") {
    method <- names(y_hat)
    if (!is.null(x_test)) {
      if (family == "binomial") {
        if (min(unlist(y_hat), na.rm = TRUE) < 0) {
          stop("too small")
        }
        if (max(unlist(y_hat), na.rm = TRUE) > 1) {
          stop("too large")
        }
      }
      for (i in seq_along(method)) {
        if (method[i] == "pcLasso") {
          next
        }
        original <- y_hat[[i]]
        if (all(is.na(original))) {
          next
        }
        eta <- coef[[i]][1] + x_test[, include] %*% coef[[i]][-1]
        if (family %in% c("gaussian", "cox")) {
          manual <- eta
        } else if (family == "binomial") {
          manual <- 1 / (1 + exp(-eta))
        } else if (family == "poisson") {
          manual <- exp(eta)
        }
        #cond <- is.na(original)|is.na(manual)
        #if (any(cond)) {
        #  message("coef:", paste(head(coef[[i]]), collapse = " "))
        #  message("original:", paste(head(original), collapse = " "))
        #  message("manual:", paste(head(manual), collapse = " "))
        #}
        #message("original: ", paste0(original[cond], collapse = " "))
        #message("manual: ", paste0(manual[cond], collapse = " "))
        if (any(abs(original - manual) > 0.001)) {
          warning(paste("unequal:", method[i]))
        }
        if (stats::sd(original) != 0 &&
              stats::sd(manual) != 0 &&
              stats::cor(original, manual) < 0.999) {
          warning(paste("correlation:", method[i]))
        }
      }
    }

    if (!is.null(x_test)) {
      range <- range(unlist(y_hat), na.rm = TRUE)
      if (family == "binomial" && (range[1] < 0 || range[2] > 1)) {
        stop("invalid y_hat range")
      }
      if (any(vapply(X = y_hat,
                     FUN = base::length,
                     FUN.VALUE = numeric(1)) != n1)) {
        stop("invalid y_hat length")
      }
    }
    if (any(vapply(X = coef,
                   FUN = base::length,
                   FUN.VALUE = numeric(1)) != sum(include) + 1)) {
      stop("invalid coef length")
    }
  } else {
    warning("Implement checks for Cox regression.")
  }
  list(y_hat = y_hat, coef = coef, difftime = difftime)
}

#'@title
#'Cross-validation method
#'
#'@description
#'Compares methods with cross-validation method
#'
#'@inheritParams cv.corila
#'@inheritParams holdout
#'
#'@param iter number of cross-validation iterations
#'@param nfolds number of cross-validation folds
#'@param foldid cross-validation folds
#'
#'@details
#'This function implements repeated \eqn{k}-fold cross-validation
#'(e.g., 5 repetitions of 10-fold cross-validation).
#'
#'@return
#'Returns a list with the following slots:
#'\itemize{
#'\item \code{nzero} non-zero coefficients
#'\item \code{metric} metric
#'}
#'Both slot contain a data frame
#'with one row for each iteration (\code{iter})
#'and one column for each \code{method}.
#'
#'@examples
#'\donttest{
#'n <- 100
#'p <- 20
#'x <- matrix(rnorm(n * p), nrow = n, ncol = p)
#'y <- stats::rnorm(n)
#'foldid <- rep(c(0, 1), times = c(50, 50))
#'results <- crossval(x, y, family = "gaussian",
#'                    method = c("mean", "corila"), foldid = foldid)
#'}
#'@export
crossval <- function(x, y, family, group = NULL, include = NULL,
                     alpha_init = 0, alpha_final = 1, iter = 5, foldid = NULL,
                     nfolds = 10, method = NULL, tune = "both") {
  n <- nrow(x)
  p <- ncol(x)
  if (is.null(group)) {
    group <- seq_len(p)
  }
  list <- list()
  list$metric <- list$nzero <- list()
  for (k in seq_len(iter)) {
    set.seed(k)
    cat("iter", k, "\n")
    if (is.null(foldid)) {
      #foldid <- sample(rep(x = seq_len(nfolds), length.out = n))
      foldid <- folds(y = y,
                      family = family,
                      nfolds = nfolds) # balanced/stratified folds
    } else {
      nfolds <- max(foldid)
    }
    y_hat <- data.frame(row.names = seq_len(n))
    for (i in seq_len(nfolds)) {
      set.seed(i)
      cat("fold", i, "\n")
      cond <- foldid == i
      results <- holdout(x_train = x[!cond, ],
                         y_train = y[!cond],
                         x_test = x[cond, ],
                         y_test = y[cond],
                         group = group,
                         include = include,
                         alpha_init = alpha_init,
                         alpha_final = alpha_final,
                         family = family,
                         nfolds = 10,
                         foldid = NULL,
                         method = method,
                         seed = NULL,
                         tune = tune)
      for (j in seq_along(results$y_hat)) {
        y_hat[[names(results$y_hat)[j]]][cond] <- results$y_hat[[j]]
      }
    }
    if (family %in% c("gaussian", "poisson")) {
      list$metric[[k]] <- apply(
        X = y_hat,
        MARGIN = 2,
        FUN = function(x) mean((y[foldid != 0] - x[foldid != 0])^2)
      )
    } else if (family == "binomial") {
      list$metric[[k]] <- apply(
        X = y_hat,
        MARGIN = 2,
        FUN = function(x) {
          pROC::auc(response = y[foldid != 0],
                    predictor = as.vector(x[foldid != 0]),
                    levels = c(0, 1), direction = "<")
        }
      )
    } else if (family == "cox") {
      list$metric[[k]] <- apply(
        X = y_hat,
        MARGIN = 2,
        FUN = function(x) {
          survival::concordance(y[foldid != 0] ~ I(-x[foldid != 0]))$concordance
        }
      )
    }
    set.seed(k)
    if (nfolds == 1) {
      list$nzero[[k]] <- vapply(X = results$coef,
                                FUN = function(x) sum(x[-1] != 0),
                                FUN.VALUE = numeric(1))
    } else {
      refit <- holdout(x_train = x[foldid != 0, ],
                       y_train = y[foldid != 0],
                       group = group,
                       include = include,
                       alpha_init = alpha_init,
                       alpha_final = alpha_final,
                       family = family,
                       nfolds = 10,
                       foldid = NULL,
                       method = method,
                       seed = NULL,
                       tune = tune)
      list$nzero[[k]] <- vapply(X = refit$coef,
                                FUN = function(x) sum(x[-1] != 0),
                                FUN.VALUE = numeric(1))
    }
  }
  list <- lapply(X = list, FUN = function(x) do.call(what = "rbind", args = x))
  list$family <- family
  list
}

.wilcox_test <- function(x, y, ...) {
  if (all(is.na(x)) || all(is.na(y))) {
    NA
  } else {
    stats::wilcox.test(x = x, y = y, ...)$p.value
  }
}

#'@title
#'Custom box plot function
#'
#'@description
#'Creates box plots for paired/matched data,
#'using Wilcoxon's signed-rank test to compare a group with the other groups.
#'
#'@param x data frame with names slots
#'@param base character string naming the slot of interest
#'(e.g., \code{"corila"})
#'@param main character string used as a title
#'@param decrease \code{TRUE} for decreasing arrow,
#'\code{FALSE} for increasing arrow
#'@param ylim limits for the vertical axis, or \code{NULL}
#'@param cex.main numeric
#'
#'@return
#'Returns \code{NULL} (and plots a figure).
#'
#'@examples
#'x <- data.frame(mean = 0, corila = rnorm(100) - 1, other = rnorm(100))
#'plot_boxes(x)
#'
#'@export
plot_boxes <- function(x, base = "corila", main = "", decrease = TRUE,
                       ylim = NULL, cex.main = 1.2) {
  #--- hypothesis testing ---
  pvalue <- list()
  for (i in c("less", "greater")){
    label <- ifelse(decrease == (i == "less"), "better", "worse")
    pvalue[[label]] <- apply(
      X = x,
      MARGIN = 2,
      FUN = function(col) {
        .wilcox_test(x = col,
                     y = x[, base],
                     paired = TRUE,
                     alternative = i,
                     exact = FALSE)
      }
    )
  }

  col <- ifelse(test = pvalue$worse <= 0.05,
                yes = "red",
                no = ifelse(test = pvalue$better <= 0.05,
                            yes = "blue",
                            no = "grey"))
  #--- boxplot ---
  graphics::boxplot(x = x,
                    main = main,
                    las = 2,
                    col = col,
                    frame.plot = FALSE,
                    xaxt = "n",
                    yaxt = "n",
                    ylim = ylim,
                    cex.main = cex.main)
  #--- horizontal axis ---
  col <- list(grey = which(pvalue$worse <= 0.05 | is.na(pvalue$worse)),
              black = which(pvalue$worse > 0.05))
  for (i in seq_along(col)) {
    graphics::axis(side = 1,
                   at = seq_len(ncol(x))[col[[i]]],
                   labels = colnames(x)[col[[i]]],
                   las = 2,
                   col.axis = names(col)[i],
                   tick = FALSE,
                   line = -0.5)
  }
  if ("mean" %in% colnames(x)) {
    graphics::abline(h = stats::median(x[, "mean"]), lty = 2, col = "grey")
  }
  #--- vertical axis ---
  usr <- graphics::par("usr")
  mar_big <- 0.05 * (usr[4] - usr[3])
  mar_small <- 0.05 * (usr[4] - usr[3])
  graphics::axis(side = 2, col = "grey", col.axis = "grey")
  graphics::arrows(x0 = usr[1],
                   y0 = usr[3] + mar_big,
                   x1 = usr[1],
                   y1 = usr[4] - mar_big,
                   length = 0.1,
                   xpd = TRUE,
                   code = ifelse(decrease, 1, 2),
                   lwd = 2)
  graphics::text(
    x = usr[1],
    y = c(usr[4] + mar_small, usr[3] - mar_small)[1 + c(!decrease, decrease)],
    labels = c("-", "+"),
    col = c("red", "blue"),
    xpd = TRUE,
    cex = 1.5,
    font = 2
  )
  invisible(NULL)
}

#@title
#Combine variables
#
#@description
#Calculates the mean or the first principal component of a group of variables
#
#@inheritParams construct_matrices
#@param x \eqn{n_0 \times p_k} matrix, where \eqn{n_0}
# is the number of observations used for model training and
# \eqn{p_k} is the number of variables inside a group
#@param fuse character string \code{"mean"}
# for arithmetic mean  or \code{"pca"} for first principal component
#
#@return
#Returns an \eqn{n_0}-dimensional numeric vector.
#
#@seealso
#This function is called by \code{\link{corila}()}
# and thereby \code{\link{cv.corila}()}.
#
#@examples
#n <- 100; p <- 5
#x <- matrix(data = stats::rnorm(n = n * p), nrow = n, ncol = p)
#mean <- combine_features(x = x, fuse = "mean")
#comp <- combine_features(x = x, fuse = "pca")
#plot(mean, comp)
#
#@export
# combine_features <- function(x, fuse = "mean") {
#   if (!fuse %in% c("mean", "pca")) {
#     stop("Argument \"fuse\" must equal \"mean\" or \"pca\".")
#   }
#   if (fuse == "mean") {
#     rowMeans(x)
#   } else if (fuse == "pca") {
#     stats::princomp(x = x)$scores[, "Comp.1"]
#   }
# }

#@title
#Construct Matrices
#
#@description
#Constructs matrices with
# (i) the original data concatenated with the inverted data,
# (ii) one meta-variable for each group, and
# (iii) one meta-variable for each group in each type.
#
#@param group \eqn{p}-dimensional vector of group labels or indices,
# or list with one slot for each group containing the variable labels or indices
#@param type \eqn{p}-dimensional vector
#@inheritParams corila
#
#@examples
#n <- 5
#p <- 6
#x <- matrix(data = rnorm(n * p), nrow = n, ncol = p)
#group <- rep(1:2, each = p / 2)
#type <- rep(x = 1, times = p)
#x <- construct_matrices(x = x, group = group, type = type)
#
#@seealso
#This function is called by \code{\link{corila}()}
# and thereby \code{\link{cv.corila}()}.
#
#@return
#See description.
#@export
# construct_matrices <- function(x, group, type, fuse = "mean") {
#   if ((is.numeric(group) && ncol(x) != length(group)) |
# ncol(x) != length(type)) {
#     stop("For each variable, the matrix \"x\" must have one column,
# and the vectors \"group\" (if applicable) and \"type\" must have one entry.")
#   }
#   index <- seq_len(ncol(x))
#   n <- nrow(x)
#   if (is.numeric(group)) {
#     q <- length(unique(group))
#   } else {
#     q <- length(group)
#   }
#   m <- length(unique(type))
#   com <- matrix(data = NA, nrow = n, ncol = q,
# dimnames = list(NULL, seq_len(q)))
#   sep <- replicate(n = m, expr = com, simplify = FALSE)
#   for (i in seq_len(m)) {
#     for (j in seq_len(q)) {
#       if (is.numeric(group)) {
#         sep[[i]][, j] <-
# combine_features(x = x[, type== i & group == j, drop = FALSE], fuse = fuse)
#       } else {
#         sep[[i]][, j] <-
# combine_features(x = x[, type == i & index %in% group[[j]], drop = FALSE],
# fuse = fuse)
#       }
#     }
#   }
#   for (j in seq_len(q)) {
#     if (is.numeric(group)) {
#       com[, j] <- combine_features(x = x[, group == j, drop = FALSE],
# fuse = fuse)
#     } else {
#       com[, j] <-
# combine_features(x = x[, index %in% group[[j]], drop = FALSE],
# fuse = fuse)
#     }
#   }
#   list(all = cbind(x, -x), com = com, sep = sep)
# }
