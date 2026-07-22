# This file contains the functions of the R package "corila".

#----- model fitting with cross-validation -----

if(FALSE){
data <- simulate_data()
noise <- stats::rnorm(n = length(data$group), sd = 1e-09)
a <- .is_adjacent(group = data$group, j = 1, p = length(data$group),
               names = names(data$group))
b <- .is_adjacent(group = data$group + noise, j = 1, p = length(data$group),
               names = names(data$group))
all(a == b)

set.seed(1)
model <- cv.corila(x = data$x_train, y = data$y_train,
                     group = data$group)
a <- coef(model)
set.seed(1)
model <- cv.corila(x = data$x_train, y = data$y_train,
                     group = data$group + noise)
b <- coef(model)
all(a == b)
}

#' @title
#' Sparse group lasso regression
#'
#' @description
#' Optimises the parameters and the hyperparameters of the sparse group lasso.
#'
#' @param x
#' \eqn{n_0 \times p} predictor matrix,
#' containing only numerical values (continuous, integer, or binary),
#' where \eqn{n_0} is the number of observations used for model training
#' and \eqn{p} is the number of predictors
#'
#' @param y
#' response vector of length \eqn{n_0},
#' containing numerical values (`family="gaussian"`),
#' integer values (`family="poisson"`),
#' binary values (`family="binomial"`),
#' or a survival object created with `survival::Surv()` (`family="cox"`),
#' where \eqn{n_0} is the number of observations used for model training
#'
#' @param group
#' group structure (multiple options):
#' - \eqn{p}-dimensional vector of group indices
#' (in \eqn{\{1, \ldots, q\}}) or labels,
#' - list with \eqn{q} slots containing the variable indices
#' (in \eqn{\{1, \ldots, p\}}) or labels,
#' - \eqn{p \times p} matrix,
#' where the entry in the \eqn{j^{\text{th}}} row
#' and the \eqn{k^{\text{th}}} column
#' indicates whether information should be transferred
#' from the \eqn{j^{\text{th}}} to the \eqn{k^{\text{th}}} variable
#'
#' @param primary
#' \eqn{p}-dimensional logical vector
#' indicating whether a predictor may be included in the final model
#' (`TRUE` for "primary predictors")
#' or must be excluded from the final model
#' (`FALSE` for "auxiliary predictors")
#'
#' @param alpha_init
#' A scalar specifying the method used for obtaining initial coefficients:
#' - a numeric scalar in the unit interval
#'   (\eqn{0 \leq} `alpha_init` \eqn{\leq 1})
#'   to define the mixing parameter for elastic net regression
#'   (default: ridge penalisation with `alpha_init`=0);
#' - the character scalar `"pearson"`, `"spearman"`, or `"kendall"`
#'   to use initial correlation coefficients
#'   (not implemented for `family="cox"`)
#' - the character scalar`"multiridge"`
#'   to use multi-penalty ridge regression
#'    with one penalty for each group
#'   (not implemented for `family="poisson"` or overlapping groups),
#' - `NA` to set all initial coefficients equal to 1
#'
#' @param alpha_final
#' elastic net mixing parameter for final regression:
#' numeric between 0 for ridge penalisation
#' and 1 for lasso penalisation
#' (default: lasso penalisation with `alpha_final`=1)
#'
#' @param family
#' character string `"gaussian"`, `"binomial"`,
#' `"poisson"`, or `"cox"`
#'
#' @param foldid
#' \eqn{n_0}-dimensional vector containing the fold identifiers
#' (minimum \eqn{1}, maximum `nfolds`)
#'
#' @param nfolds
#' positive integer specifying the number of folds
#' (minimum \eqn{3}, maximum \eqn{n})
#'
#' @param cor
#' character string `"pearson"`,
#' `"spearman"` (default),
#' or `"kendall"`;
#' or a correlation matrix
#' (\eqn{p} rows, \eqn{p} columns,
#' entries between \eqn{-1} and \eqn{+1})
#'
#' @param tune
#' character string for determining the candidate values
#' for the hyperparameters:
#' - `"none"`:
#' fixed weights and exponents
#' (`wgt_local`=1, `exp_local`=1, `wgt_global`=0),
#' no tuning
#' - `"weight"`:
#' fixed exponents (`exp_local`=0, `exp_global`=1),
#' tuning `wgt_local`=1-`wgt_global`
#' - `"exponent"`:
#' fixed weights (`wgt_local`=1, `wgt_global`=0),
#' tuning `exp_local`
#' - `"bivariate"`:
#' tuning `wgt_local`=1-`wgt_global` and `exp_local`=`exp_global`
#' - `"factorial"`:
#' tuning `wgt_local`, `exp_local`, `wgt_global`, `exp_global`
#'
#' (to implement: list with slots
#' `wgt_local`, `exp_local`, `wgt_global`, and `exp_global`)
#'
#' @param na_action
#' character `"error"` to trigger an error
#' if any observation has a missing predictor or a missing response
#' or `"complete_cases"` to exclude observations
#' with a missing predictor or a missing response from model fitting
#' (while providing fitted values for these observations)
#'
#' @param silent
#' Should messages from [glmnet::glmnet()] and [glmnet::cv.glmnet()]
#' be suppressed? (logical scalar, `FALSE` or `TRUE`)
#'
#' @inherit corila details
#'
#' @return
#' Returns an object of class `"cv.corila"`,
#' a list with the following slots:
#' - `model`:
#' list with one slot for each combination of hyperparameters,
#' each slot contains an object of class `"glmnet"`
#' - `hyper`:
#' data frame with one row for each combination of hyperparameters,
#' four columns for the values of the hyperparameters
#' (`wgt_local`, `exp_local`, `wgt_global`, and `exp_global`)
#' and a column for the cross-validated loss (`cvm`)
#' - `id_hyper`:
#' index of combination of hyperparameters
#' leading to the lowest cross-validated loss
#' - `lambda.min`
#' optimised regularisation hyperparameter
#' - `scale`:
#' output from [.forescale()]
#' - `y_hat`:
#' \eqn{n}-dimensional vector of fitted values
#'
#' @inherit corila-package references
#'
#' @seealso
#' Extract coefficients with [coef()][coef.cv.corila]
#' and make predictions with [predict()][predict.cv.corila].
#'
#' This user function repeatedly calls [corila()]
#' with different values for the regularisation and mixing hyperparameters.
#'
#' @examples
#' data <- simulate_data()
#'
#' # option A: balanced and stratified folds (generated by corila:::.folds)
#' foldid <- NULL
#'
#' # option B: define folds manually
#' foldid <- sample(seq_len(10), size = length(data$y_train), replace = TRUE)
#'
#' model <- cv.corila(x = data$x_train,
#'                    y = data$y_train,
#'                    group = data$group,
#'                    primary = data$primary,
#'                    tune = "none",
#'                    foldid = foldid)
#'
#' warning("Re-activate examples.")
#' #\dontshow{
#' #model <- cv.corila(x = data$x_train,
#' #                    y = data$y_train,
#' #                    group = data$group,
#' #                    primary = data$primary,
#' #                    tune = "none")
#' # }
#' # \donttest{
#' # model <- cv.corila(x = data$x_train,
#' #                    y = data$y_train,
#' #                    group = data$group,
#' #                    primary = data$primary)
#' # }
#'
#' @keywords methods models regression classif
#'
#' @export
#'
#' @srrstats {G2.0a} *lengths of vector inputs are documented*
#' @srrstats {G2.1a} *data types of vector inputs are documented*
#' @srrstats {G2.3b} *uses tolower() for arguments family and cor*
#' @srrstats {G2.14} *uses argument na_action*
#' @srrstats {G2.14a} *to trigger an error on missing data*
#' @srrstats {G2.14b} *to ignore observations with missing data*
#' @srrstats {G2.16} *provides option to handle undefined values*
#' @srrstats {RE2.1} *documents parameter controlling missing values*
#' @srrstats {RE2.2} *can fit values for observations with missing response*
#' @srrstats {G2.6} *one-dimensional inputs are vectorised*
#' @srrstats {G3.0} *equality comparisons between integers, or approximate*
#' @srrstats {RE1.2} *documents expected format of predictors*
#' @srrstats {RE1.3} *retains names of observations and predictors*
#' @srrstats {RE1.4} *documents assumptions for input data*
#' @srrstats {RE3.1} *convergence messages can be suppressed (@param silent)*
#' @srrstats {RE4.0} *returns a "model" object (@return)*
#' @srrstats {RE4.8} *returns response variable in slot "y_obs"*
#'
cv.corila <- function(x, y, group, primary = NULL, alpha_init = 0.0,
                      alpha_final = 1.0, family = "gaussian",
                      nfolds = 10L, cor = "spearman", tune = "weight",
                      foldid = NULL, na_action = "error", silent = FALSE) {
  y <- drop(y)
  group <- drop(group)
  primary <- drop(primary)
  # match arguments
  family <- match.arg(arg = tolower(family),
                      choices = c("gaussian", "binomial", "poisson", "cox"))
  if (is.character(cor)) {
    cor <- match.arg(arg = tolower(cor),
                     choices = c("pearson", "spearman", "kendall"))
  }
  tune <- match.arg(arg = tolower(tune),
                    choices = c("none", "weight", "exponent", "bivariate",
                                "factorial"))
  na_action <- match.arg(arg = tolower(na_action),
                         choices = c("error", "complete_cases"))
  # set default parameters
  hyper <- .set_candidates(tune = tune)
  .validate(
    x = x,
    y = y,
    group = group,
    primary = primary,
    family = family,
    na_action = na_action,
    alpha_init = alpha_init,
    alpha_final = alpha_final,
    cor = cor,
    foldid = foldid,
    nfolds = nfolds,
    lambda_init = NULL,
    hyper = hyper,
    silent = silent
  )
  if (is.null(primary)) {
    primary <- rep(x = TRUE, times = ncol(x))
  }
  if (is.null(foldid)) {
    foldid <- .folds(y = y, family = family, nfolds = nfolds)
  }
  alpha_final <- pmax(0.0, pmin(alpha_final, 1.0))
  if (identical(na_action, "complete_cases")) {
    complete <- stats::complete.cases(x = x, y = y)
    if (sum(complete) < 3L) {
      stop("Requires at least three complete observations.")
    }
    warning("Ignoring ", sum(!complete), " observations with missing data.")
    #x <- x[complete, ]
    #y <- y[complete]
    #foldid <- foldid[complete]
  } else {
    complete <- rep(x = TRUE, times = nrow(x))
  }
  # fit model on all folds
  object_ext <- corila(x = x[complete, ],
                       y = y[complete],
                       group = group,
                       primary = primary,
                       family = family,
                       alpha_init = alpha_init,
                       alpha_final = alpha_final,
                       cor = cor,
                       foldid = foldid[complete],
                       nfolds = NULL,
                       hyper = hyper,
                       lambda_init = NULL,
                       silent = silent)
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
    object_int <- corila(x = x[foldid != i & complete, ],
                         y = y[foldid != i & complete],
                         group = group,
                         primary = primary,
                         family = family,
                         alpha_init = alpha_init,
                         alpha_final = alpha_final,
                         cor = cor,
                         foldid = NULL,
                         nfolds = NULL,
                         hyper = hyper,
                         lambda_init = object_ext$lambda_init,
                         silent = silent)
    for (j in seq_len(nrow(hyper))) {
      pred[[j]][foldid == i & complete, ] <- predict.corila(
        object = object_int,
        newx = x[foldid == i & complete, , drop = FALSE],
        index = j,
        s = lambda[[j]]
      )
    }
  }
  # select the hyperparameters
  cvm <- list()
  for (l in seq_len(nrow(hyper))) {
    cvm[[l]] <- apply(
      X = pred[[l]][complete, ],
      MARGIN = 2L,
      FUN = function(x) .deviance(y_hat = x, y =  y[complete], family = family)
    )
  }
  hyper$cvm <- cvm_min <- vapply(X = cvm,
                                 FUN = base::min,
                                 FUN.VALUE = numeric(1L))
  id_hyper <- which.min(cvm_min)
  lambda.min <- object_ext$model[[id_hyper]]$lambda[which.min(cvm[[id_hyper]])]
  # return fitted model
  object <- object_ext
  object$hyper <- hyper
  object$id_hyper <- id_hyper
  object$lambda.min <- lambda.min
  class(object) <- "cv.corila"
  object$y_obs <- y
  complete_x <- stats::complete.cases(x = x)
  object$y_fit <- stats::setNames(object = rep(x = NA, times = n),
                                  nm = rownames(x))
  object$y_fit[complete_x] <- stats::predict(object = object,
                                             newx = x[complete_x, ])
  object
}

#--- model fitting without cross-validation -----

#' @title
#' predict (S3 method)
#'
#' @description
#' Makes prediction from an object of class `"corila"`.
#'
#' @inheritParams predict.cv.corila
#'
#' @param object
#' object of class `"corila"`
#'
#' @param index
#' integer scalar specifying the index of the mixing hyperparameter(s)
#'
#' @param s
#' numeric vector specifying the values of the regularisation hyperparameter
#'
#' @return
#' Returns fitted or predicted values in an
#' \eqn{n_0 \times m}-dimensional or
#' \eqn{n_1 \times m}-dimensional matrix, respectively.
#'
#' @inherit corila-package references
#'
#' @seealso
#' Estimate parameters with [corila()],
#' or estimate parameters and tune hyperparameters
#' with [cv.corila()].
#'
#' @inherit corila examples
#'
#' @keywords internal
#'
#' @export
#'
predict.corila <- function(object, newx, index, s, ...) {
  # --- check arguments ---
  .assert(x = newx, type = "numeric",
          dim = c(Inf, length(object$scale$mu.x)))
  .assert(x = index, type = "integer", min = 1L, max = length(object$model))
  index <- as.integer(round(index))
  .assert(x = s, type = "numeric", dim = Inf, min = 0.0)
  # --- make predictions ---
  newx_stand <- .forescale(x = newx, pars = object$scale)$x
  y_hat_stand <- stats::predict(object = object$model[[index]],
                                newx = cbind(newx_stand, -newx_stand),
                                s = s,
                                type = "response")
  y_hat <- .backscale(y = y_hat_stand, pars = object$scale)$y
  y_hat
}

#' @title
#' Sparse group lasso regression (without cross-validation)
#'
#' @description
#' Fits an initial ridge regression to obtain weights
#' for an adaptive lasso regression
#' that allows for heterogeneous, overlapping and unknown groups
#' of correlated variables.
#'
#' @inheritParams cv.corila
#'
#' @param lambda_init
#' regularisation hyperparameter(s),
#' or `NULL` (cross-validation)
#'
#' @param hyper
#' list of \eqn{m}-dimensional vectors
#' or a data frame with \eqn{m} rows
#' containing candidate values
#' for the regularisation and mixing hyperparameters
#'
#' @param threshold
#' threshold for absolute correlation coefficients:
#' numeric in unit interval
#'
#' @details
#' The numbers of observations (samples) for training or testing
#' are indicated by \eqn{n_0} and \eqn{n_1}, respectively,
#' the number of predictors (features) is indicated by \eqn{p},
#' and the number of predictor group is indicated by \eqn{q}.
#' Observations are indexed by \eqn{i} in \eqn{\{1, \ldots, n\}},
#' predictors are indexed by \eqn{j} in \eqn{\{1, \ldots, p\}},
#' and predictor groups are indexed by \eqn{k} in \eqn{\{1, \ldots, q\}}.
#' The number of predictors in the \eqn{k^{\text{th}}} group
#' is indicated by \eqn{p_k},
#' with \eqn{\sum_{k=1}^q p_k = p} for non-overlapping groups.
#'
#' @return
#' Returns an object of class `"corila"`.
#'
#' @inherit corila-package references
#'
#' @seealso
#' Estimate parameters and tune hyperparameters (using cross-validation)
#' with [cv.corila()].
#' Make predictions for a range of hyperparameters
#' with [predict()][predict.corila].
#'
#' This function calls
#' [.forescale()] and [.backscale()]
#' for standardising data and bringing results back to the original scale,
#' respectively,
#' [.folds()] for splitting samples into folds,
#' [.estim_initial_coefs()] for obtaining initial coefficients,
#' [.is_adjacent()] for identifying adjacent predictors,
#' and [glmnet::cv.glmnet()] and [glmnet::glmnet()]
#' for adaptive lasso regression.
#'
#' @examples
#' \donttest{
#' \dontshow{corila <- corila:::corila}
#' # simulation
#' n <- 100L
#' p <- 50L
#' group <- rep(x = seq_len(10L), each = 5L)
#' primary <- rep(x = TRUE, times = p)
#' x <- matrix(data = rnorm(n * p), nrow = n, ncol = p)
#' y <- rnorm(n = n)
#'
#' # model fitting
#' hyper <- data.frame(exp_local = 1.0, wgt_local = 0.5,
#'                     exp_global = 1.0, wgt_global = 0.5)
#' object <- corila(x = x,
#'                  y = y,
#'                  group = group,
#'                  primary = primary,
#'                  family = "gaussian",
#'                  alpha_init = 0.0,
#'                  alpha_final = 1.0,
#'                  cor = "spearman",
#'                  foldid = NULL,
#'                  nfolds = 10L,
#'                  hyper = hyper,
#'                  lambda_init = NULL)
#'
#' y_hat <- stats::predict(object, newx = x, index = 1L, s = 0.0)
#' }
#'
#' @keywords internal
#'
corila <- function(x, y, group, primary, family, hyper, alpha_init,
                   alpha_final, cor, foldid,
                   nfolds, lambda_init, silent = FALSE, threshold = 0.0) {
  args <- .validate(
    x = x,
    y = y,
    group = group,
    primary = primary,
    family = family,
    hyper = hyper,
    alpha_init = alpha_init,
    alpha_final = alpha_final,
    cor = cor,
    foldid = NULL,
    nfolds = NULL,
    lambda_init = lambda_init,
    na_action = "error",
    silent = silent
  )
  #args <- as.list(match.call())[-1L]
  #do.call(what = .validate, args = args)
  p <- args$p
  args <- c(args, mget(setdiff(names(formals(corila)), c("x", "y"))))
  scale <- .forescale(x = x, y = y, family = family)
  rm(x, y)
  # --- initial coefficients ---
  init <- .estim_initial_coefs(x = scale$x,
                               y = scale$y,
                               family = family,
                               alpha_init = alpha_init,
                               group = group,
                               foldid = foldid,
                               nfolds = nfolds,
                               lambda = lambda_init)
  #--- feature correlation ---
  if (!is.matrix(cor)) {
    cor <- stats::cor(x = scale$x, method = cor, use = "pairwise.complete")
    cor[abs(cor) <= threshold] <- 0.0
  }
  cor[is.na(cor)] <- 0.0
  #--- regression ---
  model <- list()
  for (i in seq_len(nrow(hyper))) {
    weight <- list()
    weight$global <- weight$local <- rep(x = NA, times = p)
    for (j in seq_len(p)) {
      adjacent <- .is_adjacent(group = group, j = j, p = p,
                               names = colnames(scale$x))
      cor_trans <- sign(cor[, j]) * abs(cor[, j])^hyper$exp_local[i]
      temp <-  cor_trans * init$coef * adjacent
      weight$local[j] <- sum(pmax(0.0, temp)[adjacent]) / sum(adjacent)
      weight$local[p + j] <- sum(pmax(0.0, -temp)[adjacent]) / sum(adjacent)
      # ad-hoc solution for features that are in no group (unreachable?)
      # weight$local[is.na(weight$local)] <- 0
      temp <- sign(cor[, j]) * abs(cor[, j])^hyper$exp_global[i] * init$coef
      weight$global[j] <- sum(pmax(0.0, temp)) / p
      weight$global[p + j] <- sum(pmax(0.0, -temp)) / p
    }
    weight <- lapply(
      X = weight,
      FUN = function(x) p * ifelse(test = x == 0.0, yes = 0.0, no = x / sum(x))
    )
    pf_ext <- 1.0 / (weight$local * hyper$wgt_local[i] +
                       weight$global * hyper$wgt_global[i])
    pf_ext[!c(primary, primary)] <- Inf # exclude auxiliary features
    .assert(x = pf_ext, type = "numeric", dim = 2L * p, min = 0.0)
    model[[i]] <- suppressMessages(
      glmnet::glmnet(x = cbind(scale$x, -scale$x),
                     y = scale$y,
                     family = family,
                     penalty.factor = pf_ext,
                     lower.limits = 0.0,
                     alpha = alpha_final),
      classes = "message"[silent]
    )
  }
  structure(
    list(
      model = model,
      lambda_init = init$lambda,
      scale = scale$pars,
      args = args
    ),
    class = "corila"
  )
}

#----- class-specific helper functions -----

#' @title
#' Argument check
#'
#' @description
#' Checks arguments of functions [corila()] and [cv.corila()].
#'
#' @inheritParams cv.corila
#'
#' @details
#' This function is called by [corila()] and [cv.corila()].
#' It repeatedly calls [.assert()].
#'
#' @return
#' Returns a list with slots `n`, `p`, and `q` or an error message.
#'
#' @seealso
#' Use [.assert()] to validate individual arguments.
#'
#' @keywords internal
#'
#' @srrstats {G2.4} *verifying data types:*
#' @srrstats {G2.4a} *- integer*
#' @srrstats {G2.4b} *- numeric*
#' @srrstats {G2.4c} *- character*
#'
.validate <- function(na_action, x, y, group, primary, family, hyper,
                      alpha_init, alpha_final, cor,
                      foldid, nfolds, lambda_init, silent) {
  #--- na action ---
  .assert(x = na_action, type = "nominal",
          support = c("error", "complete_cases"))
  na.rm <- identical(na_action, "complete_cases")
  # --- feature matrix ---
  .assert(x = x, type = "numeric", dim = c(Inf, Inf), na.rm = na.rm)
  n <- nrow(x) # sample size
  if (n < 3L) {
    stop("Provide at least three observations.")
  }
  p <- ncol(x) # number of features
  if (p < 2L) {
    stop("Provide at least two predictors.")
  }
  # --- target vector ---
  .assert(x = family, type = "nominal",
          support = c("gaussian", "binomial", "poisson", "cox"))
  .assert(x = y, type = "numeric", dim = n, na.rm = na.rm, family = family)
  # --- group indicator ---
  if (is.vector(group) && is.atomic(group)) {
    if (is.numeric(group)) {
      .assert(x = group, type = "integer", dim = p, min = 1L, max = p)
      group <- as.integer(round(group))
    } else if (is.character(group)) {
      .assert(x = group, type = "nominal", dim = p)
    } else {
      stop("If argument 'group' is a vector, ",
           "it must be of class 'numeric' or 'character'.")
    }
    q <- length(unique(group))
  } else if (is.list(group)) {
    for (i in seq_along(group)) {
      if (is.numeric(group[[i]])) {
        .assert(x = group[[i]], type = "integer", dim = Inf,
                min = 1L, max = p)
        group[[i]] <- as.integer(round(group[[i]]))
      } else if (is.character(group[[i]])) {
        .assert(x = group[[i]], type = "nominal", dim = Inf,
                support = colnames(x))
      } else {
        stop("If argument 'group' is a list, ",
             "it must be a list of ",
             "numeric or character vectors.")
      }
    }
    q <- length(group)
  } else if (is.matrix(group)) {
    .assert(x = group, type = "integer", dim = c(p, p), min = 0L, max = 1L)
    group <- round(group)
    class(group) <- "integer"
    q <- NA
  } else {
    stop("Argument 'group' must be a vector, ",
         "a list, or a matrix.")
  }
  # --- other arguments ---
  .assert(x = primary, type = "logical", dim = p)
  slots <- c("wgt_local", "wgt_global", "exp_local", "exp_global")
  .assert(x = names(hyper), type = "nominal", dim = length(slots),
          support = slots)
  if (!is.null(hyper)) {
    hyper <- as.matrix(hyper)
  }
  .assert(x = hyper, type = "numeric", dim = c(Inf, length(slots)), min = 0.0)
  if (is.character(alpha_init)) {
    .assert(x = alpha_init, type = "nominal",
            support = c("pearson", "spearman", "kendall", "multiridge"))
  } else {
    .assert(x = alpha_init, type = "numeric",
            min = 0.0, max = 1.0, na.rm = TRUE)
  }
  .assert(x = alpha_final, type = "numeric", min = 0.0, max = 1.0)
  if (is.character(cor)) {
    .assert(x = cor, type = "nominal",
            support = c("pearson", "spearman", "kendall"))
  } else {
    .assert(x = cor, type = "numeric", dim = c(p, p), min = 0.0, max = 1.0)
  }
  .assert(x = foldid, type = "integer", dim = n, min = 1L, max = n)
  .assert(x = nfolds, type = "integer", min = 3L, max = n)
  .assert(x = lambda_init, type = "numeric", min = 0.0)
  .assert(x = silent, type = "logical")
  list(n = n, p = p, q = q)
}

#' @title
#' Candidate values
#'
#' @description
#' Sets candidate values for hyperparameters.
#'
#' @inheritParams cv.corila tune
#'
#' @return
#' Returns a data frame with
#' the slots `"wgt_local"` and `"exp_local"`
#' for the local prior information
#' and the slots `"wgt_global"` and `"exp_global"`
#' for the global prior information.
#'
#' @seealso
#' This function is called by [cv.corila()].
#'
#' @examples
#' \dontshow{.set_candidates <- corila:::.set_candidates}
#' .set_candidates(tune = "none")
#'
#' @keywords internal
#'
.set_candidates <- function(tune) {
  if (is.character(tune)) tune <- tolower(tune)
  .assert(x = tune, type = "nominal")
  if (identical(tune, "none")) {
    hyper <- data.frame(wgt_local = 1.0,
                        exp_local = 1.0,
                        wgt_global = 0.0,
                        exp_global = Inf)
  } else if (identical(tune, "weight")) {
    wgt_cand <- seq(from = 0.0, to = 1.0, by = 0.1)
    hyper <- data.frame(wgt_local = wgt_cand,
                        exp_local = 0.0,
                        wgt_global = 1.0 - wgt_cand,
                        exp_global = 1.0)
  } else if (identical(tune, "exponent")) {
    exp_cand <- c(0.0, 0.1, 0.25, 1.0 / 3.0, 0.5, 1.0, 2.0, 3.0, 4.0, 10.0, Inf)
    hyper <- data.frame(wgt_local = 1.0,
                        exp_local = exp_cand,
                        wgt_global = 0.0,
                        exp_global = Inf)
  } else if (identical(tune, "bivariate")) {
    wgt_cand <- seq(from = 0.0, to = 1.0, by = 0.1)
    hyper <- data.frame(wgt_local = wgt_cand,
                        exp_local = NA,
                        wgt_global = 1.0 - wgt_cand,
                        exp_global = NA)
    exp_cand <- c(0.1, 0.5, 0.8, 1.0, 1.25, 2.0, 10.0)
    hyper <- hyper[rep(seq_len(nrow(hyper)), each = length(exp_cand)), ]
    hyper$exp_local <- hyper$exp_global <- exp_cand
  } else if (identical(tune, "factorial")) {
    wgt_cand <- seq(from = 0.0, to = 1.0, by = 0.25)
    exp_cand <- c(0.1, 0.5, 1.0, 2.0, 10.0)
    hyper <- expand.grid(wgt_local = wgt_cand,
                         exp_local = exp_cand,
                         wgt_global = NA,
                         exp_global = exp_cand)
    hyper$wgt_global <- 1.0 - hyper$wgt_local
  } else {
    stop("Invalid value for argument 'tune'.")
  }
  hyper$exp_local[hyper$wgt_local < .Machine$double.eps] <- Inf
  hyper$exp_global[hyper$wgt_global < .Machine$double.eps] <- Inf
  hyper <- unique(hyper)
  rownames(hyper) <- seq_len(nrow(hyper))
  hyper
}

#' @title
#' Initial coefficients
#'
#' @description
#' Estimate initial coefficients.
#'
#' @inheritParams multiridge group
#' @inheritParams cv.corila
#'
#' @param lambda
#' numeric scalar, or `NULL`
#' (determined by cross-validation)
#'
#' @details
#' This function is called by [corila()].
#' It calls [glmnet::cv.glmnet()] or [glmnet::glmnet()]
#' for an initial lasso, ridge, or elastic net regression,
#' [multiridge()] for an initial multi-penalty ridge regression,
#' or [stats::cor()] for initial correlation coefficients.
#'
#' @return
#' Returns a list with two slots:
#' - `coef`: numeric vector of length \eqn{p}
#'   (estimated coefficients)
#' - `lambda`: non-negative numeric scalar
#'   (optimised regularisation parameter) or `NULL`
#'
#' @examples
#' \dontshow{.estim_initial_coefs <- corila:::.estim_initial_coefs}
#' # simulate data
#' set.seed(1)
#' n <- 50L
#' p <- 10L
#' x <- matrix(rnorm(n * p), nrow = n, ncol = p)
#' beta <- rbinom(n = p, size = 1L, prob = 0.5) * rnorm(p)
#' y <- drop(x %*% beta)
#'
#' # initial correlation coefficients
#' .estim_initial_coefs(x = x,
#'                      y = y,
#'                      family = "gaussian",
#'                      alpha_init = "spearman",
#'                      group = NULL,
#'                      foldid = NULL,
#'                      nfolds = 10L,
#'                      lambda = NULL)
#'
#' # initial regression coefficients (cross-validating lambda)
#' foldid <- sample(seq_len(10L), size = n, replace = TRUE)
#' .estim_initial_coefs(x = x,
#'                      y = y,
#'                      family = "gaussian",
#'                      alpha_init = 0.0,
#'                      group = NULL,
#'                      foldid = foldid,
#'                      nfolds = 10L,
#'                      lambda = NULL,
#'                      silent = TRUE)
#'
#' # initial regression coefficients (using fixed lambda)
#' .estim_initial_coefs(x = x,
#'                      y = y,
#'                      family = "gaussian",
#'                      alpha_init = 0.0,
#'                      group = NULL,
#'                      foldid = NULL,
#'                      nfolds = 10L,
#'                      lambda = 0.2)
#'
#' @keywords internal
#'
.estim_initial_coefs <- function(x, y, family = "gaussian", alpha_init = 0.0,
                                 group = NULL, foldid = NULL, nfolds = 10L,
                                 lambda = NULL, silent = FALSE) {
  # --- check arguments ---
  if (is.character(family)) family <- tolower(family)
  methods <- c("pearson", "spearman", "kendall", "multiridge")
  .assert(x = x, type = "numeric", dim = c(Inf, Inf))
  n <- nrow(x)
  p <- ncol(x)
  .assert(x = y, type = "numeric", dim = n)
  .assert(x = family, type = "nominal",
          support = c("gaussian", "binomial", "poisson", "cox"))
  if (is.character(alpha_init)) {
    alpha_init <- tolower(alpha_init)
    .assert(x = alpha_init, type = "nominal", support = methods)
  } else {
    .assert(x = alpha_init, type = "numeric", min = 0.0, max = 1.0,
            na.rm = TRUE)
    alpha_init <- pmax(0.0, pmin(alpha_init, 1.0))
  }
  .assert(x = group, type = "integer", dim = p, min = 1L, max = p)
  group <- as.integer(round(group))
  .assert(x = foldid, type = "integer", dim = n,
          min = 1L, max = n)
  if (!is.null(foldid)) foldid <- as.integer(round(foldid))
  .assert(x = nfolds, type = "integer", min = 3L, max = n)
  if (!is.null(nfolds)) nfolds <- as.integer(round(nfolds))
  if (identical(alpha_init, "multiridge")) {
    dim <- length(unique(group))
  } else {
    dim <- 1L
  }
  .assert(x = lambda, type = "numeric", dim = dim, min = 0.0)
  .assert(x = silent, type = "logical")
  # --- estimate initial coefficients ---
  is_slope <- rep(c(FALSE, TRUE), times = c(family != "cox", p))
  if (all(is.na(alpha_init))) {
    coef <- rep(x = 1.0, times = p)
  } else if (is.character(alpha_init) && identical(alpha_init, "multiridge")) {
    if (is.null(lambda)) {
      model <- multiridge(x = x,
                          y = y,
                          group = group,
                          family = family,
                          foldid = foldid,
                          nfolds = nfolds)
      coef <- stats::coef(object = model)[is_slope]
      lambda <- model$penalties
    } else {
      model <- multiridge(x = x,
                          y = y,
                          group = group,
                          family = family,
                          penalties = lambda)
      coef <- stats::coef(object = model)[is_slope]
    }
  } else if (is.character(alpha_init) && alpha_init %in% methods) {
    coef <- stats::cor(x = x,
                       y = y,
                       method = alpha_init,
                       use = "pairwise.complete")
    coef[is.na(coef)] <- 0.0
  } else if (is.numeric(alpha_init) && alpha_init >= 0.0 && alpha_init <= 1.0) {
    if (is.null(lambda)) {
      model <- suppressMessages(glmnet::cv.glmnet(x = x,
                                                  y = y,
                                                  family = family,
                                                  alpha = alpha_init,
                                                  foldid = foldid,
                                                  nfolds = nfolds),
                                classes = "message"[silent])
      coef <- stats::coef(object = model, s = "lambda.min")[is_slope]
      lambda <- model$lambda.min
    } else {
      model <- suppressMessages(glmnet::glmnet(x = x,
                                               y = y,
                                               family = family,
                                               alpha = alpha_init),
                                classes = "message"[silent])
      coef <- stats::coef(object = model, s = lambda)[is_slope]
    }
  }
  list(coef = drop(coef), lambda = lambda)
}

#' @title
#' Adjacency indicator
#'
#' @description
#' Identifies adjacent predictors.
#'
#' @inheritParams cv.corila group
#'
#' @param j
#' index of predictor
#' (positive integer between \eqn{1} and \eqn{p})
#'
#' @param p
#' number of predictors
#' (positive integer)
#'
#' @param names
#' names of predictors
#' (character vector of length \eqn{p})
#'
#' @details
#' This function is called by [corila()].
#' A predictor is adjacent to itself.
#' If argument `group` is a list (specifying potentially overlapping groups),
#' two predictors are adjacent if they are one or more common groups.
#'
#' @return
#' Returns a logical vector of length \eqn{p}.
#'
#' @examples
#' \dontshow{.is_adjacent <- corila:::.is_adjacent}
#' p <- 5L
#' names <- paste0("x", seq_len(p))
#' group <- list()
#' group$index_vector <- setNames(object = c(1L, 1L, 2L, 2L, 3L), nm = names)
#' group$label_vector <- setNames(object = LETTERS[group$index_vector],
#'                                  nm = names(group$index_vector))
#' group$index_list <- lapply(X = setNames(nm = unique(group$label_vector)),
#'                      FUN = function(x) which(group$label_vector == x))
#' group$label_list <- lapply(group$index_list, names)
#' group$matrix <- 1L * outer(X = group$index_vector,
#'                           Y = group$index_vector,
#'                           FUN = "==")
#' .is_adjacent(group = group[[1L]], j = 3L, p = p, names = names)
#'
#' @keywords internal
#'
.is_adjacent <- function(group, j, p, names) {
  .assert(x = j, type = "integer", min = 1L)
  j <- as.integer(round(j))
  .assert(x = p, type = "integer", min = j)
  p <- as.integer(round(p))
  .assert(x = names, type = "nominal", dim = p)
  if (is.atomic(group) && is.null(dim(group))) {
    if (is.numeric(group)) {
      .assert(x = group, type = "integer", dim = p, min = 1L, max = p)
      group <- as.integer(round(group))
    } else {
      .assert(x = group, type = "nominal", dim = p)
    }
    #if (length(group) != p) {
    #  stop("Vector 'group' should have length p.")
    #}
    group[j] == group
  } else if (is.list(group)) {
    if (length(group) == 0L) {
      stop("List 'group' should not have length 0.")
    }
    if (is.numeric(unlist(group))) {
      group_cond <- vapply(X = group,
                           FUN = function(slot) j %in% slot,
                           FUN.VALUE = logical(1L))
      stats::setNames(object = seq_len(p) %in% unlist(group[group_cond]),
                      nm = names)
    } else if (is.character(unlist(group))) {
      group_cond <- vapply(
        X = group,
        FUN = function(slot) names[j] %in% slot,
        FUN.VALUE = logical(1L)
      )
      stats::setNames(object = names %in% unlist(group[group_cond]),
                      nm = names)
    } else {
      stop("List 'group' should have slots of type numeric or character.")
    }
  } else if (is.matrix(group)) {
    .assert(x = group, type = "integer", dim = c(p, p), min = 0L, max = 1L)
    class(group) <- "integer"
    group[, j] == 1L
  } else {
    stop("Argument 'group' should be a vector, a list, or a matrix.")
  }
}
