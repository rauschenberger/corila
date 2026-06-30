# This file contains the functions of the R package "corila".

#----- group-lasso -----

#' @title
#' Initial coefficients
#'
#' @description
#' Estimate initial coefficients.
#'
#' @inheritParams corila
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
#' @examples
#' n <- 20
#' p <- 10
#' x <- matrix(rnorm(n * p), nrow = n, ncol = p)
#' beta <- rbinom(n = p, size = 1, prob = 0.5) * rnorm(p)
#' y <- drop(x %*% beta)
#' corila:::.estim_initial_coefs(x = x,
#'                               y = y,
#'                               family = "gaussian",
#'                               alpha_init = "spearman",
#'                               group = NULL,
#'                               foldid = NULL,
#'                               nfolds = 10,
#'                               lambda = NULL)
#'
#' @keywords internal
#'
.estim_initial_coefs <- function(x, y, family, alpha_init, group,
                                 foldid, nfolds, lambda) {
  # --- check arguments ---
  methods <- c("pearson", "spearman", "kendall", "multiridge")
  .assert(x = x, type = "numeric", dim = c(Inf, Inf))
  n <- nrow(x)
  p <- ncol(x)
  .assert(x = y, type = "numeric", dim = n)
  .assert(x = family, type = "nominal",
          support = c("gaussian", "binomial", "poisson", "cox"))
  if (is.character(alpha_init)) {
    .assert(x = alpha_init, type = "nominal", support = methods)
  } else {
    .assert(x = alpha_init, type = "numeric", min = 0, max = 1, na.rm = TRUE)
  }
  .assert(x = foldid, type = "integer", dim = n,
          min = 1, max = n)
  .assert(x = nfolds, type = "integer", min = 2, max = n)
  if (identical(alpha_init, "multiridge")) {
    dim <- length(unique(group))
  } else {
    dim <- 1
  }
  .assert(x = lambda, type = "numeric", dim = dim, min = 0)
  # --- estimate initial coefficients ---
  is_slope <- rep(c(FALSE, TRUE), times = c(family != "cox", p))
  if (all(is.na(alpha_init))) {
    coef <- rep(x = 1, times = p)
  } else if (is.character(alpha_init) && identical(alpha_init, "multiridge")) {
    if (is.null(lambda)) {
      model <- multiridge(x = x,
                          y = y,
                          z = group,
                          family = family,
                          foldid = foldid,
                          nfolds = nfolds)
      coef <- stats::coef(object = model)[is_slope]
      lambda <- model$penalties
    } else {
      model <- multiridge(x = x,
                          y = y,
                          z = group,
                          family = family,
                          penalties = lambda)
      coef <- stats::coef(object = model)[is_slope]
    }
  } else if (is.character(alpha_init) && alpha_init %in% methods) {
    coef <- stats::cor(x = x,
                       y = y,
                       method = alpha_init,
                       use = "pairwise.complete")
    coef[is.na(coef)] <- 0
  } else if (is.numeric(alpha_init) && alpha_init >= 0 && alpha_init <= 1) {
    if (is.null(lambda)) {
      model <- glmnet::cv.glmnet(x = x,
                                 y = y,
                                 family = family,
                                 alpha = alpha_init,
                                 foldid = foldid,
                                 nfolds = nfolds)
      coef <- stats::coef(object = model, s = "lambda.min")[is_slope]
      lambda <- model$lambda.min
    } else {
      model <- glmnet::glmnet(x = x,
                              y = y,
                              family = family,
                              alpha = alpha_init)
      coef <- stats::coef(object = model, s = lambda)[is_slope]
    }
  }
  list(coef = drop(coef), lambda = lambda)
}

# --- check arguments ---
#' @title
#' Argument check
#'
#' @description
#' Checks arguments of functions [corila()] and [cv.corila()].
#'
#' @inheritParams corila
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
.validate <- function(na_action, x, y, group, primary, family, hyper,
                      alpha_init, alpha_final, cor,
                      foldid, nfolds, lambda_init) {
  #--- na action ---
  .assert(x = na_action, type = "nominal",
          support = c("error", "complete_cases"))
  na.rm <- identical(na_action, "complete_cases")
  # --- feature matrix ---
  .assert(x = x, type = "numeric", dim = c(Inf, Inf), na.rm = na.rm)
  n <- nrow(x) # sample size
  p <- ncol(x) # number of features
  # --- target vector ---
  .assert(x = y, type = "numeric", dim = n, na.rm = na.rm)
  .assert(x = family, type = "nominal",
          support = c("gaussian", "binomial", "poisson", "cox"))
  if (identical(family, "gaussian")) {
    if (all(y %in% c(0, 1)) || all(y %in% c(-1, 1))) {
      stop("Gaussian family requires a numerical outcome.") # nocov
    }
  } else if (identical(family, "binomial")) {
    if (!all(y %in% c(0, 1))) {
      stop("Binomial family requires a binary outcome.") # nocov
    }
  } else if (identical(family, "poisson")) {
    if (any(y %% 1 != 0)) {
      stop("Poisson family requires a count outcome.") # nocov
    }
  } else if (identical(family, "cox")) {
    if (!inherits(x = y, what = "Surv")) {
      stop("Cox model requires a survival outcome.") # nocov
    }
  }
  # --- group indicator ---
  if (is.vector(group) && is.atomic(group)) {
    q <- length(unique(group))
    if (is.numeric(group)) {
      .assert(x = group, type = "integer", dim = p, min = 1, max = p)
    } else if (is.character(group)) {
      .assert(x = group, type = "nominal", dim = p)
    } else {
      stop("If argument 'group' is a vector, ",
           "it must be of class 'numeric' or 'character'.") # nocov
    }
  } else if (is.list(group)) {
    q <- length(group)
    for (i in seq_along(group)) {
      if (is.numeric(group[[i]])) {
        .assert(x = group[[i]], type = "integer", dim = Inf,
                min = 1, max = p)
      } else if (is.character(group[[i]])) {
        .assert(x = group[[i]], type = "nominal", dim = Inf,
                support = colnames(x))
      } else {
        stop("If argument 'group' is a list, ",
             "it must be a list of ",
             "numeric or character vectors.") # nocov
      }
    }
  } else if (is.matrix(group)) {
    q <- NA
    .assert(x = group, type = "integer", dim = c(p, p), min = 0, max = 1)
  } else {
    stop("Argument 'group' must be a vector, ",
         "a list, or a matrix.") # nocov
  }
  # --- other arguments ---
  .assert(x = primary, type = "logical", dim = p)
  slots <- c("wgt_local", "wgt_global", "exp_local", "exp_global")
  .assert(x = names(hyper), type = "nominal", dim = length(slots),
          support = slots)
  if (!is.null(hyper)) {
    hyper <- as.matrix(hyper)
  }
  .assert(x = hyper, type = "numeric", dim = c(Inf, length(slots)), min = 0)
  if (is.character(alpha_init)) {
    .assert(x = alpha_init, type = "nominal",
            support = c("pearson", "spearman", "kendall", "multiridge"))
  } else {
    .assert(x = alpha_init, type = "numeric", min = 0, max = 1, na.rm = TRUE)
  }
  .assert(x = alpha_final, type = "numeric", min = 0, max = 1)
  if (is.character(cor)) {
    .assert(x = cor, type = "nominal",
            support = c("pearson", "spearman", "kendall"))
  } else {
    .assert(x = cor, type = "numeric", dim = c(p, p), min = 0, max = 1)
  }
  .assert(x = foldid, type = "integer", dim = n, min = 1, max = n)
  .assert(x = nfolds, type = "integer", min = 1, max = n)
  .assert(x = lambda_init, type = "numeric", min = 0)
  list(n = n, p = p, q = q)
}

#' @title
#' Adjacency indicator
#'
#' @description
#' Identifies adjacent predictors.
#'
#' @inheritParams corila
#'
#' @param j
#' index of predictor
#'
#' @param p
#' number of predictors
#'
#' @param names
#' names of predictors
#'
#' @details
#' This function is called by [corila()].
#'
#' @return
#' Returns a logical vector of length \eqn{p}.
#'
#' @examples
#' p <- 5
#' names <- paste0("x", seq_len(p))
#' group <- list()
#' group$index_vector <- setNames(object = c(1, 1, 2, 2, 3), nm = names)
#' group$label_vector <- setNames(object = LETTERS[group$index_vector],
#'                                  nm = names(group$index_vector))
#' group$index_list <- lapply(X = setNames(nm = unique(group$label_vector)),
#'                      FUN = function(x) which(group$label_vector == x))
#' group$label_list <- lapply(group$index_list, names)
#' group$matrix <- 1 * outer(X = group$index_vector,
#'                           Y = group$index_vector,
#'                           FUN = "==")
#' corila:::.is_adjacent(group = group[[1]], j = 3, p = p, names = names)
#'
#' @keywords internal
#'
.is_adjacent <- function(group, j, p, names) {
  .assert(x = j, type = "integer", min = 1)
  .assert(x = p, type = "integer", min = j)
  .assert(x = names, type = "nominal", dim = p)
  if (is.vector(group) && is.atomic(group)) {
    group[j] == group
  } else if (is.list(group)) {
    if (is.numeric(unlist(group))) {
      group_cond <- vapply(X = group,
                           FUN = function(slot) j %in% slot,
                           FUN.VALUE = logical(1))
      seq_len(p) %in% unlist(group[group_cond])
    } else if (is.character(unlist(group))) {
      group_cond <- vapply(
        X = group,
        FUN = function(slot) names[j] %in% slot,
        FUN.VALUE = logical(1)
      )
      names %in% unlist(group[group_cond])
    } else {
      stop("The list 'group' should have slots of type numeric or character.")
    }
  } else if (is.matrix(group)) {
    group[, j] == 1
  } else {
    stop("Argument 'group' should be a vector, a list, or a matrix.")
  }
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
#' The number of observations (samples) for training or testing
#' are indicated by \eqn{n_0} and \eqn{n_1}, respectively,
#' the number of variables (features) is indicated by \eqn{p},
#' and the number of variable groups is indicated by \eqn{q}.
#' Observations (samples) are indexed by \eqn{i} in \eqn{\{1, \ldots, n\}},
#' variables (features) are indexed by \eqn{j} in \eqn{\{1, \ldots, p\}},
#' and variable groups are indexed by \eqn{k} in \eqn{\{1, \ldots, q\}}.
#' The number of variables in the \eqn{k^{\text{th}}} group
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
#' # simulation
#' n <- 100
#' p <- 50
#' group <- rep(x = 1:10, each = 5)
#' primary <- NULL
#' x <- matrix(data = rnorm(n * p), nrow = n, ncol = p)
#' y <- rnorm(n = n)
#'
#' # model fitting
#' hyper <- data.frame(exp_local = 1, wgt_local = 0.5,
#'                     exp_global = 1, wgt_global = 0.5)
#' object <- corila:::corila(x = x,
#'                           y = y,
#'                           group = group,
#'                           primary = primary,
#'                           family = "gaussian",
#'                           alpha_init = 0,
#'                           alpha_final = 1,
#'                           cor = "spearman",
#'                           foldid = NULL,
#'                           nfolds = 10,
#'                           hyper = hyper,
#'                           lambda_init = NULL)
#'
#' y_hat <- predict(object, newx = x, index = 1, s = 0)
#' }
#'
#' @keywords internal
#'
corila <- function(x, y, group, primary, family, hyper, alpha_init,
                   alpha_final, cor, foldid,
                   nfolds, lambda_init, threshold = 0) {
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
    foldid = foldid,
    nfolds = nfolds,
    lambda_init = lambda_init,
    na_action = "error"
  )
  #args <- as.list(match.call())[-1]
  #do.call(what = .validate, args = args)
  p <- args$p
  if (identical(alpha_init, "multiridge") && identical(family, "poisson")) {
    warning("Setting alpha_init=0 due to family='poisson'.")
    alpha_init <- 0
  }
  if (is.null(group)) {
    group <- seq_len(p)
  }
  if (is.null(primary)) {
    primary <- rep(x = TRUE, times = p)
  }
  args <- c(args, mget(setdiff(names(formals(corila)), c("x", "y"))))
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
                               alpha_init = alpha_init,
                               group = group,
                               foldid = foldid,
                               nfolds = nfolds,
                               lambda = lambda_init)
  #--- feature correlation ---
  if (!is.matrix(cor)) {
    cor <- stats::cor(x = scale$x, method = cor, use = "pairwise.complete")
    cor[abs(cor) <= threshold] <- 0
  }
  cor[is.na(cor)] <- 0
  #--- regression ---
  model <- list()
  for (i in seq_len(nrow(hyper))) {
    weight <- list()
    weight$global <- weight$local <- rep(x = NA, times = p)
    # rename to weight$local and weight$global
    for (j in seq_len(p)) {
      adjacent <- .is_adjacent(group = group, j = j, p = p,
                               names = colnames(scale$x))
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
    pf_ext[!c(primary, primary)] <- Inf # excluded features
    #if (any(is.na(pf_ext))) {
    #  stop("missing pf: ", sum(is.na(pf_ext)))
    #}
    #if (any(pf_ext < 0)) {
    #  stop(paste0("negative pf:", min(pf_ext)))
    #}
    .assert(x = pf_ext, type = "numeric", dim = 2 * p, min = 0)
    model[[i]] <- glmnet::glmnet(x = cbind(scale$x, -scale$x),
                                 y = scale$y,
                                 family = family,
                                 penalty.factor = pf_ext,
                                 lower.limits = 0,
                                 alpha = alpha_final)
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
#' @param ... (not used)
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
#' @export
#'
predict.corila <- function(object, newx, index, s, ...) {
  # --- check arguments ---
  .assert(x = newx, type = "numeric",
          dim = c(Inf, length(object$scale$mu.x)))
  .assert(x = index, type = "integer", min = 1, max = length(object$model))
  .assert(x = s, type = "numeric", dim = Inf, min = 0)
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
#' Candidate values
#'
#' @description
#' Sets candidate values for hyperparameters.
#'
#' @inheritParams cv.corila
#'
#' @return
#' Returns a data frame with
#' the slots `"wgt_local"` and `"exp_local"`
#' for the local prior information
#' and the slots `"wgt_global"` and `"exp_global"`
#' for the global prior information.
#'
#' @examples
#' corila:::.set_candidates(tune = "none")
#'
#' @keywords internal
#'
.set_candidates <- function(tune) {
  .assert(x = tune, type = "nominal")
  if (identical(tune, "none")) {
    hyper <- data.frame(wgt_local = 1,
                        exp_local = 1,
                        wgt_global = 0,
                        exp_global = Inf)
  } else if (identical(tune, "weight")) {
    wgt_cand <- seq(from = 0, to = 1, by = 0.1)
    hyper <- data.frame(wgt_local = wgt_cand,
                        exp_local = 0,
                        wgt_global = 1 - wgt_cand,
                        exp_global = 1)
  } else if (identical(tune, "exponent")) {
    exp_cand <- c(0, 0.1, 0.25, 1 / 3, 0.5, 1, 2, 3, 4, 10, Inf)
    hyper <- data.frame(wgt_local = 1,
                        exp_local = exp_cand,
                        wgt_global = 0,
                        exp_global = Inf)
  } else if (identical(tune, "bivariate")) {
    wgt_cand <- seq(from = 0, to = 1, by = 0.1)
    hyper <- data.frame(wgt_local = wgt_cand,
                        exp_local = NA,
                        wgt_global = 1 - wgt_cand,
                        exp_global = NA)
    exp_cand <- c(0.1, 0.5, 0.8, 1, 1.25, 2, 10)
    hyper <- hyper[rep(seq_len(nrow(hyper)), each = length(exp_cand)), ]
    hyper$exp_local <- hyper$exp_global <- exp_cand
  } else if (identical(tune, "factorial")) {
    wgt_cand <- seq(from = 0, to = 1, by = 0.25)
    exp_cand <- c(0.1, 0.5, 1, 2, 10)
    hyper <- expand.grid(wgt_local = wgt_cand,
                         exp_local = exp_cand,
                         wgt_global = NA,
                         exp_global = exp_cand)
    hyper$wgt_global <- 1 - hyper$wgt_local
  } else {
    stop("Invalid value for argument 'tune'.")
  }
  hyper$exp_local[hyper$wgt_local == 0] <- Inf
  hyper$exp_global[hyper$wgt_global == 0] <- Inf
  hyper <- unique(hyper)
  rownames(hyper) <- seq_len(nrow(hyper))
  hyper
}

#' @title
#' Sparse group lasso regression
#'
#' @description
#' Optimises the parameters and the hyperparameters of the sparse group lasso.
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
#' group structure (three options):
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
#' elastic net mixing parameter
#' (\eqn{0 \leq} `alpha_init` \eqn{\leq 1})
#' for initial regression
#' (default: ridge penalisation with `alpha_init`=0);
#' alternative choices are
#' `"pearson"`, `"spearman"`, or `"kendall"`
#' to use initial correlation coefficients
#' (not implemented for `family="cox"`),
#' `"multiridge"` for multi-penalty ridge regression
#' with one penalty for each group
#' (not implemented for `family="poisson"` or overlapping groups),
#' or `NA` to set all initial coefficients equal to 1
#'
#' @param alpha_final
#' elastic net mixing parameter for final regression
#' (default: lasso penalisation with `alpha_final`=1)
#'
#' @param family
#' character string `"gaussian"`, `"binomial"`,
#' `"poisson"`, or `"cox"`
#'
#' @param foldid
#' \eqn{n_0}-dimensional vector containing the fold identifiers
#'
#' @param nfolds
#' integer specifying the number of folds
#'
#' @param cor
#' character string `"pearson"`,
#' `"spearman"` (default),
#' or `"kendall"`;
#' or \eqn{p \times p} correlation matrix
#'
#' @param tune
#' character string for determining the candidate values
#' for the hyperparameters:
#' - "none":
#' fixed weights and exponents
#' (`wgt_local`=1, `exp_local`=1, `wgt_global`=0),
#' no tuning
#' - "weight":
#' fixed exponents (`exp_local`=0, `exp_global`=1),
#' tuning `wgt_local`=1-`wgt_global`
#' - "exponent":
#' fixed weights (`wgt_local`=1, `wgt_global`=0),
#' tuning `exp_local`
#' - "bivariate":
#' tuning `wgt_local`=1-`wgt_global` and `exp_local`=`exp_global`
#' - "factorial":
#' tuning `wgt_local`, `exp_local`, `wgt_global`, `exp_global`
#'
#' (to implement: list with slots
#' `wgt_local`, `exp_local`, `wgt_global`, and `exp_global`)
#'
#' @param na_action
#' character `"error"` to trigger an error
#' if any observation has a missing predictor or a missing response
#' or `"complete_cases"` to omit observations
#' with a missing predictor or a missing response
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
#' # minimal example
#' n <- 50; p <- 20; q <- 5
#' x <- matrix(rnorm(n * p), nrow = n , ncol = p)
#' y <- rnorm(n)
#' group <- rep(seq_len(q), length.out = p)
#' primary <- as.logical(rbinom(n = p, size = 1, prob = 0.5))
#' cv.corila(x = x, y = y, group = group, primary = primary, tune = "none")
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
#' #primary <- stats::rbinom(n = sum(p), size = 1, prob = 0.5) == 1
#' #object <- cv.corila(x = x[cond, ], y = y[cond], group = z,
#' #                     primary = primary, family = family)
#' }
#'
#' @keywords methods models regression classif
#'
#' @export
#'
#' @srrstats {G2.3b} *uses tolower() for arguments family and cor*
#' @srrstats {RE4.0} *returns a "model" object (see @return)*
#' @srrstats {G2.14} *uses argument na_action*
#' @srrstats {G2.14a} *to trigger an error on missing data*
#' @srrstats {G2.14b} *to ignore observations with missing data*
#' @srrstats {G2.0a} *lengths of vector inputs are documented*
#' @srrstats {G2.1a} *data types of vector inputs are documented*
#'
cv.corila <- function(x, y, group, primary = NULL, alpha_init = 0,
                      alpha_final = 1, family = "gaussian",
                      nfolds = 10, cor = "spearman", tune = "weight",
                      foldid = NULL, na_action = "error") {
  # match arguments
  family <- match.arg(arg = tolower(family),
                      choices = c("gaussian", "binomial", "poisson", "cox"))
  if (is.character(cor)) {
    cor <- match.arg(arg = tolower(cor),
                     choices = c("pearson", "spearman", "kendall"))
  }
  # set default parameters
  if (is.null(primary)) {
    primary <- rep(x = TRUE, times = ncol(x))
  }
  if (is.null(foldid)) {
    foldid <- .folds(y = y, family = family, nfolds = nfolds)
  }
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
    hyper = hyper
  )
  if (identical(na_action, "complete_cases")) {
    complete <- stats::complete.cases(x = x, y = y)
    warning("Ingoring ",
            sum(!complete),
            " observations with missing data.")
    x <- x[complete, ]
    y <- y[complete]
    foldid <- foldid[complete]
  }
  # fit model on all folds
  object_ext <- corila(x = x,
                       y = y,
                       group = group,
                       primary = primary,
                       family = family,
                       alpha_init = alpha_init,
                       alpha_final = alpha_final,
                       cor = cor,
                       foldid = foldid,
                       nfolds = NULL,
                       hyper = hyper,
                       lambda_init = NULL)
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
                         primary = primary,
                         family = family,
                         alpha_init = alpha_init,
                         alpha_final = alpha_final,
                         cor = cor,
                         foldid = NULL,
                         nfolds = NULL,
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
  object$hyper <- hyper
  object$id_hyper <- id_hyper
  object$lambda.min <- lambda.min
  class(object) <- "cv.corila"
  object$y_obs <- y
  object$y_fit <- predict(object = object, newx = x)
  object
}

#' @title
#' print (S3 method)
#'
#' @description
#' Print method for class `"cv.corila"`.
#'
#' @param x
#' object of class `"cv.corila"`.
#'
#' @param ...
#' (not used)
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
  content <- ifelse(length(x$model) == 1, "an object", "multiple objects")
  cat("(contains ", content, " of class ", sQuote("cv.glmnet"), ")\n", sep = "")
  nzero <- sum(stats::coef(x, s = "lambda.min")[-1] != 0)
  cat("selected", nzero, "from", x$args$p, "predictors")
  invisible(x)
}

#' @title
#' Observation Count
#'
#' @description
#' Extracts the number of observations.
#'
#' @param object
#' object of class `cv.corila`
#'
#' @export
#'
#' @srrstats {RE4.5} *number of observations (via `nobs()`)*
#'
nobs.cv.corila <- function(object, ...) {
  object$args$n
}

#' @title
#' Deviance
#'
#' @description
#' Calculates the deviance.
#'
#' @param object
#' object of class \code{cv.corila}
#'
#' @details
#' Returns the deviance calculated by [glmnet::deviance.glmnet()]
#' for the model with the optimised mixing and regularisation hyperparameters.
#' 
#' @export
#' 
deviance.cv.corila <- function(object, ...) {
  model <- object$model[[object$id_hyper]]
  stats::deviance(model)[model$lambda == object$lambda.min]
}

if(TRUE){ # move this to unit tests
  # general
  n <- 100
  x <- rnorm(n)
  # gaussian
  y <- x + rnorm(n)
  lm <- lm(y~x)
  y_hat <- fitted(lm)
  resid <- y-y_hat
  all.equal(resid,residuals(lm))
  # binomial
  y <- rbinom(n = n, size = 1, prob = 0.5)
  lm <- glm(y ~ x, family = "binomial")
  y_hat <- fitted(lm)
  eps <- 1e-06
  resid <-  - y * log(pmax(y_hat, eps)) - (1 - y) * log(1 - pmin(y_hat, 1 - eps))
  plot(x = residuals(lm), y = resid)
  # poisson
  y <- rpois(n = n, lambda = 4)
  lm <- glm(y ~ x, family = "poisson")
  y_hat <- fitted(lm)
  resid <- 2 * (ifelse(y == 0, 0, y * log(y / y_hat)) - y + y_hat)
  plot(x = residuals(lm), y = resid)
  
}

residuals.cv.corila <- function(object, ...) {
  if(object$args$family=="gaussian") {
    object$y_obs - object$y_fit
  } else {
    stop("Not implemented.")
  }
}

#' @title
#' Summarising sparse group lasso (S3 method)
#'
#' @description
#' Summary method for class `"cv.corila"`.
#'
#' @param object
#' object of class `"cv.corila"`
#'
#' @param x
#' object of class `"summary.cv.corila"`
#'
#' @param ...
#' (not used)
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

#' @title
#' Name method (helper function)
#'
#' @description
#' Names the method used for obtaining initial or final coefficients.
#'
#' @param alpha
#' elastic net mixing parameter or character string
#' (see `alpha_init` and `alpha_final`
#' in [cv.corila()])
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
#' corila:::.type(alpha = 0)
#'
#' @keywords internal
#'
.type <- function(alpha) {
  if (is.na(alpha)) {
    "none"
  } else if (is.numeric(alpha)) {
    if (alpha == 0) {
      "ridge regression"
    } else if (alpha == 1) {
      "lasso regression"
    } else if (alpha > 0 && alpha < 1) {
      "elastic net regression"
    } else {
      stop("If argument 'alpha' is numeric, ",
           "it should be in the unit interval.")
    }
  } else {
    if (identical(alpha, "multiridge")) {
      "multi-penalty ridge regression"
    } else if (alpha %in% c("pearson", "spearman", "kendall")) {
      paste0(toupper(substr(x = alpha, start = 1, stop = 1)),
             tolower(substr(x = alpha, start = 2, stop = nchar(alpha))),
             " correlation")
    } else {
      stop("If argument 'alpha' is of type 'character', ",
           "it should equal ",
           "'pearson', 'spearman', 'kendall', or 'multiridge'.")
    }
  }
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
      signif(x$lambda.min, digits = 4), "\n")
  cat("selected weights: local = ", x$wgt_local,
      ", global = ", x$wgt_global, "\n", sep = "")
  cat("selected exponents: local = ", x$exp_local,
      ", global = ", x$exp_global, "\n", sep = "")
  cat(x$nzero, "non-zero coefficients",
      "(including intercept)"[x$family != "cox"])
  invisible(NULL)
}

# @title
# Plot Sparse Group Lasso (S3 method)
#
# @description
# Plot method for class `"cv.corila"`.
#
# @param x
# object of class `"cv.corila"`
#
# @param ...
# (not used)
#
# @return
# Returns `NULL` (invisible).
#
# @inherit summary.cv.corila examples
#
# plot.cv.corila <- function(x, ...) {
#  # observed vs fitted values
#  # estimated coefficient per group (if vector)
#  # cvm as a functions of weights and exponents
#  invisible(NULL)
#}

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
#' @examples
#' n <- 5
#' p <- 10
#' x <- matrix(data = rnorm(n * p), nrow = n, ncol = p)
#' primary <- as.logical(rbinom(n = p, size = 1, prob = 0.5))
#' x_primary <- x[, primary]
#' x_expanded <- corila:::.expand_auxiliary(x = x_primary, primary = primary)
#' all(x_expanded[, primary] == x[, primary])
#' all(x_expanded[, !primary] == 0)
#'
#' @keywords internal
#'
.expand_auxiliary <- function(x, primary) {
  .assert(x = x, type = "numeric", dim = c(Inf, Inf), na.rm = TRUE)
  .assert(x = primary, type = "logical", dim = Inf)
  if (ncol(x) == length(primary)) {
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
#' (not used)
#'
#' @inherit predict.corila return
#'
#' @inherit corila-package references
#'
#' @seealso
#' Fit models with [cv.corila()]
#' and extract coefficients with [coef()][coef.cv.corila].
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
  } else if (!is.numeric(s) || length(s) != 1 || s < 0) {
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
#' Returns a numeric vector of length \eqn{1 + p}.
#'
#' @examples
#' p <- 10
#' alpha <- rnorm(1)
#' temp <- rnorm(p)
#' beta <- pmax(c(temp, -temp), 0)
#' corila:::.combine_slopes(alpha = alpha, beta = beta)
#'
#' @keywords internal
#'
.combine_slopes <- function(alpha, beta) {
  .assert(x = alpha, type = "numeric")
  .assert(x = beta, type = "numeric", dim = Inf, min = 0)
  beta_positive <- beta[1:(length(beta) / 2)]
  beta_negative <- beta[(length(beta) / 2 + 1):(length(beta))]
  eps <- 1e-06
  if (any(beta_positive > eps & beta_negative > eps)) {
    stop("A predictor must not have ",
         "a positive and a negative coefficient.") # nocov
  }
  beta_combined <- beta_positive  - beta_negative
  c(alpha, beta_combined)
}

#' @title
#' Extract coefficients
#'
#' @description
#' Extracts coefficients from an object of class `"cv.corila"`.
#'
#' @inheritParams predict.cv.corila
#'
#' @param ...
#' (not used)
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
  } else if (!is.numeric(s) || length(s) != 1 || s < 0) {
    stop("Set s='lambda.min' or provide non-negative scalar.")
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
  coef <- .combine_slopes(alpha = alpha, beta = beta)
  coef <- .backscale(coef = coef, pars = object$scale)$coef
  if (any(coef[c(FALSE[object$scale$family != "cox"],
                 !object$args$primary)] != 0)) {
    stop("Excluded coefficients must equal zero.") # nocov
  }
  coef[c(TRUE[object$scale$family != "cox"], object$args$primary)]
}
