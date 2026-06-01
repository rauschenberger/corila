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
#' @examples
#' n <- 20
#' p <- 10
#' x <- matrix(rnorm(n * p), nrow = n, ncol = p)
#' beta <- rbinom(n = p, size = 1, prob = 0.5) * rnorm(p)
#' y <- drop(x %*% beta)
#' corila:::.estim_initial_coefs(x = x,
#'                               y = y,
#'                               family = "gaussian",
#'                               alpha = "spearman",
#'                               group = NULL,
#'                               foldid = NULL,
#'                               nfolds = 10,
#'                               lambda = NULL)
#'
#' @keywords internal
#'
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
    stop("Invalid value for argument 'alpha'.")
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

#' @title
#' Candidate Values
#'
#' @description
#' Set candidate values for hyperparameters.
#'
#' @inheritParams cv.corila
#'
#' @return
#' Returns a data frame with
#' the slots "wgt_local" and "exp_local" for the local prior information
#' and the slots "wgt_global" and "exp_global" for the global prior information.
#'
#' @examples
#' corila:::.set_candidates(tune = "none")
#'
#' @keywords internal
#'
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
    hyper <- expand.grid(wgt_local = 0.5,
                         exp_local = exp_cand,
                         wgt_global = 0.5,
                         exp_global = exp_cand)
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
                         wgt_global = NA,
                         exp_global = exp_cand)
    hyper$wgt_global <- 1 - hyper$wgt_local
    hyper$exp_local[hyper$wgt_local == 0] <- Inf
    hyper$exp_global[hyper$wgt_global == 0] <- Inf
  } else {
    stop("Invalid value for argument 'tune'.")
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
#'
#' @seealso print.corila
#'
#' @export
summary.cv.corila <- function(object, ...) {
  list <- list()
  list$family <- object$args$family
  #list$n <- object$n
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

#' @title
#' Name (helper function)
#'
#' @description
#' Names the method used for obtaining initial or final coefficients.
#'
#' @param alpha
#' elastic net mixing parameter or character string
#' (see \code{alpha_init} and \code{alpha_final}
#' in \code{\link{cv.corila}()})
#'
#' @return
#' Returns a character string
#' ("ridge regression", "lasso regression", "elastic net regression",
#' "multi-penalty ridge regression",
#' or "Pearson/Spearman/Kendall correlation")
#'
#' @seealso
#' This function is called by \code{\link{print.summary.cv.corila}()}.
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
# Plot method for class \code{"cv.corila"}.
#
# @param x
# object of class \code{"cv.corila"}
#
# @param ...
# (not used)
#
# @return
# Returns \code{NULL} (invisible).
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
#' corila:::.combine_slopes(alpha = alpha, beta = beta)
#'
#' @keywords internal
#'
.combine_slopes <- function(alpha, beta) {
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
  coef <- .combine_slopes(alpha = alpha, beta = beta)
  coef <- .backscale(coef = coef, pars = object$scale)$coef
  if (any(coef[c(FALSE[object$scale$family != "cox"],
                 !object$args$include == 1)] != 0)) {
    stop("Excluded coefs must equal zero.")
  }
  coef[c(TRUE[object$scale$family != "cox"], object$args$include == 1)]
}
