
#' @title
#' Validation functions
#'
#' @description
#' These functions validate the arguments
#' of the function [cv.corila()], its helper functions, and its S3 methods.
#' They check whether the provided arguments satisfy expectations,
#' and return them in standardised forms
#' (e.g., as integers instead of integer-like numerics).
#'
#' @inheritParams cv.corila
#' @inheritParams .residuals
#'
#' @param names
#' character vector
#' of length \eqn{n} or \eqn{p}
#' for names of observations or predictors
#'
#' @details
#' These functions are called by [cv.corila()],
#' its helper functions, and its S3 methods.
#'
#' @return
#' Return the first argument invisibly.
#' Throw an error for invalid arguments.
#'
#' @keywords internal
#'
#' @srrstats {G2.0} *implements assertions on lengths of inputs*
#' @srrstats {G2.1} *rejects unexpected input types*
#' @srrstats {G2.2} *rejects multivariate input if expecting univariate input*
#' @srrstats {G2.15} *rejects missing values by default*
#' @srrstats {G2.3a} *rejects unexpected values*
#' @srrstats {G2.4} *verifying data types:*
#' @srrstats {G2.4a} *- integer*
#' @srrstats {G2.4b} *- numeric*
#' @srrstats {G2.4c} *- character*
#' @srrstats {G2.13} *checks for missing data*
#' @srrstats {G5.2a} *messages are unique*
#' @srrstats {RE1.4} *tests assumptions for input data*
#'
#' @name validate
NULL

#' @rdname validate
.validate_na_action <- function(na_action) {
  if (is.character(na_action)) na_action <- tolower(na_action)
  checkmate::assert_choice(
    x = na_action, choices = c("error", "complete_cases")
  )
}

#' @rdname validate
.validate_family <- function(family, poisson = TRUE) {
  checkmate::assert_logical(x = poisson, any.missing = FALSE, len = 1L)
  if (is.character(family)) family <- tolower(family)
  checkmate::assert_choice(
    x = family,
    choices = c("gaussian", "linear", "binomial", "logistic",
                "poisson"[poisson], "cox")
  )
  if (family == "linear") {
    "gaussian"
  } else if (family == "logistic") {
    "binomial"
  } else {
    family
  }
}

#' @rdname validate
.validate_x <- function(x, na_action) {
  checkmate::assert_matrix(x = x, mode = "numeric",
                           any.missing = (na_action == "complete_cases"),
                           all.missing = FALSE,
                           min.rows = 3L, min.cols = 2L)
  checkmate::assert_character(x = rownames(x), unique = TRUE,
                              null.ok = TRUE, any.missing = FALSE)
  checkmate::assert_character(x = colnames(x), unique = TRUE,
                              null.ok = TRUE, any.missing = FALSE)
  x
}

#' @rdname validate
.validate_y <- function(y, family, n, na_action, names) {
  checkmate::assert_count(x = n, positive = TRUE, null.ok = TRUE)
  checkmate::assert_character(x = names, len = n, null.ok = TRUE)
  if (length(y) > 1L) y <- drop(y)
  eps <- 1e-06
  if (!is.null(n) && family == "cox") n <- 2L * n
  checkmate::assert_numeric(
    x = y, all.missing = FALSE,
    any.missing = (na_action == "complete_cases"), len = n
  )
  if (!is.null(names) && !is.null(names(y))) {
    checkmate::assert_names(x = names(y), identical.to = names)
  }
  if (identical(family, "cox") != inherits(y, "Surv")) {
    stop("Expects survival response if and only if Cox model.")
  }
  if (identical(family, "binomial")) {
    checkmate::assert_integerish(x = y, lower = 0.0 - eps, upper = 1.0 + eps)
    as.integer(round(y))
  } else if (identical(family, "poisson")) {
    checkmate::assert_integerish(x = y, lower = 0.0 - eps)
    as.integer(round(y))
  } else {
    y
  }
}

#' @rdname validate
.validate_y_hat <- function(y_hat, family, n) {
  checkmate::assert_count(x = n, positive = TRUE)
  eps <- 1e-06
  checkmate::assert_numeric(x = y_hat, any.missing = FALSE, len = n)
  if (identical(family, "binomial")) {
    checkmate::assert_numeric(x = y_hat, lower = 0.0 - eps, upper = 1.0 + eps)
    pmax(0.0, pmin(y_hat, 1.0))
  } else if (identical(family, "poisson")) {
    checkmate::assert_numeric(x = y_hat, lower = 0.0 - eps)
    pmax(0, y_hat)
  } else {
    y_hat
  }
}

#' @rdname validate
.validate_primary <- function(primary, p, names) {
  checkmate::assert_count(x = p, positive = TRUE)
  checkmate::assert_character(x = names, len = p, null.ok = TRUE)
  if (is.null(primary)) {
    stats::setNames(object = rep(x = TRUE, times = p), nm = names)
  } else {
    checkmate::assert_logical(x = primary, any.missing = FALSE, len = p)
    checkmate::assert_count(x = sum(primary))
    if (!is.null(names) && !is.null(names(primary))) {
      checkmate::assert_names(x = names(primary), identical.to = names)
    }
    drop(primary)
  }
}

#' @rdname validate
.validate_cor <- function(cor, p, names) {
  eps <- 1e-06
  checkmate::assert_count(x = p, positive = TRUE)
  checkmate::assert_character(x = names, len = p, null.ok = TRUE)
  if (is.character(cor)) {
    cor <- tolower(cor)
    checkmate::assert_choice(x = cor,
                             choices = c("pearson", "spearman", "kendall"))
  } else if (is.matrix(cor)) {
    checkmate::assert_matrix(x = cor, mode = "numeric", any.missing = FALSE,
                             nrows = p, ncols = p)
    checkmate::assert_numeric(x = cor, lower = - 1.0 - eps, upper = 1.0 + eps)
    checkmate::assert_numeric(x = diag(cor), lower = 1.0 - eps,
                              upper = 1.0 + eps)
    if (!isSymmetric(cor)) stop("Matrix 'cor' must be symmetric.")
    pmax(-1.0, pmin(cor, 1.0))
  } else {
    stop("Argument 'cor' must be either a single character or a matrix.")
  }
}

#' @rdname validate
.validate_alpha <- function(alpha, init) {
  checkmate::assert_logical(x = init, any.missing = FALSE, len = 1L)
  eps <- 1e-06
  if (is.character(alpha)) {
    choices <- c("ridge", "lasso")
    if (init) {
      choices <- c(choices, c("pearson", "spearman", "kendall", "multiridge"))
    }
    alpha <- tolower(alpha)
    checkmate::assert_choice(x = alpha, choices = choices)
    if (alpha == "ridge") alpha <- 0.0
    if (alpha == "lasso") alpha <- 1.0
    alpha
  } else if (is.numeric(alpha)) {
    checkmate::assert_number(x = alpha,
                             lower = 0.0 - eps, upper = 1.0 + eps)
    pmax(0.0, pmin(alpha, 1.0))
  } else if (is.logical(alpha) && length(alpha) == 1 && is.na(alpha)) {
    NA
  } else {
    stop("Argument 'alpha' must be ",
         "either a single character or a single numeric.")
  }
}

#' @rdname validate
.validate_group <- function(group, p, names) {
  eps <- 1e-06
  checkmate::assert_count(x = p, positive = TRUE)
  checkmate::assert_character(x = names, len = p, null.ok = TRUE)
  group <- drop(group)
  if (is.vector(group) && is.atomic(group)) {
    if (is.numeric(group)) {
      checkmate::assert_integerish(x = group, len = p,
                                   lower = 1.0 - eps, upper = p + eps)
      as.integer(round(group))
    } else if (is.character(group)) {
      checkmate::assert_character(x = group, any.missing = FALSE, len = p)
    } else {
      stop("If argument 'group' is a vector, ",
           "it must be of class 'numeric' or 'character'.")
    }
  } else if (is.list(group)) {
    checkmate::assert_list(x = group, min.len = 1L, any.missing = FALSE)
    values <- unlist(group)
    if (all(is.numeric(values))) {
      checkmate::assert_integerish(
        x = values, lower = 1.0 - eps, upper = p + eps,
        any.missing = FALSE, .var.name = "unlist(group)"
      )
      lapply(X = group, FUN = function(x) as.integer(round(x)))
    } else if (all(is.character(values))) {
      checkmate::assert_character(x = names, any.missing = FALSE, len = p,
                                  unique = TRUE)
      checkmate::assert_character(x = values, any.missing = FALSE,
                                  min.len = 1L, .var.name = "unlist(group)")
      checkmate::assert_subset(x = values, choices = names,
                               .var.name = "unlist(group)")
      group
    } else {
      stop("If argument 'group' is a list, ",
           "it must be a list of ",
           "either numeric vectors or character vectors.")
    }
  } else if (is.matrix(group)) {
    checkmate::assert_matrix(x = group, mode = "integerish",
                             nrows = p, ncols = p)
    if (!is.null(names) && !is.null(rownames(group))) {
      checkmate::assert_names(x = rownames(group), identical.to = names)
    }
    if (!is.null(names) && !is.null(colnames(group))) {
      checkmate::assert_names(x = colnames(group), identical.to = names)
    }
    checkmate::assert_numeric(x = group, lower = 0.0 - eps, upper = 1.0 + eps)
    group <- round(group)
    class(group) <- "integer"
    group
  } else {
    stop("Argument 'group' must be a vector, ",
         "a list, or a matrix.")
  }
}

#' @rdname validate
.validate_hyper <- function(hyper) {
  eps <- 1e-06
  slots <- c("wgt_local", "exp_local", "wgt_global", "exp_global")
  checkmate::assert_data_frame(x = hyper, types = "numeric",
                               any.missing = FALSE,
                               min.rows = 1L, ncols = 4L)
  checkmate::assert_names(x = names(hyper), identical.to = slots)
  checkmate::assert_numeric(x = unlist(hyper), lower = 0.0 - eps)
  hyper[hyper < 0.0] <- 0.0
  hyper
}

#' @rdname validate
.validate_foldid <- function(foldid, y, family) {
  eps <- 1e-06
  n <- length(y)
  checkmate::assert_integerish(x = foldid, lower = 1.0 - eps,
                               upper =  n + eps, len = n, any.missing = FALSE)
  foldid <- as.integer(round(foldid))
  checkmate::assert_int(x = max(foldid), lower = 3L, upper = n)
  checkmate::assert_set_equal(x = unique(foldid), y = seq_len(max(foldid)))
  if (family == "binomial") {
    rest0 <- sum(!y) - tapply(X = y, INDEX = foldid, FUN = function(x) sum(!x))
    if (any(rest0 < 2L)) {
      stop("Each fold must leave at least ",
           "two observations from class 0 for the other folds.")
    }
    rest1 <- sum(y) - tapply(X = y, INDEX = foldid, FUN = sum)
    if (any(rest1 < 2L)) {
      stop("Each fold must leave at least ",
           "two observations from class 1 for the other folds.")
    }
  } else if (family == "cox") {
    rest <- sum(y[, "status"]) - tapply(X = y, INDEX = foldid,
                                        FUN = function(x) sum(x[, "status"]))
    if (any(rest < 2L)) {
      stop("Each fold must leave at least ",
           "two uncensored observations for the other folds.")
    }
  } else {
    rest <- n - tabulate(foldid)
    if (any(rest < 2L)) {
      stop("Each fold must leave at least two observations ",
           "for the other folds.")
    }
  }
  foldid
}
