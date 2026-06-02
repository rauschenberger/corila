
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
#' corila:::calc_sign_prec(truth = truth, estim = estim) # observed value
#' corila:::calc_sign_prec(truth = truth, estim = -truth) # lower limit 0
#' corila:::calc_sign_prec(truth = truth, estim = truth) # upper limit 1
#' corila:::calc_sign_prec(truth = truth, estim = 0 * estim) # not defined
#'
#' @keywords internal
#'
calc_sign_prec <- function(truth, estim) {
  .assert(x = truth, type = "integer", dim = Inf, na.rm = TRUE)
  .assert(x = estim, type = "integer", dim = length(truth), na.rm = TRUE)
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
#' data <- corila:::simulate()
#' dims <- function(x) {
#'    if (is.matrix(x)||is.data.frame(x)) {
#'      paste(base::dim(x), collapse = " x ")
#'    } else {
#'      paste0(base::length(x))
#'    }
#' }
#' sapply(X = data, FUN = dims)
#'
#' @keywords internal
#'
simulate <- function(family = "gaussian", n0 = 100, n1 = 10000, n_group = 20,
                     n_type = 2, size_group = c(5, 3), effect_size = c(1, 1),
                     corfac_feature = 0.5, corfac_type = 0.5,
                     corfac_group = 0.25, n_group_causal = 2,
                     prop_causal = 0.5, noise_factor = 1,
                     plot = FALSE, trial = FALSE) {
  # --- check arguments ---
  .assert(x = family, type = "nominal",
          support = c("gaussian", "binomial", "poisson", "cox"))
  .assert(x = n0, type = "integer", min = 2)
  .assert(x = n1, type = "integer", min = 2)
  .assert(x = n_group, type = "integer", min = 2)
  .assert(x = n_type, type = "integer", min = 2)
  .assert(x = size_group, type = "integer", dim = n_type, min = 1)
  .assert(x = effect_size, type = "integer", dim = n_type, min = 0)
  .assert(x = corfac_feature, type = "numeric", min = 0, max = 1)
  .assert(x = corfac_type, type = "numeric", min = 0, max = 1)
  .assert(x = corfac_group, type = "numeric", min = 0, max = 1)
  .assert(x = n_group_causal, type = "integer", min = 0, max = n_group)
  .assert(x = prop_causal, type = "numeric", min = 0, max = 1)
  .assert(x = noise_factor, type = "numeric", min = 0)
  .assert(x = plot, type = "logical")
  .assert(x = trial, type = "logical")
  # family = "gaussian";n0 = 100;n1 = 10000;n_group = 20;n_type = 2;
  # size_group = c(5, 3);effect_size = c(1, 1);corfac_feature = 0.5;
  # corfac_type = 0.5;corfac_group = 0.25;n_group_causal = 2;
  # prop_causal = 0.5; noise_factor = 1; plot = TRUE
  n <- n0 + n1
  #if (n_type != length(size_group)) {
  #  stop("Wrong length.")
  #}
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
