
## function ".validate" --------------------------------------------------------

testthat::test_that("function '.validate' rejects wrong family", {
  n <- 10L
  for (family_data in c("gaussian", "binomial", "poisson", "cox")) {
    set.seed(1)
    data <- simulate(family = family_data, n0 = n, n1 = n, n_group = 3,
                     size_group = c(3, 2))
    for (family_model in c("gaussian", "binomial", "poisson", "cox")) {
      expect_error <-
        identical(family_data, "cox") ||
        identical(family_model, "cox")  ||
        identical(family_model, "binomial") ||
        (identical(family_data, "gaussian") &&
         identical(family_model, "poisson"))
      expect_warning <-
        identical(family_data, "poisson") ||
        (identical(family_data, "binomial") &&
         identical(family_model, "gaussian")) ||
        (identical(family_data, "binomial") &&
         identical(family_model, "poisson"))
      if (family_data == family_model) {
        next
      } else if (expect_error) {
        testthat::expect_error(
          cv.corila(x = data$x_train, y = data$y_train, group = data$group,
                    family = family_model)
        )
      } else if (expect_warning) {
        testthat::expect_warning(
          cv.corila(x = data$x_train, y = data$y_train, group = data$group,
                    family = family_model)
        )
      } else {
        stop("Implement missing tests!")
      }
    }
  }
})

## function ".estim_initial_coefs" ---------------------------------------------

testthat::test_that("initial coefficients are estimated", {
  family <- c("gaussian", "binomial", "poisson", "cox")
  alpha <- list(0, 0.5, 1, "pearson", "spearman", "kendall", "multiridge", NA)
  n <- 20
  p <- 10
  x <- matrix(rnorm(n * p), nrow = n, ncol = p)
  group <- rep(1:4, times = c(3, 3, 2, 2))
  beta <- rbinom(n = p, size = 1, prob = 0.5) * rnorm(p)
  for (i in seq_along(family)) {
    for (j in seq_along(alpha)) {
      if (identical(family[i], "poisson") &
            identical(alpha[[j]][1], "multiridge")) {
        next
      }
      if (identical(family[i], "cox")
          & alpha[[j]][1] %in% c("pearson", "spearman", "kendall")) {
        next
      }
      y <- .simulate_outcome(family = family[i], x = x, beta = beta)
      init <- list()
      for (k in 1:2) {
        if (k == 1) {
          lambda <- NULL
        } else {
          lambda <- init[[1]]$lambda
        }
        init[[k]] <- .estim_initial_coefs(
          x = x,
          y = y,
          family = family[i],
          alpha_init = alpha[[j]][1],
          group = group,
          foldid = NULL,
          nfolds = 10,
          lambda = lambda
        )
      }
      testthat::expect_identical(object = init[[1]], expected = init[[2]])
      testthat::expect_length(object = init[[1]]$coef, n = p)
      if (identical(alpha[[j]][1], "multiridge")) {
        length <- length(unique(group))
      } else if (is.character(alpha[[j]][1]) | is.na(alpha[[j]][1])) {
        length <- 0
      } else {
        length <- 1
      }
      testthat::expect_length(object = init[[1]]$lambda, n = length)
    }
  }
  alpha <- list(-1, Inf, "A")
  for (i in seq_along(alpha)) {
    testthat::expect_error(
      .estim_initial_coefs(
        x = x,
        y = stats::rnorm(n),
        family = "gaussian",
        alpha_init = alpha[[i]],
        group = group,
        foldid = NULL,
        nfolds = 10,
        lambda = lambda
      )
    )
  }
})


## function ".is_adjacent" -----------------------------------------------------

testthat::test_that("adjacency is detected", {
  p <- 5
  names <- paste0("x", seq_len(p))
  group <- list()
  group$index_vector <- setNames(object = c(1, 1, 2, 2, 3), nm = names)
  group$label_vector <- setNames(object = LETTERS[group$index_vector],
                                 nm = names(group$index_vector))
  group$index_list <- lapply(X = setNames(nm = unique(group$label_vector)),
                             FUN = function(x) which(group$label_vector == x))
  group$label_list <- lapply(group$index_list, names)
  group$matrix <- 1 * outer(X = group$index_vector,
                            Y = group$index_vector,
                            FUN = "==")
  p <- length(group$index_vector)
  cond <- list()
  for (i in seq_along(group)) {
    cond[[i]] <- .is_adjacent(group = group[[i]],
                              j = 1,
                              p = p,
                              names = names(group$index_vector))
  }
  lapply(X = cond[-1],
         FUN = testthat::expect_equal,
         expected = cond[[1]],
         check.attributes = FALSE)
  factor_vector <- as.factor(group$label_vector)
  testthat::expect_error(
    .is_adjacent(group = factor_vector, j = 1, p = p, names = NULL)
  )
  factor_list <- lapply(group$label_list, as.factor)
  testthat::expect_error(
    .is_adjacent(group = factor_list, j = 1, p = p, names = NULL)
  )
})

## function ".combine_slopes" --------------------------------------------------

set.seed(1)
p <- 10
alpha <- stats::rnorm(1)
temp <- stats::rnorm(p)
beta <- pmax(c(temp, -temp), 0)
coef <- .combine_slopes(alpha = alpha, beta = beta)
testthat::test_that("coefficients are in finite p + 1 vector", {
  testthat::expect_type(object = coef, type = "double")
  testthat::expect_length(object = coef, n = p + 1)
  testthat::expect_true(all(is.finite(coef)))
})
testthat::test_that("intercept does not change", {
  testthat::expect_identical(object = coef[1], expected = alpha)
})
testthat::test_that("slopes do not change", {
  testthat::expect_identical(object = coef[-1], expected = temp)
})

## function ".expand_auxiliary" ------------------------------------------------

n <- 5L
p <- 10L
set.seed(1)
x <- matrix(data = stats::rnorm(n * p), nrow = n, ncol = p)
primary <- as.logical(stats::rbinom(n = p, size = 1, prob = 0.5))
x_primary <- x[, primary]
testthat::test_that("incompatible dimensions are rejected", {
  testthat::expect_error(
    .expand_auxiliary(x = x_primary, primary = primary[-1])
  )
  testthat::expect_error(
    .expand_auxiliary(x = x_primary, primary = c(primary, TRUE))
  )
})
testthat::test_that("nothing happens if there are primary features only", {
  x_expanded <- .expand_auxiliary(x = x, primary = rep(TRUE, times = p))
  testthat::expect_identical(object = x_expanded, expected = x)
})
x_expanded <- .expand_auxiliary(x = x_primary, primary = primary)
testthat::test_that("primary predictors are equal", {
  testthat::expect_identical(object = x_expanded[, primary],
                             expected = x[, primary])
})
testthat::test_that("auxiliary features are zero", {
  testthat::expect_setequal(object = x_expanded[, !primary], expected = 0)
})
testthat::test_that("expanded features are in a finite n x p matrix", {
  testthat::expect_type(object = x_expanded, type = "double")
  testthat::expect_length(object = x_expanded, n = n * p)
  testthat::expect_identical(object = nrow(x_expanded), expected = n)
  testthat::expect_identical(object = ncol(x_expanded), expected = p)
  testthat::expect_true(all(is.finite(x_expanded)))
})

## function ".set_candidates" --------------------------------------------------

for (tune in c("none", "weight", "exponent", "bivariate", "factorial")) {
  hyper <- .set_candidates(tune = tune)
  testthat::test_that("candidate values", {
    labels <- c("wgt_local", "exp_local", "wgt_global", "exp_global")
    testthat::expect_type(object = hyper, type = "list")
    testthat::expect_named(object = hyper, expected = labels)
    testthat::expect_gte(object = min(hyper), expected = 0)
    testthat::expect_identical(object = hyper, expected = unique(hyper))
    testthat::expect_identical(object = rownames(hyper),
                               expected = as.character(seq_len(nrow(hyper))))
    testthat::expect_error(.set_candidates(tune = "random"))
  })
}
