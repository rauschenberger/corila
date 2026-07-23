
## function ".validate" --------------------------------------------------------

#' @srrstats {G5.2} *error and warning behaviour is tested*
#' @srrstats {G5.2b} *error messages are tested*
#' @srrstats {G5.8} *edge condition tests*


n <- 10L
for (family_data in c("gaussian", "binomial", "poisson", "cox")) {
  set.seed(1L)
  data <- simulate_data(family = family_data, n0 = n, n1 = n, q = 3L, p = 5L)
  p <- ncol(data$x_train)
  testthat::test_that("function '.validate' rejects wrong family", {
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
        #' @srrstats {G5.8b} *cv.corila rejects unsupported response types*
        testthat::expect_error(
          object = cv.corila(x = data$x_train, y = data$y_train,
                             group = data$group, family = family_model),
          regexp = "" #
        )
      } else if (expect_warning) {
        testthat::expect_warning(
          object = cv.corila(x = data$x_train, y = data$y_train,
                             group = data$group, family = family_model),
          regexp = "" # Implement warnings for possibly wrong family?
        )
      } else {
        stop("Implement missing tests!")
      }
    }
  })
  testthat::test_that(".validate rejects wrong group object", {
    group <- list(
      A = data$group > 5.0,
      B = array(data = 2L * outer(data$group, data$group, "=="),
                dim = c(p, p, 1L)),
      C = lapply(X = unique(data$group),
                 FUN = function(x) as.factor(which(data$group == x)))
    )
    for (k in seq_along(group)) {
      testthat::expect_error(
        cv.corila(x = data$x_train, y = data$y_train, group = group[[k]],
                  family = family_data)
      )
    }
  })
  testthat::test_that(".validate rejects wrong alpha_init", {
    testthat::expect_error(
      cv.corila(x = data$x_train, y = data$y_train, group = data$group,
                family = family_data, alpha_init = "elastic-net")
    )
  })
  #' @srrstats {G5.8a} *cv.corila rejects zero-length data*
  testthat::test_that("cv.corila rejects data with < 3 observations", {
    testthat::expect_error(
      object = cv.corila(x = data$x_train[0L, ], y = data$y_train[0L],
                         group = data$group, family = family_data),
      regexp = "at least three observations"
    )
    testthat::expect_error(
      object = cv.corila(x = data$x_train[1L, , drop = FALSE],
                         y = data$y_train[1L],
                         group = data$group,
                         family = family_data),
      regexp = "at least three observations"
    )
    testthat::expect_error(
      object = cv.corila(x = data$x_train[1L:2L, ],
                         y = data$y_train[1L:2L],
                         group = data$group,
                         family = family_data),
      regexp = "at least three observations"
    )
  })
  #' @srrstats {G5.8d} *rejects data outside the scope of the algorithm*
  testthat::test_that("cv.corila rejects data with < 2 predictors", {
    testthat::expect_error(
      object = cv.corila(x = data$x_train[, 0L, drop = FALSE],
                         y = data$y_train,
                         group = integer(),
                         family = family_data),
      regexp = "at least two predictors"
    )
    testthat::expect_error(
      object = cv.corila(x = data$x_train[, 1L, drop = FALSE],
                         y = data$y_train,
                         group = 1L,
                         family = family_data),
      regexp = "at least two predictors"
    )
  })
  #' @srrstats {G5.8c} *rejects data without complete observations*
  testthat::test_that("cv.corila rejects NA response", {
    data$x_train[, 1L] <- NA
    testthat::expect_error(
      object = cv.corila(x = data$x_train,
                         y = data$y_train,
                         group = data$group,
                         family = family_data,
                         na_action = "complete_cases"),
      regexp = "at least three complete observations"
    )
  })
}

## function ".estim_initial_coefs" ---------------------------------------------

set.seed(1L)
testthat::test_that("initial coefficients are estimated", {
  family <- c("gaussian", "binomial", "poisson", "cox")
  alpha <- list(0.0, 0.5, 1.0,
                "pearson", "spearman", "kendall", "multiridge", NA)
  n <- 20L
  p <- 10L
  x <- matrix(rnorm(n * p), nrow = n, ncol = p)
  group <- rep(1L:4L, times = c(3L, 3L, 2L, 2L))
  beta <- rbinom(n = p, size = 1L, prob = 0.5) * rnorm(p)
  for (i in seq_along(family)) {
    for (j in seq_along(alpha)) {
      if (identical(family[i], "poisson") &
            identical(alpha[[j]][1L], "multiridge")) {
        next
      }
      if (identical(family[i], "cox")
          & alpha[[j]][1L] %in% c("pearson", "spearman", "kendall")) {
        next
      }
      y <- .simulate_response(family = family[i], x = x, beta = beta)
      init <- list()
      for (k in 1L:2L) {
        if (k == 1L) {
          lambda <- NULL
        } else {
          lambda <- init[[1L]]$lambda
        }
        init[[k]] <- .estim_initial_coefs(
          x = x,
          y = y,
          family = family[i],
          alpha_init = alpha[[j]][1L],
          group = group,
          foldid = NULL,
          nfolds = 10L,
          lambda = lambda
        )
      }
      testthat::expect_identical(object = init[[1L]], expected = init[[2L]])
      testthat::expect_length(object = init[[1L]]$coef, n = p)
      if (identical(alpha[[j]][1L], "multiridge")) {
        length <- length(unique(group))
      } else if (is.character(alpha[[j]][1L]) | is.na(alpha[[j]][1L])) {
        length <- 0L
      } else {
        length <- 1L
      }
      testthat::expect_length(object = init[[1L]]$lambda, n = length)
    }
  }
  alpha <- list(-1.0, Inf, "A")
  for (i in seq_along(alpha)) {
    testthat::expect_error(
      .estim_initial_coefs(
        x = x,
        y = stats::rnorm(n),
        family = "gaussian",
        alpha_init = alpha[[i]],
        group = group,
        foldid = NULL,
        nfolds = 10L,
        lambda = lambda
      )
    )
  }
})


## function ".is_adjacent" -----------------------------------------------------

testthat::test_that("adjacency is detected", {
  p <- 5L
  names <- paste0("x", seq_len(p))
  group <- list()
  group$index_vector <- setNames(object = c(1L, 1L, 2L, 2L, 3L), nm = names)
  group$label_vector <- setNames(object = LETTERS[group$index_vector],
                                 nm = names(group$index_vector))
  group$index_list <- lapply(X = setNames(nm = unique(group$label_vector)),
                             FUN = function(x) which(group$label_vector == x))
  group$label_list <- lapply(group$index_list, names)
  group$matrix <- 1L * outer(X = group$index_vector,
                             Y = group$index_vector,
                             FUN = "==")
  p <- length(group$index_vector)
  cond <- list()
  for (i in seq_along(group)) {
    cond[[i]] <- .is_adjacent(group = group[[i]],
                              j = 1L,
                              p = p,
                              names = names(group$index_vector))
  }
  lapply(X = cond[-1L],
         FUN = testthat::expect_equal,
         expected = cond[[1L]],
         check.attributes = FALSE)
  #factor_vector <- as.factor(group$label_vector)
  #testthat::expect_error(
  #  .is_adjacent(group = factor_vector, j = 1L, p = p, names = NULL)
  #)
  factor_list <- lapply(group$label_list, as.factor)
  testthat::expect_error(
    object = .is_adjacent(group = factor_list, j = 1L, p = p, names = NULL),
    regexp = "slots of type numeric or character"
  )
  testthat::expect_error(
    object = .is_adjacent(group = list(), j = 1L, p = p, names = NULL),
    regexp = "not have length 0"
  )
  testthat::expect_error(
    object = .is_adjacent(group = array(), j = 1L, p = p, names = NULL),
    regexp = "a vector, a list, or a matrix"
  )
})

## function ".combine_slopes" --------------------------------------------------

set.seed(1L)
p <- 10L
alpha <- stats::rnorm(1L)
temp <- stats::rnorm(p)
beta <- pmax(c(temp, -temp), 0.0)
coef <- .combine_slopes(alpha = alpha, beta = beta)
testthat::test_that("coefficients are in finite p + 1 vector", {
  testthat::expect_type(object = coef, type = "double")
  testthat::expect_length(object = coef, n = p + 1L)
  testthat::expect_true(all(is.finite(coef)))
})
testthat::test_that("intercept does not change", {
  testthat::expect_identical(object = coef[1L], expected = alpha)
})
testthat::test_that("slopes do not change", {
  testthat::expect_identical(object = coef[-1L], expected = temp)
})

## function ".expand_auxiliary" ------------------------------------------------

n <- 5L
p <- 10L
set.seed(1L)
x <- matrix(data = stats::rnorm(n * p), nrow = n, ncol = p)
primary <- as.logical(stats::rbinom(n = p, size = 1L, prob = 0.5))
x_primary <- x[, primary]
testthat::test_that("incompatible dimensions are rejected", {
  testthat::expect_error(
    .expand_auxiliary(x = x_primary, primary = primary[-1L])
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
  testthat::expect_setequal(object = x_expanded[, !primary], expected = 0.0)
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
    testthat::expect_gte(object = min(hyper), expected = 0.0)
    testthat::expect_identical(object = hyper, expected = unique(hyper))
    testthat::expect_identical(object = rownames(hyper),
                               expected = as.character(seq_len(nrow(hyper))))
    testthat::expect_error(.set_candidates(tune = "random"))
  })
}
