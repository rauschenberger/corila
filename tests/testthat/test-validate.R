
#----- .validate_family --------------------------------------------------------

testthat::test_that("function .validate_family returns argument if valid", {
  x <- c("gaussian", "binomial", "poisson", "cox")
  for (i in seq_along(x)) {
    y <- .validate_family(family = x[i])
    testthat::expect_identical(object = y, expected = x[i])
  }
})

testthat::test_that("function .validate_family standardises arguments", {
  testthat::expect_identical(object = .validate_family(family = "linear"),
                             expected = "gaussian")
  testthat::expect_identical(object = .validate_family(family = "logistic"),
                             expected = "binomial")
})

testthat::test_that("function .validate_family errors if invalid argument", {
  testthat::expect_error(object = .validate_family(family = "gamma"),
                       regexp = "Must be element of set") 
})

#----- .validate_na_action -----------------------------------------------------

testthat::test_that("function .na_action returns argument if valid", {
  x <- c("error", "complete_cases")
  for (i in seq_along(x)) {
    y <- .validate_na_action(na_action = x[i])
    testthat::expect_identical(object = y, expected = x[i])
  }
  testthat::expect_error(object = .validate_na_action(na_action = "warning"),
                         regexp = "Must be element of set")
})

#----- .validate_y -------------------------------------------------------------

testthat::test_that("function .validate_y handles survival times", {
  n <- 10L
  time <- stats::rpois(n = n,lambda = 4L)
  event <- stats::rbinom(n = n, size = 1L, prob = 0.5)
  y <- survival::Surv(time = time, event = event)
  testthat::expect_error(
    object = .validate_y(y = y, family = "gaussian", n = n, na_action = "error",
                         names = NULL),
    regexp = "if and only if Cox model"
  )
  testthat::expect_error(
    object = .validate_y(y = stats::rnorm(n = n), family = "cox",
                         n = n, na_action = "error", names = NULL),
    regexp = "if and only if Cox model"
  )
})

#----- .validate_cor -----------------------------------------------------------

testthat::test_that("function .validate_cor returns valid character", {
  x <- c("pearson", "spearman", "kendall")
  for (i in seq_along(x)) {
    testthat::expect_identical(
      object = .validate_cor(cor = x[i], p = 10L, names = NULL),
      expected = x[i]
    )
  }
})

testthat::test_that("function .validate_cor errors for invalid character", {
  testthat::expect_error(
    object = .validate_cor(cor = "cramer", p = 10L, names = NULL),
    regexp = "Must be element of set" 
  )
})

n <- 10L
p <- 5L
x <- matrix(data = stats::rnorm(n = n * p), nrow = n, ncol = p)
cor <- stats::cor(x)

testthat::test_that("function .validate_cor expects character or matrix", {
  testthat::expect_identical(
    object = .validate_cor(cor = cor, p = p, names = NULL),
    expected = cor
  )
})

#----- function .validate_foldid -----------------------------------------------

testthat::test_that("function .validate_foldid complains if needed", {
  family <- "binomial"
  foldid <- rep(x = c(1L, 2L, 3L), each = 2L)
  y <- c(1L, 1L, 1L, 0L, 1L, 0L)
  testthat::expect_error(
    object = .validate_foldid(foldid = foldid, y = y, family = family),
    regexp = "at least two observations from class 0"
  )
  y <- c(0L, 0L, 0L, 1L, 0L, 1L)
  testthat::expect_error(
    object = .validate_foldid(foldid = foldid, y = y, family = family),
    regexp = "at least two observations from class 1"
  )
  family <- "cox"
  time <- stats::rpois(n = 6L, lambda = 4L)
  event <- c(0L, 0L, 0L, 1L, 0L, 1L)
  y <- survival::Surv(time = time, event = event)
  testthat::expect_error(
    object = .validate_foldid(foldid = foldid, y = y, family = family),
    regexp = "at least two uncensored observations"
  )
})

#----- function .validate_alpha ------------------------------------------------

testthat::test_that("function .validate_data", {
  testthat::expect_identical(
    object = .validate_alpha(alpha = "ridge", init = FALSE),
    expected = 0
  )
  testthat::expect_identical(
    object = .validate_alpha(alpha = "lasso", init = FALSE),
    expected = 1
  )
  testthat::expect_error(
    object = .validate_alpha(alpha = TRUE, init = FALSE),
    regexp = "either a single character or a single numeric"
  )
})

".validate_primary"
".validate_alpha"
".validate_cor"
".validate_foldid"
".validate_group"
".validate_hyper"
".validate_x"
".validate_y"
".validate_y_hat"
