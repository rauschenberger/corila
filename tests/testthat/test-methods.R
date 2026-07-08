
## S3 methods for class "cv.corila" --------------------------------------------

#' @srrstats {RE7.3} test S3 methods

n <- 10L
for (family in c("gaussian", "binomial", "poisson", "cox")) {
  set.seed(1L)
  data <- simulate(family = family, n0 = n, n1 = n, n_group = 3L,
                   size_group = c(3L, 2L))
  p <- data$info$p
  object <- cv.corila(x = data$x_train, y = data$y_train, group = data$group,
                      family = family)
  testthat::test_that("function 'cv.corila' returns a list", {
    testthat::expect_type(object = object, type = "list")
    names <- c("model", "lambda_init", "scale", "args", "hyper",
               "id_hyper", "lambda.min", "y_obs", "y_fit")
    testthat::expect_length(object = object, n = length(names))
    testthat::expect_named(object = object, expected = names)
  })
  testthat::test_that("function 'coef.cv.corila' returns finite p-vector", {
    beta_hat <- coef(object, s = "lambda.min")
    testthat::expect_type(object = beta_hat, type = "double")
    testthat::expect_length(object = beta_hat, n = p + (family != "cox"))
    testthat::expect_true(all(is.finite(beta_hat)))
    testthat::expect_error(object = coef(object, s = "lambda.1se"))
    testthat::expect_error(object = coef(object, s = -1))
  })
  testthat::test_that("function 'predict.cv.corila' returns finite n-vector", {
    y_hat <- predict(object, newx = data$x_train, s = "lambda.min")
    testthat::expect_type(object = y_hat, type = "double")
    testthat::expect_length(object = y_hat, n = n)
    testthat::expect_true(all(is.finite(y_hat)))
    testthat::expect_error(predict(object, newx = data$x_train[, -1L],
                                   s = "lambda.min"))
    testthat::expect_error(predict(object, newx = data$x_train,
                                   s = "lambda.1se"))
    testthat::expect_error(predict(object, newx = data$x_train, s = -1))
  })
  testthat::test_that("function 'fitted.cv.corila' returns finite n-vector", {
    y_hat <- fitted(object)
    testthat::expect_type(object = y_hat, type = "double")
    testthat::expect_length(object = y_hat, n = n)
    testthat::expect_true(all(is.finite(y_hat)))
    y_pred <- predict(object, newx = data$x_train, s = "lambda.min")
    testthat::expect_equal(object = y_hat, expected = y_pred)
  })
  testthat::test_that(
    desc = "function 'residuals.cv.corila' returns finite n-vector",
    code = {
      testthat::skip_if(family == "cox")
      resid <- residuals(object)
      testthat::expect_type(object = resid, type = "double")
      testthat::expect_length(object = resid, n = n)
      testthat::expect_true(all(is.finite(resid)))
    }
  )
  testthat::test_that("observed minus fitted values equal residuals", {
    testthat::skip_if_not(family == "gaussian")
    y_hat <- fitted(object)
    resid <- residuals(object)
    testthat::expect_equal(object = data$y_train - y_hat, expected = resid)
  })
  testthat::test_that("function 'nobs.cv.corila' returns the correct integer", {
    n_obs <- stats::nobs(object)
    testthat::expect_type(object = n_obs, type = "integer")
    testthat::expect_length(object = n_obs, n = 1L)
    testthat::expect_true(is.finite(n_obs))
    testthat::expect_gte(object = n_obs, expected = 1L)
    testthat::expect_identical(object = n_obs, expected = n)
  })
  testthat::test_that("function 'plot.cv.corila' returns NULL invisibly", {
    testthat::expect_invisible(call = plot(object))
    testthat::expect_null(object = plot(object))
  })
  testthat::test_that("function 'print.cv.corila' returns object invisibly", {
    testthat::expect_invisible(call = print(object))
    testthat::expect_equal(object = print(object), expected = object)
  })
  testthat::test_that("function 'print.cv.corila' prints a string", {
    string <- capture.output(print(object))
    testthat::expect_type(object = string, type = "character")
    testthat::expect_length(object = string, n = 3L)
    testthat::expect_true(object = any(grepl(pattern = "cv.corila",
                                             x = string)))
  })
  testthat::test_that(
    desc = "function 'summary.cv.corila' returns a list with named slots",
    code = {
      testthat::expect_type(object = summary(object), type = "list")
      names <- c("family", "n", "p", "p_primary", "p_auxiliary",
                 "alpha_init", "alpha_final", "lambda.min", "wgt_local",
                 "wgt_global", "exp_local", "exp_global", "nzero")
      testthat::expect_length(object = summary(object), n = length(names))
      testthat::expect_named(object = summary(object), expected = names)
    }
  )
  testthat::test_that(
    desc = "function 'print.summary.cv.corila' returns NULL invisibly",
    code = {
      testthat::expect_invisible(call = print(summary(object)))
      testthat::expect_null(object = print(summary(object)))
    }
  )
  testthat::test_that(
    desc = "function 'deviance.cv.corila' returns a non-negative scalar",
    code = {
      dev <- deviance(object)
      testthat::expect_type(object = dev, type = "double")
      testthat::expect_length(object = dev, n = 1L)
      testthat::expect_gte(object = dev, expected = 0)
    }
  )
}
