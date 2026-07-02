
#' @title
#' Example data
#'
#' @description
#' This is example data for modelling a response based on
#' grouped and correlated primary and auxiliary predictors.
#'
#' @format ## `data`
#' A list with multiple slots:
#' - `x_train`:
#'   predictor matrix of the training observations
#'   (\eqn{n_0} rows, \eqn{p} columns)
#' - `y_train`:
#'   response vector of the training observations
#'   (length \eqn{n_0})
#' - `group`:
#'   integer vector indicating the group of the predictors
#'   (length \eqn{p})
#' - `primary`:
#'   logical vector indicating
#'   primary (`TRUE`) and auxiliary (`FALSE`) predictors
#'   (length \eqn{p})
#' - `beta`:
#'    numeric vector of the effects of the predictors on the response
#'    (length \eqn{p})
#' - `x_test`:
#'   \eqn{n_1 \times p} predictor matrix for the test observations
#' - `y_test`:
#'   response vector for the test observations of length \eqn{n_1}
#'
#' @details
#' Use the objects `x_train`, `y_train`, `group`, and `primary`
#' for model training.
#' Estimated coefficients can be compared with `beta`.
#' Use the object `x_test` for model testing.
#' Predicted values can be compared with `y_test`.
#'
"data"
