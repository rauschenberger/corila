
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
#' - `x_test`:
#'   \eqn{n_1 \times p} predictor matrix for the test observations
#' - `y_test`:
#'   response vector for the test observations of length \eqn{n_1}
#'
"data"
