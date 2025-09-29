
#' @name corila-package
#' @keywords documentation
#' @docType package
#' 
#' @aliases corila-package
#' 
#' @title
#' Sparse group lasso
#' 
#' @description
#' The R package `corila` implements the sparse group lasso.
#' 
#' @details
#' Use function [cv.corila()] for model fitting.
#' Type `library(corila)` and then `?cv.corila` or
#' `help("cv.corila")` to open its help file.
#' 
#' See the vignette for further examples.
#' Type `vignette("corila")` or `browseVignettes("corila")`
#' to open the vignette.
#' 
#' @seealso
#' First use \code{\link{cv.corila}} to fit the models,
#' and then \code{\link[=coef.cv.corila]{coef}} to extract coefficients
#' or \code{\link[=predict.cv.corila]{predict}} to make predictions.
#' 
#' @references
#' \href{https://orcid.org/0000-0001-6498-4801}{Armin Rauschenberger},
#' (2025).
#' "A flexible version of the sparse group lasso".
#' \emph{In preparation}.
#' 
#' @examples
#' ?cv.corila
#' ?coef.cv.corila
#' ?predict.cv.corila
#' 
"_PACKAGE"
