
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
#' The R package `corila` implements
#' "Sparse modelling with grouped and correlated features
#' allowing for privileged information" (*Rauschenberger, 2026*).
#' This is the first implementation of a novel algorithm,
#' which is based on adaptive lasso regression using the
#' [glmnet-package][glmnet::glmnet-package].
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
#' This package also includes the wrapper function [multiridge()]
#' for multi-penalty ridge regression with
#' the [multiridge-package][multiridge::multiridge-package].
#'
#' @seealso
#' First use [cv.corila()] to fit the model,
#' and then [coef()][coef.cv.corila] to extract coefficients
#' or [predict()][predict.cv.corila] to make predictions.
#'
#' @author
#' [Armin Rauschenberger](https://orcid.org/0000-0001-6498-4801)
#'
#' @references
#' [Armin Rauschenberger](https://orcid.org/0000-0001-6498-4801)
#' (2026).
#' "Sparse modelling with grouped and correlated features
#' allowing for privileged information".
#' *In preparation*.
#'
#' @examples
#' ?cv.corila
#' ?coef.cv.corila
#' ?predict.cv.corila
#'
#' @srrstats {G1.0} *@references mentions the primary reference*
#' @srrstats {G1.1} *@description specifies novelty*
#'
"_PACKAGE"
