
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
#' The R package \code{corila} implements
#' "Sparse modelling with grouped and correlated features
#' allowing for privileged information" (\emph{Rauschenberger, 2026}).
#' This is the first implementation of a novel algorithm,
#' which is based on adaptive lasso regression using the
#' \code{\link[glmnet]{glmnet-package}}.
#'
#' @details
#' Use function \code{\link[=cv.corila]{cv.corila}()} for model fitting.
#' Type \code{library(corila)} and then \code{?cv.corila} or
#' \code{help("cv.corila")} to open its help file.
#'
#' See the vignette for further examples.
#' Type \code{vignette("corila")} or \code{browseVignettes("corila")}
#' to open the vignette.
#'
#' This package also includes the wrapper function
#' \code{\link[=multiridge]{multiridge}()}
#' for multi-penalty ridge regression with
#' the \code{\link[multiridge]{multiridge-package}}.
#'
#' @seealso
#' First use \code{\link[=cv.corila]{cv.corila}()} to fit the model,
#' and then \code{\link[=coef.cv.corila]{coef}()} to extract coefficients
#' or \code{\link[=predict.cv.corila]{predict}()} to make predictions.
#'
#' @author \href{https://orcid.org/0000-0001-6498-4801}{Armin Rauschenberger}
#'
#' @references
#' \href{https://orcid.org/0000-0001-6498-4801}{Armin Rauschenberger}
#' (2026).
#' "Sparse modelling with grouped and correlated features
#' allowing for privileged information".
#' \emph{In preparation}.
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
