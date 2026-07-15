#' srr_stats
#'
#' @srrstatsVerbose TRUE
#'
#' @srrstats {G1.2} *README.md contains a life cycle badge* 
#' @srrstats {G1.3} *statistical terminology is defined (see files in folder R)* 
#' @srrstats {G1.4} *roxygen2 is used (see DESCRIPTION and files in folder R)*
#' @srrstats {G1.4a} *internal functions are documented and have the keyword internal*
#' @srrstats {RE3.0} *The function glmnet from the glmnet-package issues warnings if the model fails to converge.*
#' @srrstats {RE3.2} *See glmnet-package for convergence thresholds*
#' @srrstats {RE4.7} *convergence statistics are in the returned glmnet object* 
#' 
#' @noRd
NULL

#' NA_standards
#'
#' @srrstatsNA {G2.4d} *method does not use factors*
#' @srrstatsNA {G2.4e} *idem*
#' @srrstatsNA {G2.5} *no function has a factor argument*
#' @srrstatsNA {G2.7} *package does not accept standard tabular forms*
#' @srrstatsNA {G2.8} *function .assert imposes matrix*
#' @srrstatsNA {G2.10} *package does not accept data frames*
#' @srrstatsNA {G2.11} *idem* 
#' @srrstatsNA {G2.12} *idem* 
#' @srrstatsNA {G2.14c} *Missing data are not replaced by imputed values, because the type of imputation can have a major impact on the model.*
#' @srrstatsNA {G2.9} *package only accepts vectors and matrices (no data frames)* 
#' @srrstatsNA {G3.1} *software does not rely on covariance calculation*
#' @srrstatsNA {G3.1a} *covariance methods cannot arbitrarily be specified*
#' @srrstatsNA {G4.0} *package does not enable outputs to be written to local files*
#' @srrstatsNA {G5.0} *tests use simulated data, standard data sets with known properties are not available for the use case (high-dimensional grouped and correlated features)*
#' @srrstatsNA {G5.4b} *This is not a new implementation of an existing method.*
#' @srrstatsNA {G5.4c} *stored values are not drawn from published paper outputs*
#' @srrstatsNA {G5.10} *no unit tests are in the extended tests category* 
#' @srrstatsNA {G5.12} *see previous point*
#' @srrstatsNA {G5.11} *tests only requires simulated datasets or small provided dataset*
#' @srrstatsNA {G5.11a} *see previous point*
#' @srrstatsNA {RE1.0} *As this is a regression method for high-dimensional data, using the formula interface is not practical*
#' @srrstatsNA {RE1.1} *idem*
#' @srrstatsNA {RE1.3a} *relevant information is transferred*
#' @srrstatsNA {RE2.0} *input data are not transformed*
#' @srrstatsNA {RE2.4} *high-dimensional data are always perfectly collinear*
#' @srrstatsNA {RE2.4a} *idem*
#' @srrstatsNA {RE2.4b} *idem*
#' @srrstatsNA {RE3.3} *convergence thresholds are set internally by the glmnet-package* 
#' @srrstatsNA {RE4.1} *As this package extends the glmnet-package, it cannot generate a model object without fitting the model.*
#' @srrstatsNA {RE4.3} *confidence intervals are not available for penalised regression*
#' @srrstatsNA {RE4.4} *formula is not useful for high-dimensional settings*
#' @srrstatsNA {RE4.6} *variance-covariance matrix of coefficients is not available for this high-dimensional approach*
#' @srrstatsNA {RE4.13} *predictors cannot be extracted to save memory*
#' @srrstatsNA {RE4.14} *no closed-form prediction intervals are available for penalised regression*
#' @srrstatsNA {RE4.15} *this method is not about forecasting for multiple time-points* 
#' @srrstatsNA {RE4.16} *method models same responses for all groups*
#' @srrstatsNA {RE5.0} *(adaptive) lasso/ridge regression is handled by the glmnet package*
#' @srrstatsNA {RE6.1} *default plot method is generic*
#' @srrstatsNA {RE6.3} *As this package is not about time series, there is no difference between interpolation and extrapolation.*
#' @srrstatsNA {RE7.0} *not meaningful in penalised high-dimensional settings*
#' @srrstatsNA {RE7.0a} *idem*
#' @srrstatsNA {RE7.1} *idem*
#' @srrstatsNA {RE7.1a} *idem*
#' @srrstatsNA {RE7.4} *method is not about forecasting*
#' @noRd
NULL
