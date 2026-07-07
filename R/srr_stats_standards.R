#' srr_stats
#'
#' @srrstatsVerbose TRUE
#'
#' @srrstats {G1.2} *README.md contains a life cycle badge* 
#' @srrstats {G1.3} *statistical terminology is defined (see files in folder R)* 
#' @srrstats {G1.4} *roxygen2 is used (see DESCRIPTION and files in folder R)*
#' @srrstats {G1.4a} *internal functions are documented and have the keyword "internal" (see files in folder R)*
#' @srrstatsTODO {G2.6} *Software which accepts one-dimensional input should ensure values are appropriately pre-processed regardless of class structures.* 
#' @srrstatsTODO {G2.7} *Software should accept as input as many of the above standard tabular forms as possible, including extension to domain-specific forms.*
#' @srrstatsTODO {G2.8} *Software should provide appropriate conversion or dispatch routines as part of initial pre-processing to ensure that all other sub-functions of a package receive inputs of a single defined class or type.*
#' @srrstatsTODO {G2.9} *Software should issue diagnostic messages for type conversion in which information is lost (such as conversion of variables from factor to character; standardisation of variable names; or removal of meta-data such as those associated with [`sf`-format](https://r-spatial.github.io/sf/) data) or added (such as insertion of variable or column names where none were provided).* 
#' @srrstatsTODO {G2.10} *Software should ensure that extraction or filtering of single columns from tabular inputs should not presume any particular default behaviour, and should ensure all column-extraction operations behave consistently regardless of the class of tabular data used as input.* 
#' @srrstatsTODO {G2.11} *Software should ensure that `data.frame`-like tabular objects which have columns which do not themselves have standard class attributes (typically, `vector`) are appropriately processed, and do not error without reason. This behaviour should be tested. Again, columns created by the [`units` package](https://github.com/r-quantities/units/) provide a good test case.*
#' @srrstatsTODO {G2.12} *Software should ensure that `data.frame`-like tabular objects which have list columns should ensure that those columns are appropriately pre-processed either through being removed, converted to equivalent vector columns where appropriate, or some other appropriate treatment such as an informative error. This behaviour should be tested.* 
#' @srrstatsTODO {G4.0} *Statistical Software which enables outputs to be written to local files should parse parameters specifying file names to ensure appropriate file suffixes are automatically generated where not provided.*
#' @srrstatsTODO {G5.0} *Where applicable or practicable, tests should use standard data sets with known properties (for example, the [NIST Standard Reference Datasets](https://www.itl.nist.gov/div898/strd/), or data sets provided by other widely-used R packages).*
#' @srrstatsTODO {G5.1} *Data sets created within, and used to test, a package should be exported (or otherwise made generally available) so that users can confirm tests and run examples.* 
#' @srrstatsTODO {G5.2} *Appropriate error and warning behaviour of all functions should be explicitly demonstrated through tests. In particular,*
#' @srrstatsTODO {G5.2a} *Every message produced within R code by `stop()`, `warning()`, `message()`, or equivalent should be unique*
#' @srrstatsTODO {G5.2b} *Explicit tests should demonstrate conditions which trigger every one of those messages, and should compare the result with expected values.*
#' @srrstatsTODO {G5.4} **Correctness tests** *to test that statistical algorithms produce expected results to some fixed test data sets (potentially through comparisons using binding frameworks such as [RStata](https://github.com/lbraglia/RStata)).*
#' @srrstatsTODO {G5.4a} *For new methods, it can be difficult to separate out correctness of the method from the correctness of the implementation, as there may not be reference for comparison. In this case, testing may be implemented against simple, trivial cases or against multiple implementations such as an initial R implementation compared with results from a C/C++ implementation.*
#' @srrstatsTODO {G5.4b} *For new implementations of existing methods, correctness tests should include tests against previous implementations. Such testing may explicitly call those implementations in testing, preferably from fixed-versions of other software, or use stored outputs from those where that is not possible.*
#' @srrstatsTODO {G5.4c} *Where applicable, stored values may be drawn from published paper outputs when applicable and where code from original implementations is not available*
#' @srrstatsTODO {G5.6} **Parameter recovery tests** *to test that the implementation produce expected results given data with known properties. For instance, a linear regression algorithm should return expected coefficient values for a simulated data set generated from a linear model.*
#' @srrstatsTODO {G5.6a} *Parameter recovery tests should generally be expected to succeed within a defined tolerance rather than recovering exact values.*
#' @srrstatsTODO {G5.6b} *Parameter recovery tests should be run with multiple random seeds when either data simulation or the algorithm contains a random component. (When long-running, such tests may be part of an extended, rather than regular, test suite; see G5.10-4.12, below).* 
#' @srrstatsTODO {G5.7} **Algorithm performance tests** *to test that implementation performs as expected as properties of data change. For instance, a test may show that parameters approach correct estimates within tolerance as data size increases, or that convergence times decrease for higher convergence thresholds.*

#' @srrstatsTODO {G5.8} **Edge condition tests** *to test that these conditions produce expected behaviour such as clear warnings or errors when confronted with data with extreme properties including but not limited to:*
#' @srrstatsTODO {G5.8a} *Zero-length data*
#' @srrstatsTODO {G5.8b} *Data of unsupported types (e.g., character or complex numbers in for functions designed only for numeric data)*
#' @srrstatsTODO {G5.8c} *Data with all-`NA` fields or columns or all identical fields or columns*
#' @srrstatsTODO {G5.8d} *Data outside the scope of the algorithm (for example, data with more fields (columns) than observations (rows) for some regression algorithms)*

#' @srrstatsTODO {G5.10} *Extended tests should included and run under a common framework with other tests but be switched on by flags such as as a `<MYPKG>_EXTENDED_TESTS="true"` environment variable.* - The extended tests can be then run automatically by GitHub Actions for example by adding the following to the `env` section of the workflow: 
#' @srrstatsTODO {G5.11} *Where extended tests require large data sets or other assets, these should be provided for downloading and fetched as part of the testing workflow.*
#' @srrstatsTODO {G5.11a} *When any downloads of additional data necessary for extended tests fail, the tests themselves should not fail, rather be skipped and implicitly succeed with an appropriate diagnostic message.*
#' @srrstatsTODO {G5.12} *Any conditions necessary to run extended tests such as platform requirements, memory, expected runtime, and artefacts produced that may need manual inspection, should be described in developer documentation such as a `CONTRIBUTING.md` or `tests/README.md` file.*

#' @srrstatsTODO {RE1.2} *Regression Software should document expected format (types or classes) for inputting predictor variables, including descriptions of types or classes which are not accepted.* 
#' @srrstatsTODO {RE1.3} *Regression Software which passes or otherwise transforms aspects of input data onto output structures should ensure that those output structures retain all relevant aspects of input data, notably including row and column names, and potentially information from other `attributes()`.*
#' @srrstatsTODO {RE1.3a} *Where otherwise relevant information is not transferred, this should be explicitly documented.* 
#' @srrstatsTODO {RE1.4} *Regression Software should document any assumptions made with regard to input data; for example distributional assumptions, or assumptions that predictor data have mean values of zero. Implications of violations of these assumptions should be both documented and tested.* 
#' @srrstatsTODO {RE2.0} *Regression Software should document any transformations applied to input data, for example conversion of label-values to `factor`, and should provide ways to explicitly avoid any default transformations (with error or warning conditions where appropriate).*
#' @srrstatsTODO {RE2.1} *Regression Software should implement explicit parameters controlling the processing of missing values, ideally distinguishing `NA` or `NaN` values from `Inf` values (for example, through use of `na.omit()` and related functions from the `stats` package).* 
#' @srrstatsTODO {RE2.2} *Regression Software should provide different options for processing missing values in predictor and response data. For example, it should be possible to fit a model with no missing predictor data in order to generate values for all associated response points, even where submitted response values may be missing.*
#' @srrstats {RE3.0} *The function glmnet from the glmnet-package issues warnings if the model fails to converge.*
#' @srrstats {RE3.2} *See glmnet-package for convergence thresholds*
#' @srrstats {RE4.7} *convergence statistics are in the returned glmnet object* 
#' @srrstatsTODO {RE4.11} *Goodness-of-fit and other statistics associated such as effect sizes with model coefficients.*
#' @srrstatsTODO {RE5.0} *Scaling relationships between sizes of input data (numbers of observations, with potential extension to numbers of variables/columns) and speed of algorithm.* 
#' @srrstatsTODO {RE7.0} *Tests with noiseless, exact relationships between predictor (independent) data.*
#' @srrstatsTODO {RE7.0a} In particular, these tests should confirm ability to reject perfectly noiseless input data.
#' @srrstatsTODO {RE7.1} *Tests with noiseless, exact relationships between predictor (independent) and response (dependent) data.*
#' @srrstatsTODO {RE7.1a} *In particular, these tests should confirm that model fitting is at least as fast or (preferably) faster than testing with equivalent noisy data (see RE2.4b).* 
#' @srrstatsTODO {RE7.3} Demonstrate and test expected behaviour when objects returned from regression software are submitted to the accessor methods of **RE4.2**--**RE4.7**.
#' @srrstatsTODO {RE7.4} Extending directly from **RE4.15**, where appropriate, tests should demonstrate and confirm that forecast errors, confidence intervals, or equivalent values increase with forecast horizons.
#' @noRd
NULL

#' NA_standards
#'
#' @srrstatsNA {G2.4d} *method does not use factors*
#' @srrstatsNA {G2.4e} *idem*
#' @srrstatsNA {G2.5} *no function has a factor argument*
#' @srrstatsNA {G2.14c} *Missing data are not replaced by imputed values, because the type of imputation can have a major impact on the model.*
#' @srrstatsNA {G3.1} *software does not rely on covariance calculation*
#' @srrstatsNA {G3.1a} *covariance methods cannot arbitrarily be specified* 
#' @srrstatsNA {RE1.0} *As this is a regression method for high-dimensional data, using the formula interface is not practical*
#' @srrstatsNA {RE1.1} *idem*
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
#' @srrstatsNA {RE6.1} *default plot method is generic*
#' @srrstatsNA {RE6.3} *As this package is not about time series, there is no difference between interpolation and extrapolation.*
#' @noRd
NULL
