# Package index

## R Package

- [`corila-package`](https://rauschenberger.github.io/corila/reference/corila-package.md)
  : Sparse modelling with grouped and correlated features allowing for
  privileged information

## Sparse group lasso regression

- [`cv.corila()`](https://rauschenberger.github.io/corila/reference/cv.corila.md)
  : Sparse group lasso regression

- [`coef(`*`<cv.corila>`*`)`](https://rauschenberger.github.io/corila/reference/coef.cv.corila.md)
  : Extract coefficients

- [`predict(`*`<cv.corila>`*`)`](https://rauschenberger.github.io/corila/reference/predict.cv.corila.md)
  : predict (S3 method)

- [`fitted(`*`<cv.corila>`*`)`](https://rauschenberger.github.io/corila/reference/fitted.cv.corila.md)
  : Fitted values

- [`residuals(`*`<cv.corila>`*`)`](https://rauschenberger.github.io/corila/reference/residuals.cv.corila.md)
  : Residuals

- [`nobs(`*`<cv.corila>`*`)`](https://rauschenberger.github.io/corila/reference/nobs.cv.corila.md)
  : Observation Count

- [`plot(`*`<cv.corila>`*`)`](https://rauschenberger.github.io/corila/reference/plot.cv.corila.md)
  : Plot Sparse Group Lasso (S3 method)

- [`deviance(`*`<cv.corila>`*`)`](https://rauschenberger.github.io/corila/reference/deviance.cv.corila.md)
  : Deviance

- [`methods`](https://rauschenberger.github.io/corila/reference/methods.md)
  :

  List of methods for class `"cv.corila"`

## Multi-penalty ridge regression

- [`multiridge()`](https://rauschenberger.github.io/corila/reference/multiridge.md)
  : Multi-penalty ridge regression
- [`coef(`*`<multiridge>`*`)`](https://rauschenberger.github.io/corila/reference/coef.multiridge.md)
  : Extract coefficients
- [`predict(`*`<multiridge>`*`)`](https://rauschenberger.github.io/corila/reference/predict.multiridge.md)
  : Make predictions

## Simulation

- [`simulate_data()`](https://rauschenberger.github.io/corila/reference/simulate_data.md)
  : Data simulation

## Internal functions

- [`calc_sign_prec()`](https://rauschenberger.github.io/corila/reference/calc_sign_prec.md)
  : Precision for sign variable
- [`corila()`](https://rauschenberger.github.io/corila/reference/corila.md)
  : Sparse group lasso regression (without cross-validation)
- [`.backscale()`](https://rauschenberger.github.io/corila/reference/dot-backscale.md)
  : Inverse standardisation
- [`.combine_slopes()`](https://rauschenberger.github.io/corila/reference/dot-combine_slopes.md)
  : Combine coefficients
- [`.deviance()`](https://rauschenberger.github.io/corila/reference/dot-deviance.md)
  : Deviance
- [`.estim_initial_coefs()`](https://rauschenberger.github.io/corila/reference/dot-estim_initial_coefs.md)
  : Initial coefficients
- [`.expand_auxiliary()`](https://rauschenberger.github.io/corila/reference/dot-expand_auxiliary.md)
  : Expand auxiliary features
- [`.folds()`](https://rauschenberger.github.io/corila/reference/dot-folds.md)
  : Fold identifiers
- [`.forescale()`](https://rauschenberger.github.io/corila/reference/dot-forescale.md)
  : Standardisation
- [`.is_adjacent()`](https://rauschenberger.github.io/corila/reference/dot-is_adjacent.md)
  : Adjacency indicator
- [`.mean_function()`](https://rauschenberger.github.io/corila/reference/dot-mean_function.md)
  : Mean function
- [`.residuals()`](https://rauschenberger.github.io/corila/reference/dot-residuals.md)
  : Deviance Residuals
- [`.set_candidates()`](https://rauschenberger.github.io/corila/reference/dot-set_candidates.md)
  : Candidate values
- [`.simulate_effects()`](https://rauschenberger.github.io/corila/reference/dot-simulate_effects.md)
  : Simulate effects
- [`.simulate_predictors()`](https://rauschenberger.github.io/corila/reference/dot-simulate_predictors.md)
  : Simulate predictors
- [`.simulate_response()`](https://rauschenberger.github.io/corila/reference/dot-simulate_response.md)
  : Simulate outcome
- [`.type()`](https://rauschenberger.github.io/corila/reference/dot-type.md)
  : Name method (helper function)
- [`predict(`*`<corila>`*`)`](https://rauschenberger.github.io/corila/reference/predict.corila.md)
  : predict (S3 method)
- [`print(`*`<cv.corila>`*`)`](https://rauschenberger.github.io/corila/reference/print.cv.corila.md)
  : print (S3 method)
- [`summary(`*`<cv.corila>`*`)`](https://rauschenberger.github.io/corila/reference/summary.cv.corila.md)
  [`print(`*`<summary.cv.corila>`*`)`](https://rauschenberger.github.io/corila/reference/summary.cv.corila.md)
  : Summarising sparse group lasso (S3 method)
- [`.validate_na_action()`](https://rauschenberger.github.io/corila/reference/validate.md)
  [`.validate_family()`](https://rauschenberger.github.io/corila/reference/validate.md)
  [`.validate_x()`](https://rauschenberger.github.io/corila/reference/validate.md)
  [`.validate_y()`](https://rauschenberger.github.io/corila/reference/validate.md)
  [`.validate_y_hat()`](https://rauschenberger.github.io/corila/reference/validate.md)
  [`.validate_primary()`](https://rauschenberger.github.io/corila/reference/validate.md)
  [`.validate_cor()`](https://rauschenberger.github.io/corila/reference/validate.md)
  [`.validate_alpha()`](https://rauschenberger.github.io/corila/reference/validate.md)
  [`.validate_group()`](https://rauschenberger.github.io/corila/reference/validate.md)
  [`.validate_hyper()`](https://rauschenberger.github.io/corila/reference/validate.md)
  [`.validate_foldid()`](https://rauschenberger.github.io/corila/reference/validate.md)
  : Validation functions
