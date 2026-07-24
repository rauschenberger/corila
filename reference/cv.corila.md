# Sparse group lasso regression

Optimises the parameters and the hyperparameters of the sparse group
lasso.

## Usage

``` r
cv.corila(
  x,
  y,
  group,
  primary = NULL,
  family = "gaussian",
  alpha_init = 0,
  cor = "spearman",
  alpha_final = 1,
  nfolds = 10L,
  foldid = NULL,
  tune = "weight",
  na_action = "error",
  silent = FALSE
)
```

## Arguments

- x:

  \\n_0 \times p\\ predictor matrix, containing only numerical values
  (continuous, integer, or binary), where \\n_0\\ is the number of
  observations used for model training and \\p\\ is the number of
  predictors

- y:

  response vector of length \\n_0\\, containing numerical values
  (`family="gaussian"`), integer values (`family="poisson"`), binary
  values (`family="binomial"`), or a survival object created with
  [`survival::Surv()`](https://rdrr.io/pkg/survival/man/Surv.html)
  (`family="cox"`), where \\n_0\\ is the number of observations used for
  model training

- group:

  group structure (multiple options):

  - \\p\\-dimensional vector of group indices (in \\\\1, \ldots, q\\\\)
    or labels,

  - list with \\q\\ slots containing the variable indices (in \\\\1,
    \ldots, p\\\\) or labels,

  - \\p \times p\\ matrix, where the entry in the \\j^{\text{th}}\\ row
    and the \\k^{\text{th}}\\ column indicates whether information
    should be transferred from the \\j^{\text{th}}\\ to the
    \\k^{\text{th}}\\ variable

- primary:

  \\p\\-dimensional logical vector indicating whether a predictor may be
  included in the final model (`TRUE` for "primary predictors") or must
  be excluded from the final model (`FALSE` for "auxiliary predictors")

- family:

  character string `"gaussian"`, `"binomial"`, `"poisson"`, or `"cox"`

- alpha_init:

  A scalar specifying the method used for obtaining initial
  coefficients:

  - a numeric scalar in the unit interval (\\0 \leq\\ `alpha_init`
    \\\leq 1\\) to define the mixing parameter for elastic net
    regression (default: ridge penalisation with `alpha_init`=0);

  - the character scalar `"pearson"`, `"spearman"`, or `"kendall"` to
    use initial correlation coefficients (not implemented for
    `family="cox"`)

  - the character scalar`"multiridge"` to use multi-penalty ridge
    regression with one penalty for each group (not implemented for
    `family="poisson"` or overlapping groups),

  - `NA` to set all initial coefficients equal to 1

- cor:

  character string `"pearson"`, `"spearman"` (default), or `"kendall"`;
  or a correlation matrix (\\p\\ rows, \\p\\ columns, entries between
  \\-1\\ and \\+1\\)

- alpha_final:

  elastic net mixing parameter for final regression: numeric between 0
  for ridge penalisation and 1 for lasso penalisation (default: lasso
  penalisation with `alpha_final`=1)

- nfolds:

  positive integer specifying the number of folds (minimum \\3\\,
  maximum \\n\\)

  NB: If `foldid` is provided, `nfolds` is overwritten by `max(foldid)`.

- foldid:

  \\n_0\\-dimensional vector containing the fold identifiers (minimum
  \\1\\, maximum `nfolds`)

- tune:

  character string for determining the candidate values for the
  hyperparameters:

  - `"none"`: fixed weights and exponents (`wgt_local`=1, `exp_local`=1,
    `wgt_global`=0), no tuning

  - `"weight"`: fixed exponents (`exp_local`=0, `exp_global`=1), tuning
    `wgt_local`=1-`wgt_global`

  - `"exponent"`: fixed weights (`wgt_local`=1, `wgt_global`=0), tuning
    `exp_local`

  - `"bivariate"`: tuning `wgt_local`=1-`wgt_global` and
    `exp_local`=`exp_global`

  - `"factorial"`: tuning `wgt_local`, `exp_local`, `wgt_global`,
    `exp_global`

  (to implement: data frame with columns `wgt_local`, `exp_local`,
  `wgt_global`, and `exp_global`)

- na_action:

  character `"error"` to trigger an error if any observation has a
  missing predictor or a missing response or `"complete_cases"` to
  exclude observations with a missing predictor or a missing response
  from model fitting (while providing fitted values for these
  observations)

- silent:

  Should messages from
  [`glmnet::glmnet()`](https://rdrr.io/pkg/glmnet/man/glmnet.html) and
  [`glmnet::cv.glmnet()`](https://rdrr.io/pkg/glmnet/man/cv.glmnet.html)
  be suppressed? (logical scalar, `FALSE` or `TRUE`)

## Value

Returns an object of class `"cv.corila"`, a list with the following
slots:

- `model`: list with one slot for each combination of hyperparameters,
  each slot contains an object of class `"glmnet"`

- `hyper`: data frame with one row for each combination of
  hyperparameters, four columns for the values of the hyperparameters
  (`wgt_local`, `exp_local`, `wgt_global`, and `exp_global`) and a
  column for the cross-validated loss (`cvm`)

- `id_hyper`: index of combination of hyperparameters leading to the
  lowest cross-validated loss

- `lambda.min` optimised regularisation hyperparameter

- `scale`: output from
  [`.forescale()`](https://rauschenberger.github.io/corila/reference/dot-forescale.md)

- `y_hat`: \\n\\-dimensional vector of fitted values

## Details

The numbers of observations (samples) for training or testing are
indicated by \\n_0\\ and \\n_1\\, respectively, the number of predictors
(features) is indicated by \\p\\, and the number of predictor group is
indicated by \\q\\. Observations are indexed by \\i\\ in \\\\1, \ldots,
n\\\\, predictors are indexed by \\j\\ in \\\\1, \ldots, p\\\\, and
predictor groups are indexed by \\k\\ in \\\\1, \ldots, q\\\\. The
number of predictors in the \\k^{\text{th}}\\ group is indicated by
\\p_k\\, with \\\sum\_{k=1}^q p_k = p\\ for non-overlapping groups.

## References

[Armin Rauschenberger](https://orcid.org/0000-0001-6498-4801) (2026).
"Sparse modelling with grouped and correlated features allowing for
privileged information". *In preparation*.

## See also

Extract coefficients with
[coef()](https://rauschenberger.github.io/corila/reference/coef.cv.corila.md)
and make predictions with
[predict()](https://rauschenberger.github.io/corila/reference/predict.cv.corila.md).

This user function repeatedly calls
[`corila()`](https://rauschenberger.github.io/corila/reference/corila.md)
with different values for the regularisation and mixing hyperparameters.

The arguments of this function are validated with the helper functions
listed
[here](https://rauschenberger.github.io/corila/reference/validate.md).

## Examples

``` r
# \donttest{
data <- simulate_data()
model <- cv.corila(x = data$x_train,
                   y = data$y_train,
                   group = data$group,
                   primary = data$primary)
beta_hat <- coef(object = model)
y_hat <- predict(object = model, newx = data$x_test)
# }

# example for automatic mutation testing (with the R package autotest)
data <- simulate_data()
model <- cv.corila(x = data$x_train,
                   y = data$y_train,
                   group = as.double(data$group),
                   primary = data$primary,
                   alpha_init = 0.0,
                   foldid = rep(1:10, length.out = nrow(data$x_train)))
```
