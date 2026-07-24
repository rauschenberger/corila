# Validation functions

These functions validate the arguments of the function
[`cv.corila()`](https://rauschenberger.github.io/corila/reference/cv.corila.md),
its helper functions, and its S3 methods. They check whether the
provided arguments satisfy expectations, and return them in standardised
forms (e.g., as integers instead of integerish numerics).

## Usage

``` r
.validate_na_action(na_action)

.validate_family(family, poisson = TRUE)

.validate_x(x, na_action)

.validate_y(y, family, n, na_action, names)

.validate_y_hat(y_hat, family, n)

.validate_primary(primary, p, names)

.validate_cor(cor, p, names)

.validate_alpha(alpha, init)

.validate_group(group, p, names)

.validate_hyper(hyper)

.validate_foldid(foldid, y, family)
```

## Arguments

- na_action:

  character `"error"` to trigger an error if any observation has a
  missing predictor or a missing response or `"complete_cases"` to
  exclude observations with a missing predictor or a missing response
  from model fitting (while providing fitted values for these
  observations)

- family:

  character string `"gaussian"`, `"binomial"`, `"poisson"`, or `"cox"`

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

- names:

  character vector of length \\n\\ or \\p\\ for names of observations or
  predictors

- y_hat:

  \\n\\-dimensional vector of fitted values or probabilities

- primary:

  \\p\\-dimensional logical vector indicating whether a predictor may be
  included in the final model (`TRUE` for "primary predictors") or must
  be excluded from the final model (`FALSE` for "auxiliary predictors")

- cor:

  character string `"pearson"`, `"spearman"` (default), or `"kendall"`;
  or a correlation matrix (\\p\\ rows, \\p\\ columns, entries between
  \\-1\\ and \\+1\\)

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

- foldid:

  \\n_0\\-dimensional vector containing the fold identifiers (minimum
  \\1\\, maximum `nfolds`)

## Value

Return the first argument invisibly. Throw an error for invalid
arguments.

## Details

These functions are called by
[`cv.corila()`](https://rauschenberger.github.io/corila/reference/cv.corila.md),
its helper functions, and its S3 methods.
