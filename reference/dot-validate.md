# Argument check

Checks arguments of functions
[`corila()`](https://rauschenberger.github.io/corila/reference/corila.md)
and
[`cv.corila()`](https://rauschenberger.github.io/corila/reference/cv.corila.md).

## Usage

``` r
.validate(
  x,
  y,
  group,
  primary,
  family,
  hyper,
  alpha_init,
  alpha_final,
  cor,
  foldid,
  nfolds,
  lambda_init
)
```

## Arguments

- x:

  \\n_0 \times p\\ predictor matrix, where \\n_0\\ is the number of
  observations used for model training and \\p\\ is the number of
  variables

- y:

  \\n_0\\-dimensional response vector, where \\n_0\\ is the number of
  observations used for model training

- group:

  group structure (three options):

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

- hyper:

  list of \\m\\-dimensional vectors or a data frame with \\m\\ rows
  containing candidate values for the regularisation and mixing
  hyperparameters

- alpha_init:

  elastic net mixing parameter (\\0 \leq\\ `alpha_init` \\\leq 1\\) for
  initial regression (default: ridge penalisation with `alpha_init`=0);
  alternative choices are `"pearson"`, `"spearman"`, or `"kendall"` to
  use initial correlation coefficients (not implemented for
  `family="cox"`), `"multiridge"` for multi-penalty ridge regression
  with one penalty for each group (not implemented for
  `family="poisson"` or overlapping groups, falls back to `alpha_init=0`
  for `family="poisson"`), or `NA` to set all initial coefficients equal
  to 1

- alpha_final:

  elastic net mixing parameter for final regression (default: lasso
  penalisation with `alpha_final`=1)

- cor:

  character string `"pearson"`, `"spearman"` (default), or `"kendall"`;
  or \\p \times p\\ correlation matrix

- foldid:

  \\n_0\\-dimensional vector containing the fold identifiers

- nfolds:

  integer specifying the number of folds

- lambda_init:

  regularisation hyperparameter(s), or `NULL` (cross-validation)

## Value

Returns a list with slots `n`, `p`, and `q` or an error message.

## Details

This function is called by
[`corila()`](https://rauschenberger.github.io/corila/reference/corila.md)
and
[`cv.corila()`](https://rauschenberger.github.io/corila/reference/cv.corila.md).
It repeatedly calls
[`.assert()`](https://rauschenberger.github.io/corila/reference/dot-assert.md).

## See also

Use
[`.assert()`](https://rauschenberger.github.io/corila/reference/dot-assert.md)
to validate individual arguments.
