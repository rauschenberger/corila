# Sparse group lasso regression (without cross-validation)

Fits an initial ridge regression to obtain weights for an adaptive lasso
regression that allows for heterogeneous, overlapping and unknown groups
of correlated variables.

## Usage

``` r
corila(
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
  lambda_init,
  silent = FALSE,
  threshold = 0
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
  `family="poisson"` or overlapping groups), or `NA` to set all initial
  coefficients equal to 1

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

- silent:

  Should messages from
  [`glmnet::glmnet()`](https://glmnet.stanford.edu/reference/glmnet.html)
  and
  [`glmnet::cv.glmnet()`](https://glmnet.stanford.edu/reference/cv.glmnet.html)
  be suppressed? logical

- threshold:

  threshold for absolute correlation coefficients: numeric in unit
  interval

## Value

Returns an object of class `"corila"`.

## Details

The number of observations (samples) for training or testing are
indicated by \\n_0\\ and \\n_1\\, respectively, the number of variables
(features) is indicated by \\p\\, and the number of variable groups is
indicated by \\q\\. Observations (samples) are indexed by \\i\\ in
\\\\1, \ldots, n\\\\, variables (features) are indexed by \\j\\ in
\\\\1, \ldots, p\\\\, and variable groups are indexed by \\k\\ in \\\\1,
\ldots, q\\\\. The number of variables in the \\k^{\text{th}}\\ group is
indicated by \\p_k\\, with \\\sum\_{k=1}^q p_k = p\\ for non-overlapping
groups.

## References

[Armin Rauschenberger](https://orcid.org/0000-0001-6498-4801) (2026).
"Sparse modelling with grouped and correlated features allowing for
privileged information". *In preparation*.

## See also

Estimate parameters and tune hyperparameters (using cross-validation)
with
[`cv.corila()`](https://rauschenberger.github.io/corila/reference/cv.corila.md).
Make predictions for a range of hyperparameters with
[predict()](https://rauschenberger.github.io/corila/reference/predict.corila.md).

This function calls
[`.forescale()`](https://rauschenberger.github.io/corila/reference/dot-forescale.md)
and
[`.backscale()`](https://rauschenberger.github.io/corila/reference/dot-backscale.md)
for standardising data and bringing results back to the original scale,
respectively,
[`.folds()`](https://rauschenberger.github.io/corila/reference/dot-folds.md)
for splitting samples into folds,
[`.estim_initial_coefs()`](https://rauschenberger.github.io/corila/reference/dot-estim_initial_coefs.md)
for obtaining initial coefficients,
[`.is_adjacent()`](https://rauschenberger.github.io/corila/reference/dot-is_adjacent.md)
for identifying adjacent predictors, and
[`glmnet::cv.glmnet()`](https://glmnet.stanford.edu/reference/cv.glmnet.html)
and
[`glmnet::glmnet()`](https://glmnet.stanford.edu/reference/glmnet.html)
for adaptive lasso regression.

## Examples

``` r
# \donttest{
# simulation
n <- 100
p <- 50
group <- rep(x = 1:10, each = 5)
primary <- NULL
x <- matrix(data = rnorm(n * p), nrow = n, ncol = p)
y <- rnorm(n = n)

# model fitting
hyper <- data.frame(exp_local = 1, wgt_local = 0.5,
                    exp_global = 1, wgt_global = 0.5)
object <- corila:::corila(x = x,
                          y = y,
                          group = group,
                          primary = primary,
                          family = "gaussian",
                          alpha_init = 0,
                          alpha_final = 1,
                          cor = "spearman",
                          foldid = NULL,
                          nfolds = 10,
                          hyper = hyper,
                          lambda_init = NULL)
#> Error in !c(primary, primary): invalid argument type

y_hat <- stats::predict(object, newx = x, index = 1, s = 0)
#> Error: object 'object' not found
# }
```
