# Initial coefficients

Estimate initial coefficients.

## Usage

``` r
.estim_initial_coefs(
  x,
  y,
  family,
  alpha_init,
  group,
  foldid,
  nfolds,
  lambda,
  silent
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

- family:

  character string `"gaussian"`, `"binomial"`, `"poisson"`, or `"cox"`

- alpha_init:

  elastic net mixing parameter (\\0 \leq\\ `alpha_init` \\\leq 1\\) for
  initial regression (default: ridge penalisation with `alpha_init`=0);
  alternative choices are `"pearson"`, `"spearman"`, or `"kendall"` to
  use initial correlation coefficients (not implemented for
  `family="cox"`), `"multiridge"` for multi-penalty ridge regression
  with one penalty for each group (not implemented for
  `family="poisson"` or overlapping groups), or `NA` to set all initial
  coefficients equal to 1

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

- foldid:

  \\n_0\\-dimensional vector containing the fold identifiers

- nfolds:

  integer specifying the number of folds

- lambda:

  numeric scalar, or `NULL` (determined by cross-validation)

- silent:

  Should messages from
  [`glmnet::glmnet()`](https://rdrr.io/pkg/glmnet/man/glmnet.html) and
  [`glmnet::cv.glmnet()`](https://rdrr.io/pkg/glmnet/man/cv.glmnet.html)
  be suppressed? logical

## Details

This function is called by
[`corila()`](https://rauschenberger.github.io/corila/reference/corila.md).
It calls
[`glmnet::cv.glmnet()`](https://rdrr.io/pkg/glmnet/man/cv.glmnet.html)
or [`glmnet::glmnet()`](https://rdrr.io/pkg/glmnet/man/glmnet.html) for
an initial lasso, ridge, or elastic net regression,
[`multiridge()`](https://rauschenberger.github.io/corila/reference/multiridge.md)
for an initial multi-penalty ridge regression, or
[`stats::cor()`](https://rdrr.io/r/stats/cor.html) for initial
correlation coefficients.

## Examples

``` r
n <- 20
p <- 10
x <- matrix(rnorm(n * p), nrow = n, ncol = p)
beta <- rbinom(n = p, size = 1, prob = 0.5) * rnorm(p)
y <- drop(x %*% beta)
corila:::.estim_initial_coefs(x = x,
                              y = y,
                              family = "gaussian",
                              alpha_init = "spearman",
                              group = NULL,
                              foldid = NULL,
                              nfolds = 10,
                              lambda = NULL)
#> $coef
#>  [1]  0.21503759  0.08872180 -0.01052632 -0.40751880  0.42406015  0.07669173
#>  [7]  0.10375940  0.07518797 -0.78045113  0.26616541
#> 
#> $lambda
#> NULL
#> 
```
