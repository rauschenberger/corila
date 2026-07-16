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

  positive integer specifying the number of folds (minimum \\3\\,
  maximum \\n\\)

- lambda:

  numeric scalar, or `NULL` (determined by cross-validation)

- silent:

  Should messages from
  [`glmnet::glmnet()`](https://glmnet.stanford.edu/reference/glmnet.html)
  and
  [`glmnet::cv.glmnet()`](https://glmnet.stanford.edu/reference/cv.glmnet.html)
  be suppressed? logical

## Details

This function is called by
[`corila()`](https://rauschenberger.github.io/corila/reference/corila.md).
It calls
[`glmnet::cv.glmnet()`](https://glmnet.stanford.edu/reference/cv.glmnet.html)
or
[`glmnet::glmnet()`](https://glmnet.stanford.edu/reference/glmnet.html)
for an initial lasso, ridge, or elastic net regression,
[`multiridge()`](https://rauschenberger.github.io/corila/reference/multiridge.md)
for an initial multi-penalty ridge regression, or
[`stats::cor()`](https://rdrr.io/r/stats/cor.html) for initial
correlation coefficients.

## Examples

``` r
# simulate data
n <- 20
p <- 10
x <- matrix(rnorm(n * p), nrow = n, ncol = p)
beta <- rbinom(n = p, size = 1, prob = 0.5) * rnorm(p)
y <- drop(x %*% beta)

# initial correlation coefficients
corila:::.estim_initial_coefs(x = x,
                              y = y,
                              family = "gaussian",
                              alpha_init = "spearman",
                              group = NULL,
                              foldid = NULL,
                              nfolds = 10,
                              lambda = NULL)
#> $coef
#>  [1]  0.14736842  0.35037594 -0.03909774  0.29924812  0.43759398 -0.17293233
#>  [7]  0.17293233 -0.10977444 -0.72180451  0.14135338
#> 
#> $lambda
#> NULL
#> 

# initial regression coefficients (cross-validating lambda)
corila:::.estim_initial_coefs(x = x,
                              y = y,
                              family = "gaussian",
                              alpha_init = 0,
                              group = NULL,
                              foldid = NULL,
                              nfolds = 10,
                              lambda = NULL)
#> Warning: Option grouped=FALSE enforced in cv.glmnet, since < 3 observations per fold
#> $coef
#>  [1] -0.01003925  1.87125995 -0.04623964  0.85885005  0.62140233  0.01499372
#>  [7]  0.06481836  0.03630338 -1.41391759  0.05301106
#> 
#> $lambda
#> [1] 0.2427865
#> 
                              
# initial regression coefficients (using fixed lambda)
corila:::.estim_initial_coefs(x = x,
                              y = y,
                              family = "gaussian",
                              alpha_init = 0,
                              group = NULL,
                              foldid = NULL,
                              nfolds = 10,
                              lambda = 0.2)
#> $coef
#>  [1] -0.01003925  1.87125995 -0.04623964  0.85885005  0.62140233  0.01499372
#>  [7]  0.06481836  0.03630338 -1.41391759  0.05301106
#> 
#> $lambda
#> [1] 0.2
#> 
```
