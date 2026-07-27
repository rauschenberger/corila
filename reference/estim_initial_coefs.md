# Initial coefficients

Estimate initial coefficients.

## Usage

``` r
.estim_initial_coefs(
  x,
  y,
  family = "gaussian",
  alpha_init = 0,
  group = NULL,
  foldid = NULL,
  nfolds = 10L,
  lambda = NULL,
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

- group:

  \\p\\-dimensional integer vector with entries in \\\\1, \ldots, q\\\\

- foldid:

  \\n_0\\-dimensional vector containing the fold identifiers (minimum
  \\1\\, maximum `nfolds`)

- nfolds:

  positive integer specifying the number of folds (minimum \\3\\,
  maximum \\n\\)

  NB: If `foldid` is provided, `nfolds` is overwritten by `max(foldid)`.

- lambda:

  numeric scalar, or `NULL` (determined by cross-validation)

- silent:

  Should messages from
  [`glmnet::glmnet()`](https://glmnet.stanford.edu/reference/glmnet.html)
  and
  [`glmnet::cv.glmnet()`](https://glmnet.stanford.edu/reference/cv.glmnet.html)
  be suppressed? (logical scalar, `FALSE` or `TRUE`)

## Value

Returns a list with two slots:

- `coef`: numeric vector of length \\p\\ (estimated coefficients)

- `lambda`: non-negative numeric scalar (optimised regularisation
  parameter) or `NULL`

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
set.seed(1)
n <- 50L
p <- 10L
x <- matrix(rnorm(n * p), nrow = n, ncol = p)
beta <- rbinom(n = p, size = 1L, prob = 0.5) * rnorm(p)
y <- drop(x %*% beta)

# initial correlation coefficients
.estim_initial_coefs(x = x,
                     y = y,
                     family = "gaussian",
                     alpha_init = "spearman",
                     group = NULL,
                     foldid = NULL,
                     nfolds = 10L,
                     lambda = NULL)
#> $coef
#>  [1]  0.53478992 -0.50991597  0.31870348  0.19558223 -0.09483794 -0.02713085
#>  [7] -0.18953181 -0.42223289 -0.03990396 -0.30554622
#> 
#> $lambda
#> NULL
#> 

# initial regression coefficients (cross-validating lambda)
foldid <- sample(seq_len(10L), size = n, replace = TRUE)
.estim_initial_coefs(x = x,
                     y = y,
                     family = "gaussian",
                     alpha_init = 0.0,
                     group = NULL,
                     foldid = foldid,
                     nfolds = 10L,
                     lambda = NULL,
                     silent = TRUE)
#> $coef
#>  [1]  1.47303510 -1.30312590  0.04308875  1.05078015 -0.02502813 -0.02995143
#>  [7] -0.85606093 -0.92824916  0.04838767 -0.39441178
#> 
#> $lambda
#> [1] 0.126489
#> 

# initial regression coefficients (using fixed lambda)
.estim_initial_coefs(x = x,
                     y = y,
                     family = "gaussian",
                     alpha_init = 0.0,
                     group = NULL,
                     foldid = NULL,
                     nfolds = 10L,
                     lambda = 0.2)
#> $coef
#>  [1]  1.41315853 -1.26524541  0.06226520  0.99767196 -0.03630484 -0.04342165
#>  [7] -0.81640523 -0.90602485  0.04799027 -0.38908914
#> 
#> $lambda
#> [1] 0.2
#> 
```
