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
  be suppressed? (`FALSE` or `TRUE`)

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
n <- 20L
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
#>  [1] -0.04210526  0.58947368 -0.38646617  0.26466165  0.04511278 -0.13834586
#>  [7] -0.32781955  0.18646617 -0.11278195  0.75187970
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
                     lambda = NULL)
#> Warning: number of rows of result is not a multiple of vector length (arg 1)
#> Warning: Option grouped=FALSE enforced in cv.glmnet, since < 3 observations per fold
#> $coef
#>  [1] -0.02515595  1.04723119 -0.36371329  0.07573726  0.05057138 -0.03498746
#>  [7] -0.89062638  0.07621415 -0.22428786  1.34885909
#> 
#> $lambda
#> [1] 0.1763745
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
#>  [1] -0.02760918  1.03400885 -0.36489326  0.08274210  0.05476363 -0.03841544
#>  [7] -0.87535485  0.08259835 -0.21968587  1.33762730
#> 
#> $lambda
#> [1] 0.2
#> 
```
