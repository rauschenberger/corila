# Sparse Group Lasso

Optimises the parameters and the hyperparameters of the sparse group
lasso.

## Usage

``` r
cv.corila(
  x,
  y,
  group,
  primary = NULL,
  alpha_init = 0,
  alpha_final = 1,
  family = "gaussian",
  nfolds = 10,
  cor = "spearman",
  tune = "weight",
  foldid = NULL
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

- family:

  character string `"gaussian"`, `"binomial"`, `"poisson"`, or `"cox"`

- nfolds:

  integer specifying the number of folds

- cor:

  character string `"pearson"`, `"spearman"` (default), or `"kendall"`;
  or \\p \times p\\ correlation matrix

- tune:

  character string for determining the candidate values for the
  hyperparameters:

  - "none": fixed weights and exponents (`wgt_local`=1, `exp_local`=1,
    `wgt_global`=0), no tuning

  - "weight": fixed exponents (`exp_local`=0, `exp_global`=1), tuning
    `wgt_local`=1-`wgt_global`

  - "exponent": fixed weights (`wgt_local`=1, `wgt_global`=0), tuning
    `exp_local`

  - "bivariate": tuning `wgt_local`=1-`wgt_global` and
    `exp_local`=`exp_global`

  - "factorial": tuning `wgt_local`, `exp_local`, `wgt_global`,
    `exp_global`

  (to implement: list with slots `wgt_local`, `exp_local`, `wgt_global`,
  and `exp_global`)

- foldid:

  \\n_0\\-dimensional vector containing the fold identifiers

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

Extract coefficients with
[coef()](https://rauschenberger.github.io/corila/reference/coef.cv.corila.md)
and make predictions with
[predict()](https://rauschenberger.github.io/corila/reference/predict.cv.corila.md).

This user function repeatedly calls
[`corila()`](https://rauschenberger.github.io/corila/reference/corila.md)
with different values for the regularisation and mixing hyperparameters.

## Examples

``` r
# minimal example
n <- 50; p <- 20; q <- 5
x <- matrix(rnorm(n * p), nrow = n , ncol = p)
y <- rnorm(n)
group <- rep(seq_len(q), length.out = p)
primary <- as.logical(rbinom(n = p, size = 1, prob = 0.5))
cv.corila(x = x, y = y, group = group, primary = primary, tune = "none")
#> object of class ‘cv.corila’ 
#> (contains an object of class ‘cv.glmnet’)

# \donttest{
# simulation
set.seed(1)
n0 <- 100
n1 <- 10000
n <- n0 + n1
p <- c(100, 50)
z <- rep(x = seq_along(p), times = p)
x <- sapply(X = z, FUN = function(x) stats::rnorm(n = n, sd = x))
beta <- stats::rnorm(n = sum(p), mean = 1, sd = 0) *
        stats::rbinom(n = sum(p), size = 1, prob = 0.2)
eta <- x %*% beta
family <- "gaussian"
if (identical(family, "gaussian")) {
  y <- eta + 0.5 * stats::rnorm(n = n, sd = stats::sd(eta))
} else if (identical(family, "binomial")) {
  y <- stats::rbinom(n = n, size = 1, prob = 1 / (1 + exp(-eta)))
} else if (identical(family, "cox")) {
  time <- stats::rexp(n = n, rate = exp(eta))
  status <- stats::rbinom(n = n, prob = 0.5, size = 1)
  y <- survival::Surv(time = time, event = status)
}
cond <- rep(x = c(TRUE, FALSE), times = c(n0, n1))

y_hat <- coef <- list()

# standard lasso regression
object <- glmnet::cv.glmnet(x = x[cond, ], y = y[cond],
                            family = family, alpha = 1)
coef$glmnet <- stats::coef(object = object, s = "lambda.min")
y_hat$glmnet <- stats::predict(object = object, newx = x[!cond, ],
                               type = "response", s = "lambda.min")

# flexible group lasso regression
object <- cv.corila(x = x[cond, ], y = y[cond], group = z, family = family)
coef$corila <- stats::coef(object = object)
y_hat$corila <- stats::predict(object = object, newx = x[!cond, ])

# selection performance
sapply(coef, function(x) mean(sign(x[-1]) == sign(beta)))
#>    glmnet    corila 
#> 0.8066667 0.7866667 
sapply(coef, function(x) {
  sum(sign(x[-1]) != 0 & sign(x[-1]) == sign(beta)) / sum(x[-1] != 0)
})
#>    glmnet    corila 
#> 0.5675676 0.5227273 

# predictive performance
if (identical(family, "gaussian")) {
  metric <- sapply(X = y_hat, FUN = function(x)
    mean((x-y[!cond])^2))
} else if (identical(family, "binomial")) {
  metric <- sapply(X = y_hat, FUN = function(x)
    pROC::auc(response = y[!cond],
              predictor = as.vector(x),
              levels = c(0, 1),
              direction = "<"))
} else if (identical(family, "cox")) {
  metric <- sapply(X = y_hat, FUN = function(x)
    survival::concordance(y[!cond]~I(-x))$concordance)
}
metric
#>   glmnet   corila 
#> 61.23431 49.25623 

# privileged information
#primary <- stats::rbinom(n = sum(p), size = 1, prob = 0.5) == 1
#object <- cv.corila(x = x[cond, ], y = y[cond], group = z,
#                     primary = primary, family = family)
# }
```
