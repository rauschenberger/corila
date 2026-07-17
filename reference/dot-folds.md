# Fold identifiers

Splits observations into balanced and stratified folds.

## Usage

``` r
.folds(y, family, nfolds)
```

## Arguments

- y:

  response vector of length \\n_0\\, containing numerical values
  (`family="gaussian"`), integer values (`family="poisson"`), binary
  values (`family="binomial"`), or a survival object created with
  [`survival::Surv()`](https://rdrr.io/pkg/survival/man/Surv.html)
  (`family="cox"`), where \\n_0\\ is the number of observations used for
  model training

- family:

  character string `"gaussian"`, `"binomial"`, `"poisson"`, or `"cox"`

- nfolds:

  positive integer specifying the number of folds (minimum \\3\\,
  maximum \\n\\)

## Value

Returns an \\n_0\\-dimensional vector with entries in \\\\1, \ldots,
\\`nfolds`\\\\\\.

## Details

Randomly splits observations into balanced folds (approximately the same
number of observations per fold) and stratified folds (separate
splitting for both classes in binomial family or censored/uncensored
observations in Cox model).

## Examples

``` r
# Gaussian and Poisson families
y <- stats::rnorm(n = 100L)
y <- stats::rpois(n = 100L, lambda = 4)
foldid <- .folds(y = y, family = "gaussian", nfolds = 10L)
table(foldid)
#> foldid
#>  1  2  3  4  5  6  7  8  9 10 
#> 10 10 10 10 10 10 10 10 10 10 

# binomial family
y <- stats::rbinom(n = 100L, size = 1L, prob = 0.2)
foldid <- .folds(y = y, family = "binomial", nfolds = 10L)
table(y, foldid)
#>    foldid
#> y   1 2 3 4 5 6 7 8 9 10
#>   0 9 9 8 8 8 8 8 8 8  8
#>   1 1 1 2 2 2 2 2 2 2  2

# \donttest{
# Cox model
time <- stats::rexp(n = 100L, rate = 5)
status <- stats::rbinom(n = 100L, size = 1L, prob = 0.2)
y <- survival::Surv(time = time, event = status)
foldid <- .folds(y = y, family = "cox", nfolds = 10L)
table(y[, "status"], foldid)
#>    foldid
#>     1 2 3 4 5 6 7 8 9 10
#>   0 8 8 8 8 8 8 8 7 7  7
#>   1 2 2 2 2 2 2 2 3 3  3
# }
```
