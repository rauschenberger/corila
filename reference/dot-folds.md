# Fold identifiers

Splits observations into balanced and stratified folds.

## Usage

``` r
.folds(y, family, nfolds)
```

## Arguments

- y:

  \\n_0\\-dimensional response vector, where \\n_0\\ is the number of
  observations used for model training

- family:

  character string `"gaussian"`, `"binomial"`, `"poisson"`, or `"cox"`

- nfolds:

  integer specifying the number of folds

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
y <- stats::rnorm(n = 100)
y <- stats::rpois(n = 100, lambda = 4)
foldid <- corila:::.folds(y = y, family = "gaussian", nfolds = 10)
table(foldid)
#> foldid
#>  1  2  3  4  5  6  7  8  9 10 
#> 10 10 10 10 10 10 10 10 10 10 

# binomial family
y <- stats::rbinom(n = 100, prob = 0.2, size = 1)
foldid <- corila:::.folds(y = y, family = "binomial", nfolds = 10)
table(y, foldid)
#>    foldid
#> y   1 2 3 4 5 6 7 8 9 10
#>   0 8 8 8 8 8 8 8 8 9  8
#>   1 2 2 2 2 2 2 2 1 2  2

# \donttest{
# Cox model
time <- stats::rexp(n = 100, rate = 5)
status <- stats::rbinom(n = 100, prob = 0.2, size = 1)
y <- survival::Surv(time = time, event = status)
foldid <- corila:::.folds(y = y, family = "cox", nfolds = 10)
table(y[, "status"], foldid)
#>    foldid
#>     1 2 3 4 5 6 7 8 9 10
#>   0 8 8 8 8 8 8 8 8 8  7
#>   1 2 2 2 3 2 2 2 2 2  2
# }
```
