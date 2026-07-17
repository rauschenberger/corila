# Precision for sign variable

Calculates precision for ternary variables with support \\\\-1, 0,
1\\\\, i.e., the proportion of positive or negative estimated signs that
match the true sign.

## Usage

``` r
calc_sign_prec(truth, estim)
```

## Arguments

- truth:

  integer vector with values in \\\\-1, 0, 1\\\\

- estim:

  integer vector of same length with values in \\\\-1, 0, 1\\\\

## Value

Returns a scalar between 0 (minimum precision) and 1 (maximum
precision), or `NA` if all estimated signs equal 0.

## Examples

``` r
truth <- sample(x = c(-1L, 0L, 1L), size = 10L, replace = TRUE)
estim <- sample(x = c(-1L, 0L, 1L), size = 10L, replace = TRUE)
calc_sign_prec(truth = truth, estim = estim) # observed value
#> [1] 0.4285714
calc_sign_prec(truth = truth, estim = -truth) # lower limit 0
#> [1] 0
calc_sign_prec(truth = truth, estim = truth) # upper limit 1
#> [1] 1
calc_sign_prec(truth = truth, estim = 0L * estim) # not defined
#> [1] NA
```
