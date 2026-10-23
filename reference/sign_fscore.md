# F-score for sign variable

**\[experimental\]**

Calculates the F-score for ternary variables with support \\\\-1, 0,
1\\\\.

## Usage

``` r
sign_fscore(truth, estim)
```

## Arguments

- truth:

  integer vector with values in \\\\-1, 0, 1\\\\

- estim:

  integer vector of same length with values in \\\\-1, 0, 1\\\\

## Value

Returns a scalar between 0 (minimum) and 1 (maximum), or `NA`. (Returns
`NA` if precision or recall equals `NA`, and returns 0 if precision and
recall equal 0.)

## Examples

``` r
truth <- sample(x = c(-1L, 0L, 1L), size = 10L, replace = TRUE)
estim <- sample(x = c(-1L, 0L, 1L), size = 10L, replace = TRUE)
sign_fscore(truth = truth, estim = estim) # observed value
#> [1] 0.4
sign_fscore(truth = truth, estim = -truth) # lower limit 0
#> [1] 0
sign_fscore(truth = truth, estim = truth) # upper limit 1
#> [1] 1
sign_fscore(truth = 0L * truth, estim = estim) # not defined
#> [1] NA
sign_fscore(truth = truth, estim = 0L *estim) # not defined
#> [1] NA
```
