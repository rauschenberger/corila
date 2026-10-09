# Recall for sign variable

**\[experimental\]**

Calculates recall for ternary variables with support \\\\-1, 0, 1\\\\,
i.e., the proportion of correctly identified positive and negative
signs.

## Usage

``` r
sign_recall(truth, estim)
```

## Arguments

- truth:

  integer vector with values in \\\\-1, 0, 1\\\\

- estim:

  integer vector of same length with values in \\\\-1, 0, 1\\\\

## Value

Returns a scalar between 0 (minimum recall) and 1 (maximum recall), or
`NA` if all true signs equal 0.

## Examples

``` r
truth <- sample(x = c(-1L, 0L, 1L), size = 10L, replace = TRUE)
estim <- sample(x = c(-1L, 0L, 1L), size = 10L, replace = TRUE)
sign_recall(truth = truth, estim = estim) # observed value
#> [1] 0.25
sign_recall(truth = truth, estim = 0L * estim) # lower limit 0
#> [1] 0
sign_recall(truth = truth, estim = truth) # upper limit 1
#> [1] 1
sign_recall(truth = 0L * truth, estim = estim) # not defined
#> [1] NA
```
