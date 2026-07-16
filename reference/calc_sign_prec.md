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
