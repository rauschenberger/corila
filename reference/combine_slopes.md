# Combine coefficients

Combine estimated coefficients for positive effects and estimated
coefficients for negative effects.

## Usage

``` r
.combine_slopes(alpha, beta)
```

## Arguments

- alpha:

  estimated intercept: scalar

- beta:

  estimated slopes: numeric vector of length \\2 \* p\\ with
  non-negative entries, namely of \\p\\ estimated coefficients for
  positive effects and \\p\\ estimated coefficients for negative effects

## Value

Returns a numeric vector of length \\1 + p\\.

## See also

This function is called by
[coef()](https://rauschenberger.github.io/corila/reference/coef.cv.corila.md).

## Examples

``` r
p <- 10L
alpha <- rnorm(1L)
temp <- rnorm(p)
beta <- pmax(c(temp, -temp), 0.0)
.combine_slopes(alpha = alpha, beta = beta)
#>  [1] -0.1162478  1.8173120  0.3706279  0.5202165 -0.7505320  0.8168998
#>  [7] -0.8863575 -0.3315776  1.1207127  0.2987237  0.7796219
```
