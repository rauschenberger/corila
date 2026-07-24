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
#>  [1]  0.475509529 -0.709946431  0.610726353 -0.934097632 -1.253633400
#>  [6]  0.291446236 -0.443291873  0.001105352  0.074341324 -0.589520946
#> [11] -0.568668733
```
