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
#>  [1] -1.0225909  2.5957718  0.3031988  0.9087474  0.2078499  0.1780140
#>  [7] -0.1657650  0.5571036  1.4443344  0.9013571 -0.2220350
```
