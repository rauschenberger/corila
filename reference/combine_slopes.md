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
#>  [1]  0.5855288  0.7094660 -0.1093033 -0.4534972  0.6058875 -1.8179560
#>  [7]  0.6300986 -0.2761841 -0.2841597 -0.9193220 -0.1162478
```
