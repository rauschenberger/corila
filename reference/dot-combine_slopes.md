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
#>  [1] -1.0363519 -1.1107092  0.2066082  0.3540105 -1.0114184  1.2174819
#>  [7]  0.7275355 -3.2210369  0.6559715  0.4356118 -0.5215687
```
