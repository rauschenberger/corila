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

## Examples

``` r
p <- 10
alpha <- rnorm(1)
temp <- rnorm(p)
beta <- pmax(c(temp, -temp), 0)
corila:::.combine_slopes(alpha = alpha, beta = beta)
#>  [1] -1.9669122 -1.1236407 -0.9037849 -0.1674562  0.8707123  0.4821105
#>  [7]  0.3937619 -0.1506551 -1.2089154  1.1205360 -0.1224579
```
