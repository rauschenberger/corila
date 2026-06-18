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
#>  [1]  0.3239434  0.7675508 -0.8118477  0.3877671 -0.6545560 -1.1769173
#>  [7] -0.4656670  1.9416875 -1.4177729 -2.0597286 -0.1076466
```
