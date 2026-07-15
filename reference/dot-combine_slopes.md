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
p <- 10
alpha <- rnorm(1)
temp <- rnorm(p)
beta <- pmax(c(temp, -temp), 0)
corila:::.combine_slopes(alpha = alpha, beta = beta)
#>  [1] -0.4874282  1.1858346 -1.0706625 -0.4342590 -1.9739971 -1.0363519
#>  [7] -1.1107092  0.2066082  0.3540105 -1.0114184  1.2174819
```
