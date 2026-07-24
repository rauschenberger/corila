# Deviance Residuals

Calculates the deviance residuals.

## Usage

``` r
.residuals(y, y_hat, family)
```

## Arguments

- y:

  \\n\\-dimensional vector of observed values

- y_hat:

  \\n\\-dimensional vector of fitted values or probabilities

- family:

  character `"gaussian"`, `"binomial"`, or `"poisson"`

## Value

Returns an \\n\\-dimensional vector.

## Details

This function is called by
[`residuals.cv.corila()`](https://rauschenberger.github.io/corila/reference/residuals.cv.corila.md).

## Examples

``` r
n <- 10L

y <- stats::rnorm(n = n)
y_hat <- stats::rnorm(n = n)
.residuals(y = y, y_hat = y_hat, family = "gaussian")
#>  [1]  2.72219304  2.08273331  4.06620885 -0.63673367  1.01550569  0.23718041
#>  [7]  1.74721013 -0.01598406 -1.85191712 -0.72359451

y <- stats::rbinom(n = n, size = 1L, prob = 0.2)
y_hat <- stats::runif(n = n)
.residuals(y = y, y_hat = y_hat, family = "binomial")
#>  [1] -1.1311203  0.5995055 -0.5089340 -1.5009462 -1.1717845 -2.1564864
#>  [7] -1.2659546 -0.5246211 -1.0801980 -0.6535468

y <- stats::rpois(n = n, lambda = 4.0)
y_hat <- stats::rexp(n = n, rate = 0.25)
.residuals(y = y, y_hat = y_hat, family = "poisson")
#>  [1]  3.0550014  0.1332344  3.9281638  0.8293250 -2.8022961  3.0161842
#>  [7] -0.8142865  0.5589854 -2.1815729  0.9211323
```
