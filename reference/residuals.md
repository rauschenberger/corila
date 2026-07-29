# Deviance Residuals

Calculates the deviance residuals.

## Usage

``` r
.residuals(y, y_hat, family)
```

## Arguments

- y:

  \\n_0\\-dimensional vector of observed values

- y_hat:

  \\n_0\\-dimensional vector of fitted values or probabilities

- family:

  character `"gaussian"`, `"binomial"`, or `"poisson"`

## Value

Returns an \\n_0\\-dimensional vector.

## Details

This function is called by
[`residuals.cv.corila()`](https://rauschenberger.github.io/corila/reference/residuals.cv.corila.md).

## Examples

``` r
n <- 10L

y <- stats::rnorm(n = n)
y_hat <- stats::rnorm(n = n)
.residuals(y = y, y_hat = y_hat, family = "gaussian")
#>  [1]  0.78975751 -0.08759941  0.36352758 -1.92828113 -0.28245617 -1.08557846
#>  [7]  0.87820576 -0.68048115  1.19346418  1.62444244

y <- stats::rbinom(n = n, size = 1L, prob = 0.2)
y_hat <- stats::runif(n = n)
.residuals(y = y, y_hat = y_hat, family = "binomial")
#>  [1] -1.5501738 -0.1446479 -1.1840541 -1.6206567 -0.8945908 -1.8534159
#>  [7] -2.0791134  0.5577054 -1.0988661 -1.1274718

y <- stats::rpois(n = n, lambda = 4.0)
y_hat <- stats::rexp(n = n, rate = 0.25)
.residuals(y = y, y_hat = y_hat, family = "poisson")
#>  [1] -1.331031  1.372616  1.757591  1.841133  2.893044 -1.484706 -2.201960
#>  [8] -1.998516  2.326685 -1.953178
```
