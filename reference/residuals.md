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
#>  [1] -0.5492225 -2.6731641 -0.7235023  3.0783580 -1.0374305 -0.3982712
#>  [7] -0.4735381 -1.5642441 -0.2259049  1.6022490

y <- stats::rbinom(n = n, size = 1L, prob = 0.2)
y_hat <- stats::runif(n = n)
.residuals(y = y, y_hat = y_hat, family = "binomial")
#>  [1] -2.6753965 -2.5203875 -1.2596784  0.1755230 -0.9257565 -0.9713595
#>  [7]  1.3166702 -0.8150338 -0.3710410  2.2486072

y <- stats::rpois(n = n, lambda = 4.0)
y_hat <- stats::rexp(n = n, rate = 0.25)
.residuals(y = y, y_hat = y_hat, family = "poisson")
#>  [1]  4.3307640  4.2013170  3.4253125 -0.3862833 -1.1175292  2.2847043
#>  [7]  2.3351363  2.5649349  2.1201348  1.7236492
```
