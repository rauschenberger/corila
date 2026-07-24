# Deviance Residuals

Calculates the deviance residuals.

## Usage

``` r
.residuals(y_obs, y_fit, family)
```

## Arguments

- y_obs:

  \\n\\-dimensional vector of observed values

- y_fit:

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

y_obs <- stats::rnorm(n = n)
y_fit <- stats::rnorm(n = n)
.residuals(y_obs = y_obs, y_fit = y_fit, family = "gaussian")
#> Error in .residuals(y_obs = y_obs, y_fit = y_fit, family = "gaussian"): unused arguments (y_obs = y_obs, y_fit = y_fit)

y_obs <- stats::rbinom(n = n, size = 1L, prob = 0.2)
y_fit <- stats::runif(n = n)
.residuals(y_obs = y_obs, y_fit = y_fit, family = "binomial")
#> Error in .residuals(y_obs = y_obs, y_fit = y_fit, family = "binomial"): unused arguments (y_obs = y_obs, y_fit = y_fit)

y_obs <- stats::rpois(n = n, lambda = 4.0)
y_fit <- stats::rexp(n = n, rate = 0.25)
.residuals(y_obs = y_obs, y_fit = y_fit, family = "poisson")
#> Error in .residuals(y_obs = y_obs, y_fit = y_fit, family = "poisson"): unused arguments (y_obs = y_obs, y_fit = y_fit)
```
