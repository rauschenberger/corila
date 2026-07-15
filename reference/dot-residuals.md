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

## Details

This function is called by
[`residuals.cv.corila()`](https://rauschenberger.github.io/corila/reference/residuals.cv.corila.md).

## Examples

``` r
n <- 10
y_obs <- stats::rbinom(n = n, size = 1, prob = 0.2)
y_fit <- stats::runif(n = n)
corila:::.residuals(y_obs = y_obs, y_fit = y_fit, family = "binomial")
#>  [1] -0.3311562 -1.7092747 -1.6068298 -0.5062574  0.3907324  0.6340327
#>  [7] -2.3999946 -1.8292354 -2.0643748 -2.3998834
```
