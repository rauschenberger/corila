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

  character `"gaussian"`, `"binomial"`, `"poisson"`, or `"cox"`

## Details

This function is called by
[`residuals.cv.corila()`](https://rauschenberger.github.io/corila/reference/residuals.cv.corila.md).

## Examples

``` r
n <- 10
y_obs <- stats::rbinom(n = n, size = 1, prob = 0.2)
y_fit <- stats::runif(n = n)
corila:::.residuals(y_obs = y_obs, y_fit = y_fit, family = "binomial")
#>  [1] -1.0423095 -0.7213308 -1.2142415 -0.8977264 -0.7093122 -0.8803970
#>  [7] -0.7449365 -1.0567336 -0.7809023 -1.4377581
```
