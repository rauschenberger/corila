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
#>  [1] -1.1008985 -0.4069483  2.3780376  1.7892905 -1.3401366  2.5894503
#>  [7]  1.9803386 -0.6420839 -1.6173356 -0.8766318
```
