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
n <- 10
y_obs <- stats::rbinom(n = n, size = 1, prob = 0.2)
y_fit <- stats::runif(n = n)
corila:::.residuals(y_obs = y_obs, y_fit = y_fit, family = "binomial")
#>  [1]  1.1421337  1.9220425 -1.6448521 -1.7580650 -1.2249872  0.8805749
#>  [7] -1.4093989 -0.5479869  1.0558925 -2.7001360
```
