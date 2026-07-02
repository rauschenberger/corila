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
#>  [1]  1.3448172  2.2014934 -1.1121960 -1.3176498 -2.1962678  1.7323795
#>  [7]  0.5474234 -0.8280820 -1.3074263 -1.2162960
```
