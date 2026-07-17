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
#>  [1]  0.9350058  1.4724596 -0.6337288  1.6796832  2.7308100  0.5235084
#>  [7] -1.6578061 -0.5071186  1.0225506  0.1845635

y_obs <- stats::rbinom(n = n, size = 1L, prob = 0.2)
y_fit <- stats::runif(n = n)
.residuals(y_obs = y_obs, y_fit = y_fit, family = "binomial")
#>  [1] -1.3798325 -0.7243609 -0.5710542 -0.7056555 -1.7914253  0.9822467
#>  [7] -1.3669939 -0.5981388 -1.0471899 -1.1286798

y_obs <- stats::rpois(n = n, lambda = 4.0)
y_fit <- stats::rexp(n = n, rate = 0.25)
.residuals(y_obs = y_obs, y_fit = y_fit, family = "poisson")
#>  [1] -2.8210408 -1.7191617  3.8927327  5.6934177 -1.0755031  7.7879500
#>  [7]  3.0782055 -4.4910601 -1.8671604  0.6423464
```
