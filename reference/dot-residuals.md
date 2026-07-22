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
#>  [1] -1.24328858  1.25657963 -0.02789375  0.89456946  1.86163087 -1.06507372
#>  [7]  0.51062526 -0.12689730 -1.15558302 -0.30580868

y_obs <- stats::rbinom(n = n, size = 1L, prob = 0.2)
y_fit <- stats::runif(n = n)
.residuals(y_obs = y_obs, y_fit = y_fit, family = "binomial")
#>  [1] -2.6959341 -2.8017948 -0.3885653 -1.1129692 -2.3112787 -0.5281688
#>  [7]  0.5512039 -1.2116173 -0.7128686  1.3043475

y_obs <- stats::rpois(n = n, lambda = 4.0)
y_fit <- stats::rexp(n = n, rate = 0.25)
.residuals(y_obs = y_obs, y_fit = y_fit, family = "poisson")
#>  [1]  4.25318804  2.49490550 -0.54468833 -0.52911019 -1.29655354 -0.02134148
#>  [7]  5.57425171 -0.44366331 -1.11588544  1.49851710
```
