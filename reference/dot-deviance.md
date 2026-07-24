# Deviance

Calculates the deviance.

## Usage

``` r
.deviance(y, y_hat, family)
```

## Arguments

- y:

  response vector of length \\n_0\\, containing numerical values
  (`family="gaussian"`), integer values (`family="poisson"`), binary
  values (`family="binomial"`), or a survival object created with
  [`survival::Surv()`](https://rdrr.io/pkg/survival/man/Surv.html)
  (`family="cox"`), where \\n_0\\ is the number of observations used for
  model training

- y_hat:

  predicted response: numeric vector of length \\n\\, with entries on
  the real range (`family="gaussian"` or `family="cox"`), in the unit
  interval (`family="binomial"`), or on the non-negative real range
  (`family="poisson"`)

- family:

  character string `"gaussian"`, `"binomial"`, `"poisson"`, or `"cox"`

## Value

Returns the deviance (a numeric scalar).

## See also

The function
[`deviance.cv.corila()`](https://rauschenberger.github.io/corila/reference/deviance.cv.corila.md)
extracts the deviance from a fitted model.

## Examples

``` r
n <- 10L

y <- rnorm(n)
y_hat <- rnorm(n)
.deviance(y = y, y_hat = y_hat, family = "gaussian")
#> [1] 4.997531

y <- rbinom(n = n, size = 1L, prob = 0.5)
y_hat <- runif(n)
.deviance(y = y, y_hat = y_hat, family = "binomial")
#> [1] 1.435059

y <- rpois(n = n, lambda = 4.0)
y_hat <- rexp(n)
.deviance(y = y, y_hat = y_hat, family = "poisson")
#> [1] 9.967484
```
