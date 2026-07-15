# Deviance

Calculates the deviance.

## Usage

``` r
.deviance(y, y_hat, family)
```

## Arguments

- y:

  response: numeric vector of length \\n\\

- y_hat:

  predicted response: numeric vector of length \\n\\

- family:

  character

## Value

Returns the deviance (a numeric scalar).

## See also

The function
[`deviance.cv.corila()`](https://rauschenberger.github.io/corila/reference/deviance.cv.corila.md)
extracts the deviance from a fitted model.

## Examples

``` r
n <- 10

y <- rnorm(n)
y_hat <- rnorm(n)
corila:::.deviance(y = y , y_hat = y_hat, family = "gaussian")
#> [1] 5.761124

y <- rbinom(n = n, size = 1, prob = 0.5)
y_hat <- runif(n)
corila:::.deviance(y = y , y_hat = y_hat, family = "binomial")
#> [1] 1.444421

y <- rpois(n = n, lambda = 4)
y_hat <- rexp(n)
corila:::.deviance(y = y , y_hat = y_hat, family = "poisson")
#> [1] 9.178619
```
