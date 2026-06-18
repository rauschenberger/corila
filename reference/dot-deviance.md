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

## Examples

``` r
n <- 10

y <- rnorm(n)
y_hat <- rnorm(n)
corila:::.deviance(y = y , y_hat = y_hat, family = "gaussian")
#> [1] 1.41056

y <- rbinom(n = n, size = 1, prob = 0.5)
y_hat <- runif(n)
corila:::.deviance(y = y , y_hat = y_hat, family = "binomial")
#> [1] 1.093534

y <- rpois(n = n, lambda = 4)
y_hat <- rexp(n)
corila:::.deviance(y = y , y_hat = y_hat, family = "poisson")
#> [1] 6.062777
```
