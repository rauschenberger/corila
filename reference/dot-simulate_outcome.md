# Simulate outcome

Simulates outcome vector.

## Usage

``` r
.simulate_outcome(family, x = NULL, beta = NULL, n = NULL, factor = 1)
```

## Arguments

- family:

  character `"gaussian"`, `"binomial"`, `"poisson"`, or `"cox"`

- x:

  numeric \\n \times p\\ matrix

- beta:

  numeric \\p\\-dimensional vector

- n:

  integer or `NULL`

- factor:

  non-negative scalar (default: `factor=1`) for multiplying the linear
  predictor (to increase or decrease the signal strength)

## Value

Returns an \\n\\-dimensional outcome vector.

## See also

Use
[`simulate()`](https://rauschenberger.github.io/corila/reference/simulate.md)
to simulate a predictor matrix, an effect vector, and an outcome vector.

## Examples

``` r
# simulate independent outcome
corila:::.simulate_outcome(family = "gaussian", n = 10)
#>  [1] -0.22223493  0.20199028  0.12426070  0.31747511  0.18880526  2.49608775
#>  [7]  1.00040265  0.01060549 -1.42152204 -1.30521156

# simulate dependent outcome
n <- 10
p <- 20
x <- matrix(rnorm(n * p), n, p)
beta <- rnorm(p)
corila:::.simulate_outcome(family = "gaussian", x = x, beta = beta)
#>  [1]  0.7249821 -0.2947392 -0.0939813  0.3663215  1.9130457  0.5715634
#>  [7]  0.5294206 -1.1678636  1.7544299 -1.1663178
```
