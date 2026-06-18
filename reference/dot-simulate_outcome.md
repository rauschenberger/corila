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
#>  [1]  0.14457084 -0.91107471  0.48129531  0.43516490 -0.04162846 -0.20415504
#>  [7]  0.05402925 -0.76406361 -0.69892937 -0.63471855

# simulate dependent outcome
n <- 10
p <- 20
x <- matrix(rnorm(n * p), n, p)
beta <- rnorm(p)
corila:::.simulate_outcome(family = "gaussian", x = x, beta = beta)
#>  [1] -0.6618132 -0.1856400 -1.3569514 -1.2114130 -2.2969133  0.3021709
#>  [7]  2.3079562  1.8628679 -1.4648566  1.8779476
```
