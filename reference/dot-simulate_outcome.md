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

  positive integer or `NULL`

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
.simulate_outcome(family = "gaussian", n = 10)
#>  [1] -0.01630106  0.48985659 -0.08067527 -0.12074474 -1.00545767 -0.76637666
#>  [7] -0.41149538  0.21176267  0.22923896 -0.72947843

# simulate dependent outcome
n <- 10
p <- 20
x <- matrix(rnorm(n * p), n, p)
beta <- rnorm(p)
.simulate_outcome(family = "gaussian", x = x, beta = beta)
#>  [1] -1.90096148  1.12334430  0.76754954 -0.10134765 -1.33034655  0.06386416
#>  [7] -1.54369537 -1.43104691 -1.70874831  0.07181430
```
