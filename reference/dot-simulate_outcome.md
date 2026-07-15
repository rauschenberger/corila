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
corila:::.simulate_outcome(family = "gaussian", n = 10)
#>  [1]  0.246304780 -0.948836419  0.613990444 -1.320130677  0.138598367
#>  [6] -0.471144892  2.305710580  0.576793089 -0.691726313 -0.001400463

# simulate dependent outcome
n <- 10
p <- 20
x <- matrix(rnorm(n * p), n, p)
beta <- rnorm(p)
corila:::.simulate_outcome(family = "gaussian", x = x, beta = beta)
#>  [1] -1.94473471  0.40983288  0.53617522 -0.05408577 -1.19707525  0.29798117
#>  [7] -0.88099489  1.03116636 -0.30447646  1.22519758
```
