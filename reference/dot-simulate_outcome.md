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
#>  [1]  0.9631562 -0.3371137 -0.4508069 -0.7580095 -0.3097903 -1.2448618
#>  [7] -0.3567317 -0.4819441  0.1445708 -0.9110747

# simulate dependent outcome
n <- 10
p <- 20
x <- matrix(rnorm(n * p), n, p)
beta <- rnorm(p)
corila:::.simulate_outcome(family = "gaussian", x = x, beta = beta)
#>  [1] -1.3973937  0.4946690  1.2838288  1.6280967  1.3014228 -1.6888260
#>  [7] -1.0981675 -3.6495604 -0.1098804 -0.3896175
```
