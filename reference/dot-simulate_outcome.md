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
#>  [1]  0.52362315 -0.01973839  0.35955249  2.86940836  0.05236355  0.64790447
#>  [7]  0.06967448  0.33082434  0.18316304  0.24630478

# simulate dependent outcome
n <- 10
p <- 20
x <- matrix(rnorm(n * p), n, p)
beta <- rnorm(p)
corila:::.simulate_outcome(family = "gaussian", x = x, beta = beta)
#>  [1]  0.14431489  0.33879367 -3.09769902 -0.01120175  1.03561133 -1.54659602
#>  [7]  0.75059887 -1.22907389 -0.01890733 -0.91226085
```
