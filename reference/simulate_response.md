# Simulate outcome

Simulates outcome vector.

## Usage

``` r
.simulate_response(family, x = NULL, beta = NULL, n = NULL, seed = 1L)
```

## Arguments

- family:

  character string `"gaussian"`, `"binomial"`, `"poisson"`, or `"cox"`

- x:

  predictors: numeric matrix with \\n\\ rows (observations) and \\p\\
  columns (predictors)

- beta:

  effects: numeric vector of length \\p\\

- n:

  sample size: positive integer scalar or `NULL` (minimum 1, maximum
  \\100\\000\\)

- seed:

  random seed for reproducibility: integer scalar (unrestricted)

## Value

Returns an \\n\\-dimensional response vector.

## See also

This function is called by
[`simulate_data()`](https://rauschenberger.github.io/corila/reference/simulate_data.md).

## Examples

``` r
# simulate independent response
.simulate_response(family = "gaussian", n = 10L)
#>  [1] -0.6264538  0.1836433 -0.8356286  1.5952808  0.3295078 -0.8204684
#>  [7]  0.4874291  0.7383247  0.5757814 -0.3053884

# simulate dependent response
set.seed(1L)
n <- 10L
p <- 20L
x <- matrix(rnorm(n * p), n, p)
beta <- rnorm(p)
.simulate_response(family = "gaussian", x = x, beta = beta)
#>  [1]   0.5803419   3.2072851  -4.3545695 -10.0067385   5.8595180   4.2391688
#>  [7]   0.4219104  -6.4163150  -1.3329548  -1.5614558
```
