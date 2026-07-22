# Simulate predictors

Simulates predictor matrix.

## Usage

``` r
.simulate_predictors(n, p = NULL, group = NULL, rho = 0, seed = 1L)
```

## Arguments

- n:

  number of observations: positive integer (minimum 1, maximum
  \\110\\000\\)

- p:

  number of predictors: positive integer scalar (minimum 1 leads to a
  single predictor, maximum \\1\\000\\)

- group:

  group indicator: integer vector of length \\p\\ with entries between 1
  and \\q\\, where \\p\\ is the number of predictors and \\q\\ is the
  number of predictor groups (maximum length \\1\\000\\, minimum entry
  1, maximum entry \\1\\000\\)

- rho:

  correlation coefficient for predictors within the same group: numeric
  scalar in the unit interval (minimum 0 leads to uncorrelated
  predictors within each group, maximum 1 leads to identical predictors
  within each group)

- seed:

  random seed for reproducibility: integer scalar (unrestricted)

## Value

Returns a numeric matrix with \\n\\ rows (observations) and \\p\\
columns (predictors).

## See also

This function is called by
[`simulate_data()`](https://rauschenberger.github.io/corila/reference/simulate_data.md).

## Examples

``` r
.simulate_predictors(n = 5L, p = 7L)
#>             [,1]        [,2]        [,3]        [,4]       [,5]       [,6]
#> [1,]  1.35867955 -0.05612874  0.91897737 -0.04493361  1.5117812 -0.8204684
#> [2,] -0.10278773 -0.15579551  0.78213630 -0.01619026  0.3898432  0.4874291
#> [3,]  0.38767161 -1.47075238  0.07456498  0.94383621 -0.6212406  0.7383247
#> [4,] -0.05380504 -0.47815006 -1.98935170  0.82122120 -2.2146999  0.5757814
#> [5,] -1.37705956  0.41794156  0.61982575  0.59390132  1.1249309 -0.3053884
#>            [,7]
#> [1,] -0.6264538
#> [2,]  0.1836433
#> [3,] -0.8356286
#> [4,]  1.5952808
#> [5,]  0.3295078
.simulate_predictors(n = 5L, group = rep(c(1L, 2L), each = 3L), rho = 1.0)
#>            [,1]       [,2]       [,3]       [,4]       [,5]       [,6]
#> [1,]  0.6264538  0.6264538  0.6264538  0.8204684  0.8204684  0.8204684
#> [2,] -0.1836433 -0.1836433 -0.1836433 -0.4874290 -0.4874291 -0.4874291
#> [3,]  0.8356286  0.8356286  0.8356286 -0.7383247 -0.7383247 -0.7383247
#> [4,] -1.5952808 -1.5952808 -1.5952808 -0.5757814 -0.5757813 -0.5757813
#> [5,] -0.3295078 -0.3295078 -0.3295078  0.3053884  0.3053884  0.3053884
```
