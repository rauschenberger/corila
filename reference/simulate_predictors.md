# Simulate predictors

Simulates predictor matrix.

## Usage

``` r
.simulate_predictors(
  n,
  p = NULL,
  group = NULL,
  rho_within = 0,
  rho_between = 0,
  seed = 1L
)
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

- rho_within:

  correlation coefficient for predictors within the same group: positive
  numeric scalar in the unit interval (minimum 0 leads to uncorrelated
  predictors within each group, maximum 1 leads to identical predictors
  within each group)

- rho_between:

  correlation coefficient for predictors in different groups: positive
  numeric scalar in the unit interval (minimum 0 leads to uncorrelated
  predictors between groups, maximum `rho_within` leads to same
  correlation between and within groups)

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
.simulate_predictors(n = 5L, group = rep(c(1L, 2L), each = 3L),
                     rho_within = 0.5, rho_between = 0.2)
#>            [,1]        [,2]        [,3]       [,4]        [,5]       [,6]
#> [1,]  1.0662725  1.39458262 -0.03473632  0.2054373  0.36454078 -0.5218025
#> [2,] -0.3669381 -0.02765002 -0.67443015  0.4352125  0.07786598 -0.1693932
#> [3,] -0.7265088  0.88402641  0.42278252  0.8067964  0.54929905  1.3640730
#> [4,] -2.0109892 -1.91334934 -0.06047507 -1.5443969 -1.27242568  0.5007818
#> [5,]  0.3727967  0.33846546 -0.91943801 -0.3312861 -0.34621180 -0.4157776
```
