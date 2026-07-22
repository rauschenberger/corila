# Mean function

Transform the linear predictor to predicted values/probabilities.

## Usage

``` r
.mean_function(x, family)
```

## Arguments

- x:

  numeric vector of length \\n\\

- family:

  character string `"gaussian"`, `"binomial"`, `"poisson"`, or `"cox"`

## Value

Returns a numeric vector of length \\n\\.

## Examples

``` r
x <- rnorm(n = 10L)
.mean_function(x, family = "binomial")
#>  [1] 0.5012784 0.6880513 0.3628093 0.5532538 0.7265290 0.4417258 0.7002861
#>  [8] 0.6579047 0.1693037 0.5767444
.mean_function(x, family = "poisson")
#>  [1] 1.0051266 2.2056555 0.5693889 1.2384074 2.6566945 0.7912343 2.3365151
#>  [8] 1.9231624 0.2038094 1.3626388
```
