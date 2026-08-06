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
#>  [1] 0.3987606 0.5251807 0.7151961 0.8114708 0.4020712 0.3815242 0.4215974
#>  [8] 0.7702996 0.3201722 0.3431547
.mean_function(x, family = "poisson")
#>  [1] 0.6632311 1.1060646 2.5111874 4.3042169 0.6724399 0.6168781 0.7288997
#>  [8] 3.3534979 0.4709607 0.5224284
```
