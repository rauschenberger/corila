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
#>  [1] 0.6166870 0.3296107 0.6481065 0.2820941 0.2220718 0.5723502 0.3909569
#>  [8] 0.5002763 0.5185768 0.3567448
.mean_function(x, family = "poisson")
#>  [1] 1.6088337 0.4916705 1.8417687 0.3929403 0.2854657 1.3383617 0.6419198
#>  [8] 1.0011060 1.0771744 0.5545929
```
