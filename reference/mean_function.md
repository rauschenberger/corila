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
#>  [1] 0.6092941 0.5029823 0.4976800 0.4249764 0.6206611 0.3537217 0.3357534
#>  [8] 0.5711972 0.5413023 0.3011024
.mean_function(x, family = "poisson")
#>  [1] 1.5594697 1.0120008 0.9907629 0.7390590 1.6361649 0.5473211 0.5054650
#>  [8] 1.3320741 1.1800849 0.4308247
```
