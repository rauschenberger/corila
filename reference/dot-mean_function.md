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
x <- rnorm(10)
corila:::.mean_function(x, family = "binomial")
#>  [1] 0.4714342 0.1733777 0.5582993 0.2540705 0.6483129 0.8169870 0.6826045
#>  [8] 0.6007864 0.1546458 0.4468054
corila:::.mean_function(x, family = "poisson")
#>  [1] 0.8919122 0.2097424 1.2639766 0.3406093 1.8434369 4.4640922 2.1506437
#>  [8] 1.5049249 0.1829360 0.8076822
```
