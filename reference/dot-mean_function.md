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

  character `"gaussian"`, `"binomial"`, `"poisson"`, or `"cox"`

## Value

Returns a numeric vector of length \\n\\.

## Examples

``` r
x <- rnorm(10)
corila:::.mean_function(x, family = "binomial")
#>  [1] 0.7472769 0.5468746 0.7237533 0.4165108 0.3891689 0.3190786 0.4231659
#>  [8] 0.2235908 0.4117509 0.3817931
corila:::.mean_function(x, family = "poisson")
#>  [1] 2.9569001 1.2068944 2.6199524 0.7138276 0.6371139 0.4685982 0.7336008
#>  [8] 0.2879807 0.6999602 0.6175816
```
