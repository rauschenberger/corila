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
#>  [1] 0.7165083 0.5437453 0.4470098 0.6609935 0.6961031 0.5091373 0.5240705
#>  [8] 0.6580698 0.8623511 0.5893710
corila:::.mean_function(x, family = "poisson")
#>  [1] 2.5274405 1.1917584 0.8083503 1.9497959 2.2905897 1.0372295 1.1011515
#>  [8] 1.9245734 6.2648577 1.4352884
```
