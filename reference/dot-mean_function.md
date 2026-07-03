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
#>  [1] 0.4067475 0.2289361 0.4001881 0.6075639 0.4434902 0.5189876 0.3599993
#>  [8] 0.7475629 0.2129885 0.1911145
corila:::.mean_function(x, family = "poisson")
#>  [1] 0.6856230 0.2969094 0.6671892 1.5481857 0.7969137 1.0789486 0.5624983
#>  [8] 2.9613825 0.2706295 0.2362689
```
