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
#>  [1] 0.4468054 0.3459346 0.2425051 0.9488641 0.5663275 0.5220676 0.1661518
#>  [8] 0.6451374 0.8100362 0.8303441
corila:::.mean_function(x, family = "poisson")
#>  [1]  0.8076822  0.5288990  0.3201410 18.5557460  1.3058876  1.0923463
#>  [7]  0.1992591  1.8179923  4.2641607  4.8942839
```
