# Deviance

Calculates the deviance.

## Usage

``` r
# S3 method for class 'cv.corila'
deviance(object, ...)
```

## Arguments

- object:

  object of class `"cv.corila"`

- ...:

  (for compatibility with
  [stats::deviance](https://rdrr.io/r/stats/deviance.html))

## Value

Returns a scalar.

## Details

Returns the deviance calculated by
[`glmnet::deviance.glmnet()`](https://glmnet.stanford.edu/reference/deviance.glmnet.html)
for the model with the optimised mixing and regularisation
hyperparameters.

## See also

The internal function
[`.deviance()`](https://rauschenberger.github.io/corila/reference/deviance.md)
calculates the deviance from fitted and observed values.

## Examples

``` r
# listing S3 methods
methods(class = "cv.corila")
#> [1] coef      deviance  fitted    nobs      plot      predict   print    
#> [8] residuals summary  
#> see '?methods' for accessing help and source code

# simulating data
n <- 10L; p <- 20L; q <- 5L
x <- matrix(rnorm(n * p), nrow = n , ncol = p)
y <- rnorm(n)
group <- rep(seq_len(q), length.out = p)
primary <- as.logical(rbinom(n = p, size = 1L, prob = 0.5))

# fitting the model
object <- cv.corila(x = x, y = y, group = group, primary = primary)
#> Warning: Option grouped=FALSE enforced in cv.glmnet, since < 3 observations per fold

# using S3 methods
coef(object)
#> (intercept)        <NA>        <NA>        <NA>        <NA>        <NA> 
#>  0.14865837  0.00000000  0.05360646  0.00000000  0.00000000  0.00000000 
#>        <NA>        <NA>        <NA>        <NA>        <NA>        <NA> 
#>  0.00000000  0.00000000  0.00000000  0.00000000  0.00000000  0.00000000 
#>        <NA>        <NA>        <NA>        <NA>        <NA>        <NA> 
#>  0.00000000  0.00000000  0.00000000  0.00000000  0.00000000  0.00000000 
#>        <NA>        <NA>        <NA> 
#>  0.00000000  0.00000000  0.00000000 
predict(object, newx = x)
#>  [1] 0.14328750 0.18686188 0.14471484 0.14664093 0.11211696 0.13127539
#>  [7] 0.15188335 0.11708982 0.17714999 0.06726264
fitted(object)
#>  [1] 0.14328750 0.18686188 0.14471484 0.14664093 0.11211696 0.13127539
#>  [7] 0.15188335 0.11708982 0.17714999 0.06726264
residuals(object)
#>  [1] -0.1792099  0.8822996 -0.6286898 -0.2676510 -1.4062570  0.3630375
#>  [7]  1.1560182  1.3799512  0.6375527 -1.9370514
plot(object)

print(object)
#> object of class ‘cv.corila’ 
#> (contains multiple objects of class ‘cv.glmnet’)
#> selected 1 from 20 predictors
summary(object)
#> --- object of class “cv.corila” --- 
#> generalised linear model with gaussian family 
#> 20 features (12 primary and 8 auxiliary features)
#> initial coefficients: ridge regression 
#> final coefficients: adaptive lasso regression 
#> optimised regularisation parameter: lambda.min = 5.061 
#> selected weights: local = 1, global = 0
#> selected exponents: local = 0, global = Inf
#> 2 non-zero coefficients (including intercept)
```
