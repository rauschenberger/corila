# Residuals

Residuals

## Usage

``` r
# S3 method for class 'cv.corila'
residuals(object, ...)
```

## Arguments

- object:

  object of class `"cv.corila"`

- ...:

  (for compatibility with
  [stats::residuals](https://rdrr.io/r/stats/residuals.html))

## Value

Returns a numeric vector of length \\n_0\\ (one entry for each training
observation).

## Details

This function extracts the observed and fitted values from the fitted
model and calls the internal function
[`.residuals()`](https://rauschenberger.github.io/corila/reference/residuals.md)
to calculate the residuals.

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
#>   0.1878071  -0.3230415   0.0000000   0.0000000   0.0000000   0.0000000 
#>        <NA>        <NA>        <NA>        <NA>        <NA>        <NA> 
#>   0.0000000   0.0000000   0.0000000   0.0000000   0.0000000   0.0000000 
#>        <NA>        <NA>        <NA>        <NA>        <NA>        <NA> 
#>   0.0000000   0.0000000   0.0000000   0.0000000   0.0000000   0.0000000 
#>        <NA>        <NA>        <NA> 
#>   0.0000000   0.0000000   0.0000000 
predict(object, newx = x)
#>  [1]  0.18029153  0.34256424  0.18282212 -0.32680091  0.17072728  0.33165797
#>  [7]  0.09103962 -0.36409979  0.33019865  0.52958048
fitted(object)
#>  [1]  0.18029153  0.34256424  0.18282212 -0.32680091  0.17072728  0.33165797
#>  [7]  0.09103962 -0.36409979  0.33019865  0.52958048
residuals(object)
#>  [1] -0.9013482  0.6671411  0.6358414 -0.1569692 -0.1585298  1.5830001
#>  [7]  0.2693328 -0.9852950 -0.3180567 -0.6351166
plot(object)

print(object)
#> object of class ‘cv.corila’ 
#> (contains multiple objects of class ‘cv.glmnet’)
#> selected 1 from 20 predictors
summary(object)
#> --- object of class “cv.corila” --- 
#> generalised linear model with gaussian family 
#> 20 features (10 primary and 10 auxiliary features)
#> initial coefficients: ridge regression 
#> final coefficients: adaptive lasso regression 
#> optimised regularisation parameter: lambda.min = 1.24 
#> selected weights: local = 1, global = 0
#> selected exponents: local = 0, global = Inf
#> 2 non-zero coefficients (including intercept)
```
