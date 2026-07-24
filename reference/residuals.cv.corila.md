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
#>   0.3447973   0.0000000   0.0000000   0.0000000   0.0000000   0.0000000 
#>        <NA>        <NA>        <NA>        <NA>        <NA>        <NA> 
#>   0.0000000   0.0000000   0.0000000   0.0000000   0.0000000   0.0000000 
#>        <NA>        <NA>        <NA>        <NA>        <NA>        <NA> 
#>   0.0000000   0.0000000   0.0000000   0.0000000   0.0000000   0.0000000 
#>        <NA>        <NA>        <NA> 
#>   0.0000000   0.0000000   0.0000000 
predict(object, newx = x)
#>  [1] 0.3447973 0.3447973 0.3447973 0.3447973 0.3447973 0.3447973 0.3447973
#>  [8] 0.3447973 0.3447973 0.3447973
fitted(object)
#>  [1] 0.3447973 0.3447973 0.3447973 0.3447973 0.3447973 0.3447973 0.3447973
#>  [8] 0.3447973 0.3447973 0.3447973
residuals(object)
#>  [1] -0.05713017 -1.85019844  1.17449970  0.02261204  1.35506510  0.29939967
#>  [7] -2.03259834  0.30284868  0.10399691  0.68150485
plot(object)

print(object)
#> object of class ‘cv.corila’ 
#> (contains multiple objects of class ‘cv.glmnet’)
#> selected 0 from 20 predictors
summary(object)
#> --- object of class “cv.corila” --- 
#> generalised linear model with gaussian family 
#> 20 features (9 primary and 11 auxiliary features)
#> initial coefficients: ridge regression 
#> final coefficients: adaptive lasso regression 
#> optimised regularisation parameter: lambda.min = 1.277 
#> selected weights: local = 1, global = 0
#> selected exponents: local = 0, global = Inf
#> 1 non-zero coefficients (including intercept)
```
