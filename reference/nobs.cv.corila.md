# Observation Count

Extracts the number of observations.

## Usage

``` r
# S3 method for class 'cv.corila'
nobs(object, ...)
```

## Arguments

- object:

  object of class `"cv.corila"`

- ...:

  (for compatibility with
  [stats::nobs](https://rdrr.io/r/stats/nobs.html))

## Value

Returns a positive integer.

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
#>   wgt_local exp_local wgt_global exp_global threshold      cvm
#> 9       0.8         0        0.2          2         0 3.430483

# using S3 methods
coef(object)
#> (intercept)        <NA>        <NA>        <NA>        <NA>        <NA> 
#>  -0.2193227   0.0000000   0.0000000   0.0000000   0.0000000   0.0000000 
#>        <NA>        <NA>        <NA>        <NA>        <NA>        <NA> 
#>   0.0000000   0.0000000   0.0000000   0.0000000   0.0000000   0.0000000 
#>        <NA>        <NA>        <NA>        <NA>        <NA>        <NA> 
#>   0.0000000   0.0000000  -0.1889409   0.0000000   0.0000000   0.0000000 
#>        <NA>        <NA>        <NA> 
#>   0.0000000   0.0000000   0.0000000 
predict(object, newx = x)
#>  [1] -0.003880185  0.025113013 -0.106959797  0.064242444 -0.222318506
#>  [6] -0.321382841  0.073024018 -0.379856906 -0.388616254 -0.245527119
fitted(object)
#>  [1] -0.003880185  0.025113013 -0.106959797  0.064242444 -0.222318506
#>  [6] -0.321382841  0.073024018 -0.379856906 -0.388616254 -0.245527119
residuals(object)
#>  [1]  0.9958309 -0.8843638 -0.1746199  2.0020048 -0.3892345  0.6369957
#>  [7]  0.5872694 -1.3423455 -1.7460098  0.3144727
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
#> optimised regularisation parameter: lambda.min = 0.8088 
#> selected weights: local = 0.8, global = 0.2
#> selected exponents: local = 0, global = 2
#> 2 non-zero coefficients (including intercept)
```
