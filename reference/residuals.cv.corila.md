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
#>   wgt_local exp_local wgt_global exp_global threshold      cvm
#> 5       0.4         0        0.6          2         0 2.885987

# using S3 methods
coef(object)
#> (intercept)        <NA>        <NA>        <NA>        <NA>        <NA> 
#>   0.1314233   0.0000000   0.0000000  -0.3703490   0.0000000   0.0000000 
#>        <NA>        <NA>        <NA>        <NA>        <NA>        <NA> 
#>   0.0000000   0.0000000   0.0000000   0.0000000   0.0000000   0.0000000 
#>        <NA>        <NA>        <NA>        <NA>        <NA>        <NA> 
#>  -0.2308594   0.0000000   0.0000000   0.0000000   0.0000000   0.0000000 
#>        <NA>        <NA>        <NA> 
#>   0.0000000   0.0000000  -0.6398561 
predict(object, newx = x)
#>  [1] -0.02159641  0.81992936 -0.26708138 -0.75205031  0.23008413  0.72817440
#>  [7] -1.43826926  0.35489118  1.26389884 -0.25248387
fitted(object)
#>  [1] -0.02159641  0.81992936 -0.26708138 -0.75205031  0.23008413  0.72817440
#>  [7] -1.43826926  0.35489118  1.26389884 -0.25248387
residuals(object)
#>  [1] -0.08393967  1.22013830 -0.05932453 -0.83556861  0.14858285  1.31602999
#>  [7] -0.42177644 -0.53272310 -0.85713063  0.10571184
plot(object)

print(object)
#> object of class ‘cv.corila’ 
#> (contains multiple objects of class ‘cv.glmnet’)
#> selected 3 from 20 predictors
summary(object)
#> --- object of class “cv.corila” --- 
#> generalised linear model with gaussian family 
#> 20 features (10 primary and 10 auxiliary features)
#> initial coefficients: ridge regression 
#> final coefficients: adaptive lasso regression 
#> optimised regularisation parameter: lambda.min = 0.3568 
#> selected weights: local = 0.4, global = 0.6
#> selected exponents: local = 0, global = 2
#> 4 non-zero coefficients (including intercept)
```
