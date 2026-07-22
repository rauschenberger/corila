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
[`.residuals()`](https://rauschenberger.github.io/corila/reference/dot-residuals.md)
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
#>  (intercept)         <NA>         <NA>         <NA>         <NA>         <NA> 
#> -0.515475587  0.000000000  0.000000000  0.000000000  0.684670470  0.000000000 
#>         <NA>         <NA>         <NA>         <NA>         <NA>         <NA> 
#>  0.000000000  0.000000000  0.000000000  0.000000000  0.000000000  0.000000000 
#>         <NA>         <NA>         <NA>         <NA>         <NA>         <NA> 
#>  0.000000000  0.000000000  0.000000000  0.003283113  0.000000000  0.000000000 
#>         <NA>         <NA>         <NA> 
#>  0.000000000  0.000000000  0.000000000 
predict(object, newx = x)
#>  [1] -0.634177946  0.693666118  0.003064403  0.252989249 -1.150475708
#>  [6] -0.402590808  0.287708686 -0.557814814 -1.972376167 -0.283003936
fitted(object)
#>  [1] -0.634177946  0.693666118  0.003064403  0.252989249 -1.150475708
#>  [6] -0.402590808  0.287708686 -0.557814814 -1.972376167 -0.283003936
residuals(object)
#>  [1] -1.567604376 -0.301692374  0.493896549 -0.477863964  0.033332542
#>  [6]  0.007596205  1.262121657 -0.185699666 -0.359335951  1.095249378
plot(object)

print(object)
#> object of class ‘cv.corila’ 
#> (contains multiple objects of class ‘cv.glmnet’)
#> selected 2 from 20 predictors
summary(object)
#> --- object of class “cv.corila” --- 
#> generalised linear model with gaussian family 
#> 20 features (7 primary and 13 auxiliary features)
#> initial coefficients: ridge regression 
#> final coefficients: adaptive lasso regression 
#> optimised regularisation parameter: lambda.min = 1.019 
#> selected weights: local = 0, global = 1
#> selected exponents: local = Inf, global = 1
#> 3 non-zero coefficients (including intercept)
```
