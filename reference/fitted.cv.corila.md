# Fitted values

Extracts fitted values.

## Usage

``` r
# S3 method for class 'cv.corila'
fitted(object, ...)
```

## Arguments

- object:

  object of class `"cv.corila"`

- ...:

  (for compatibility with
  [stats::fitted](https://rdrr.io/r/stats/fitted.values.html))

## Value

Returns a numeric vector of length \\n_0\\ (one entry for each training
observation).

## See also

Use
[predict()](https://rauschenberger.github.io/corila/reference/predict.cv.corila.md)
to obtain predicted values (i.e., for testing observations).

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
#>  0.06401393  0.00000000  0.00000000  0.00000000  0.00000000  0.00000000 
#>        <NA>        <NA>        <NA>        <NA>        <NA>        <NA> 
#>  0.00000000  0.00000000  0.00000000  0.00000000  0.00000000  0.00000000 
#>        <NA>        <NA>        <NA>        <NA>        <NA>        <NA> 
#>  0.00000000  0.00000000  0.00000000  0.00000000  0.00000000  0.00000000 
#>        <NA>        <NA>        <NA> 
#>  0.00000000  0.00000000  0.00000000 
predict(object, newx = x)
#>  [1] 0.06401393 0.06401393 0.06401393 0.06401393 0.06401393 0.06401393
#>  [7] 0.06401393 0.06401393 0.06401393 0.06401393
fitted(object)
#>  [1] 0.06401393 0.06401393 0.06401393 0.06401393 0.06401393 0.06401393
#>  [7] 0.06401393 0.06401393 0.06401393 0.06401393
residuals(object)
#>  [1] -0.4899953  0.9326448  0.6636468 -1.7906445  0.2893846  0.6627997
#>  [7]  0.6042470 -2.4883312 -0.2993714  1.9156194
plot(object)

print(object)
#> object of class ‘cv.corila’ 
#> (contains multiple objects of class ‘cv.glmnet’)
#> selected 0 from 20 predictors
summary(object)
#> --- object of class “cv.corila” --- 
#> generalised linear model with gaussian family 
#> 20 features (10 primary and 10 auxiliary features)
#> initial coefficients: ridge regression 
#> final coefficients: adaptive lasso regression 
#> optimised regularisation parameter: lambda.min = 1.166 
#> selected weights: local = 1, global = 0
#> selected exponents: local = 0, global = Inf
#> 1 non-zero coefficients (including intercept)
```
