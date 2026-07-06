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

# fitting the model
n <- 10; p <- 20; q <- 5
x <- matrix(rnorm(n * p), nrow = n , ncol = p)
y <- rnorm(n)
group <- rep(seq_len(q), length.out = p)
primary <- as.logical(rbinom(n = p, size = 1, prob = 0.5))
object <- cv.corila(x = x, y = y, group = group, primary = primary)
#> Warning: Option grouped=FALSE enforced in cv.glmnet, since < 3 observations per fold

# using S3 methods
coef(object)
#>  [1] -0.09030563  0.00000000  0.00000000  0.00000000  0.00000000  0.00000000
#>  [7]  0.00000000  0.00000000  0.00000000  0.00000000  0.00000000  0.00000000
#> [13]  0.00000000  0.00000000  0.00000000  0.00000000  0.00000000  0.00000000
#> [19]  0.00000000  0.00000000  0.00000000
predict(object, newx = x)
#>  [1] -0.09030563 -0.09030563 -0.09030563 -0.09030563 -0.09030563 -0.09030563
#>  [7] -0.09030563 -0.09030563 -0.09030563 -0.09030563
fitted(object)
#>  [1] -0.09030563 -0.09030563 -0.09030563 -0.09030563 -0.09030563 -0.09030563
#>  [7] -0.09030563 -0.09030563 -0.09030563 -0.09030563
residuals(object)
#>  [1] -1.34584008 -0.53895402  0.33382739  1.14866786  0.92165445  0.19551745
#>  [7] -1.65140747  0.73555262  0.18740985  0.01357193
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
#> optimised regularisation parameter: lambda.min = 1.137 
#> selected weights: local = 1, global = 0
#> selected exponents: local = 0, global = Inf
#> 1 non-zero coefficients (including intercept)
```
