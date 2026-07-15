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
#>  [1] 0.03385926 0.00000000 0.00000000 0.00000000 0.00000000 0.00000000
#>  [7] 0.00000000 0.00000000 0.00000000 0.00000000 0.00000000 0.00000000
#> [13] 0.00000000 0.00000000 0.00000000 0.00000000 0.00000000 0.00000000
#> [19] 0.00000000 0.00000000 0.00000000
predict(object, newx = x)
#>  [1] 0.03385926 0.03385926 0.03385926 0.03385926 0.03385926 0.03385926
#>  [7] 0.03385926 0.03385926 0.03385926 0.03385926
fitted(object)
#>  [1] 0.03385926 0.03385926 0.03385926 0.03385926 0.03385926 0.03385926
#>  [7] 0.03385926 0.03385926 0.03385926 0.03385926
residuals(object)
#>  [1] -1.0203984 -0.1297852  0.1355968  0.2877109 -0.4928061 -0.6170884
#>  [7] -0.8932443  1.7304278  0.3405536  0.6590333
plot(object)

print(object)
#> object of class ‘cv.corila’ 
#> (contains multiple objects of class ‘cv.glmnet’)
#> selected 0 from 20 predictors
summary(object)
#> --- object of class “cv.corila” --- 
#> generalised linear model with gaussian family 
#> 20 features (11 primary and 9 auxiliary features)
#> initial coefficients: ridge regression 
#> final coefficients: adaptive lasso regression 
#> optimised regularisation parameter: lambda.min = 1.69 
#> selected weights: local = 1, global = 0
#> selected exponents: local = 0, global = Inf
#> 1 non-zero coefficients (including intercept)
```
