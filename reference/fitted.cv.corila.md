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
#>  [1]  0.2854464  0.0000000  0.0000000  0.0000000  0.0000000  0.5787971
#>  [7]  0.1614690  0.0000000  0.0000000  0.0000000  0.0000000  0.0000000
#> [13]  0.0000000 -0.1137071  0.0000000  0.4729298  0.0000000  0.3853406
#> [19] -0.6358562  0.0000000  0.2407236
predict(object, newx = x)
#>  [1] -0.23051325 -0.15702890 -0.15425913  0.61597916  0.04372087  1.75889599
#>  [7]  1.62855405  0.09542364 -0.19228939 -0.91483210
fitted(object)
#>  [1] -0.23051325 -0.15702890 -0.15425913  0.61597916  0.04372087  1.75889599
#>  [7]  1.62855405  0.09542364 -0.19228939 -0.91483210
residuals(object)
#>  [1] -0.12845860  0.01377974  0.21239739  0.02410230 -0.19036856  0.07229898
#>  [7]  0.02635743  0.11748997 -0.07589160 -0.07170707
plot(object)

print(object)
#> object of class ‘cv.corila’ 
#> (contains multiple objects of class ‘cv.glmnet’)
#> selected 7 from 20 predictors
summary(object)
#> --- object of class “cv.corila” --- 
#> generalised linear model with gaussian family 
#> 20 features (12 primary and 8 auxiliary features)
#> initial coefficients: ridge regression 
#> final coefficients: adaptive lasso regression 
#> optimised regularisation parameter: lambda.min = 0.01632 
#> selected weights: local = 0.3, global = 0.7
#> selected exponents: local = 0, global = 1
#> 8 non-zero coefficients (including intercept)
```
