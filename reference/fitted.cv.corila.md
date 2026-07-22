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
#>   0.1162742   0.0000000   0.0000000   0.0000000  -1.0662153   0.0000000 
#>        <NA>        <NA>        <NA>        <NA>        <NA>        <NA> 
#>   0.0000000   0.0000000   0.2327205   0.0000000   0.0000000   0.0000000 
#>        <NA>        <NA>        <NA>        <NA>        <NA>        <NA> 
#>   0.0000000   0.0000000   0.0000000   0.0000000   0.0000000   0.0000000 
#>        <NA>        <NA>        <NA> 
#>   0.0000000   0.0000000   0.0000000 
predict(object, newx = x)
#>  [1]  0.15935091  0.35495346 -0.35267392 -0.83181384  0.98885950  0.94810074
#>  [7] -0.02504362 -0.77393724  0.32217038 -0.76101705
fitted(object)
#>  [1]  0.15935091  0.35495346 -0.35267392 -0.83181384  0.98885950  0.94810074
#>  [7] -0.02504362 -0.77393724  0.32217038 -0.76101705
residuals(object)
#>  [1] -0.323726743  0.065741183 -0.047572827 -0.538394041 -0.001021233
#>  [6]  0.571644289 -0.283696950 -0.479352513  0.320070923  0.716307910
plot(object)

print(object)
#> object of class ‘cv.corila’ 
#> (contains multiple objects of class ‘cv.glmnet’)
#> selected 2 from 20 predictors
summary(object)
#> --- object of class “cv.corila” --- 
#> generalised linear model with gaussian family 
#> 20 features (8 primary and 12 auxiliary features)
#> initial coefficients: ridge regression 
#> final coefficients: adaptive lasso regression 
#> optimised regularisation parameter: lambda.min = 0.4837 
#> selected weights: local = 0, global = 1
#> selected exponents: local = Inf, global = 1
#> 3 non-zero coefficients (including intercept)
```
