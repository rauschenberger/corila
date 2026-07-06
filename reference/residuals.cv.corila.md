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
#>  [1] -0.267289240  0.000000000  0.063460819  0.126237831  0.000000000
#>  [6]  0.231509776  0.000000000  0.000000000  0.000000000 -0.005133078
#> [11]  0.000000000  0.000000000  0.000000000  0.000000000  0.000000000
#> [16]  0.000000000  0.000000000  0.000000000 -0.048436477  0.000000000
#> [21]  0.000000000
predict(object, newx = x)
#>  [1] -0.31692173 -0.62313040 -0.60808944  0.19913461 -0.28458860 -0.17309989
#>  [7] -0.21176436 -0.07275968  0.04145662 -0.89902673
fitted(object)
#>  [1] -0.31692173 -0.62313040 -0.60808944  0.19913461 -0.28458860 -0.17309989
#>  [7] -0.21176436 -0.07275968  0.04145662 -0.89902673
residuals(object)
#>  [1] -0.18065741  0.02210300  0.09589163 -0.20406699  0.18744069  0.14906449
#>  [7]  0.08437402 -0.05912594  0.41228563 -0.50730910
plot(object)

print(object)
#> object of class ‘cv.corila’ 
#> (contains multiple objects of class ‘cv.glmnet’)
#> selected 5 from 20 predictors
summary(object)
#> --- object of class “cv.corila” --- 
#> generalised linear model with gaussian family 
#> 20 features (12 primary and 8 auxiliary features)
#> initial coefficients: ridge regression 
#> final coefficients: adaptive lasso regression 
#> optimised regularisation parameter: lambda.min = 0.2669 
#> selected weights: local = 0.6, global = 0.4
#> selected exponents: local = 0, global = 1
#> 6 non-zero coefficients (including intercept)
```
