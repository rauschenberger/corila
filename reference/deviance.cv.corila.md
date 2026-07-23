# Deviance

Calculates the deviance.

## Usage

``` r
# S3 method for class 'cv.corila'
deviance(object, ...)
```

## Arguments

- object:

  object of class `"cv.corila"`

- ...:

  (for compatibility with
  [stats::deviance](https://rdrr.io/r/stats/deviance.html))

## Value

Returns a scalar.

## Details

Returns the deviance calculated by
[`glmnet::deviance.glmnet()`](https://rdrr.io/pkg/glmnet/man/deviance.glmnet.html)
for the model with the optimised mixing and regularisation
hyperparameters.

## See also

The internal function
[`.deviance()`](https://rauschenberger.github.io/corila/reference/dot-deviance.md)
calculates the deviance from fitted and observed values.

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
#>  0.10235956  0.00000000  0.00000000  0.00000000  0.00000000 -0.20630263 
#>        <NA>        <NA>        <NA>        <NA>        <NA>        <NA> 
#>  0.00000000  0.00000000  0.00000000  0.00000000  0.00000000  0.00000000 
#>        <NA>        <NA>        <NA>        <NA>        <NA>        <NA> 
#>  0.00000000  0.00000000  0.00000000  0.00000000  0.00000000  0.00000000 
#>        <NA>        <NA>        <NA> 
#>  0.00000000  0.01934724  0.00000000 
predict(object, newx = x)
#>  [1]  0.236149175  0.205475646 -0.194471656  0.231808305  0.158599736
#>  [6]  0.205573215  0.121881660  0.171022293  0.007657089  0.130715548
fitted(object)
#>  [1]  0.236149175  0.205475646 -0.194471656  0.231808305  0.158599736
#>  [6]  0.205573215  0.121881660  0.171022293  0.007657089  0.130715548
residuals(object)
#>  [1] -0.49508176  0.18890352 -0.65738544  2.41735858 -0.00258806  0.92463405
#>  [7] -2.41100564  0.56997886 -1.32390225  0.78908813
plot(object)

print(object)
#> object of class ‘cv.corila’ 
#> (contains multiple objects of class ‘cv.glmnet’)
#> selected 2 from 20 predictors
summary(object)
#> --- object of class “cv.corila” --- 
#> generalised linear model with gaussian family 
#> 20 features (11 primary and 9 auxiliary features)
#> initial coefficients: ridge regression 
#> final coefficients: adaptive lasso regression 
#> optimised regularisation parameter: lambda.min = 0.6963 
#> selected weights: local = 0, global = 1
#> selected exponents: local = Inf, global = 1
#> 3 non-zero coefficients (including intercept)
```
