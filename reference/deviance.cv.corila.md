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
[`glmnet::deviance.glmnet()`](https://glmnet.stanford.edu/reference/deviance.glmnet.html)
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
#>  [1] 0.3193801 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000
#>  [8] 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000
#> [15] 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000
predict(object, newx = x)
#>  [1] 0.3193801 0.3193801 0.3193801 0.3193801 0.3193801 0.3193801 0.3193801
#>  [8] 0.3193801 0.3193801 0.3193801
fitted(object)
#>  [1] 0.3193801 0.3193801 0.3193801 0.3193801 0.3193801 0.3193801 0.3193801
#>  [8] 0.3193801 0.3193801 0.3193801
residuals(object)
#>  [1] -0.03769785 -0.88136412  0.69344059  0.45409974 -0.68050515  0.81391340
#>  [7]  0.53959657 -0.16148370 -0.47280549 -0.26719399
plot(object)

print(object)
#> object of class ‘cv.corila’ 
#> (contains multiple objects of class ‘cv.glmnet’)
#> selected 0 from 20 predictors
summary(object)
#> --- object of class “cv.corila” --- 
#> generalised linear model with gaussian family 
#> 20 features (12 primary and 8 auxiliary features)
#> initial coefficients: ridge regression 
#> final coefficients: adaptive lasso regression 
#> optimised regularisation parameter: lambda.min = 0.9783 
#> selected weights: local = 0, global = 1
#> selected exponents: local = Inf, global = 1
#> 1 non-zero coefficients (including intercept)
```
