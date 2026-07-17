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
#>  [1]  0.1435740  0.0000000  0.0000000  0.0000000  0.0000000  0.0000000
#>  [7]  0.0000000  0.0000000  0.0000000  0.0000000  0.0000000  0.0000000
#> [13]  0.0000000  0.0000000 -0.1997738  0.0000000  0.0000000  0.0000000
#> [19]  0.0000000  0.0000000  0.0000000
predict(object, newx = x)
#>  [1]  0.25777848  0.16488811  0.28292914  0.10192787  0.37821762  0.04403382
#>  [7]  0.23439076 -0.09932211  0.26511080  0.19203287
fitted(object)
#>  [1]  0.25777848  0.16488811  0.28292914  0.10192787  0.37821762  0.04403382
#>  [7]  0.23439076 -0.09932211  0.26511080  0.19203287
residuals(object)
#>  [1] -0.8737269  0.6954060 -0.4406849 -0.7145216  0.5414637 -0.6102182
#>  [7]  0.5768959 -0.4819820  1.8509564 -0.5435885
plot(object)

print(object)
#> object of class ‘cv.corila’ 
#> (contains multiple objects of class ‘cv.glmnet’)
#> selected 1 from 20 predictors
summary(object)
#> --- object of class “cv.corila” --- 
#> generalised linear model with gaussian family 
#> 20 features (8 primary and 12 auxiliary features)
#> initial coefficients: ridge regression 
#> final coefficients: adaptive lasso regression 
#> optimised regularisation parameter: lambda.min = 0.6982 
#> selected weights: local = 1, global = 0
#> selected exponents: local = 0, global = Inf
#> 2 non-zero coefficients (including intercept)
```
