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

# fitting the model
n <- 10; p <- 20; q <- 5
x <- matrix(rnorm(n * p), nrow = n , ncol = p)
y <- rnorm(n)
group <- rep(seq_len(q), length.out = p)
primary <- as.logical(rbinom(n = p, size = 1, prob = 0.5))
object <- cv.corila(x = x, y = y, group = group, primary = primary)
#> Warning: Option grouped=FALSE enforced in cv.glmnet, since < 3 observations per fold
#> Error: from glmnet C++ code (error code 7777); All used predictors have zero variance

# using S3 methods
coef(object)
#> Error: object 'object' not found
predict(object, newx = x)
#> Error: object 'object' not found
fitted(object)
#> Error: object 'object' not found
residuals(object)
#> Error: object 'object' not found
plot(object)
#> Error: object 'object' not found
print(object)
#> Error: object 'object' not found
summary(object)
#> Error: object 'object' not found
```
