# List of methods for class `"cv.corila"`

Implemented S3 methods for objects of class `"cv.corila"`:

- [coef()](https://rauschenberger.github.io/corila/reference/coef.cv.corila.md):
  extracts estimated coefficients

- [predict()](https://rauschenberger.github.io/corila/reference/predict.cv.corila.md):
  calculates predicted values

- [fitted()](https://rauschenberger.github.io/corila/reference/fitted.cv.corila.md):
  extracts fitted values

- [residuals()](https://rauschenberger.github.io/corila/reference/residuals.cv.corila.md):
  calculates deviance residuals

- [plot()](https://rauschenberger.github.io/corila/reference/plot.cv.corila.md):
  visualises observed vs fitted values and estimated coefficients

- [print()](https://rauschenberger.github.io/corila/reference/print.cv.corila.md):
  prints information to the console

- [summary()](https://rauschenberger.github.io/corila/reference/summary.cv.corila.md):
  summarises the fitted model

- [deviance()](https://rauschenberger.github.io/corila/reference/deviance.cv.corila.md):
  extracts the deviance

- [nobs()](https://rauschenberger.github.io/corila/reference/nobs.cv.corila.md):
  extracts the number of observations

## Value

[coef()](https://rauschenberger.github.io/corila/reference/coef.cv.corila.md)
returns a \\(1 +) p\\-dimensional vector,
[predict()](https://rauschenberger.github.io/corila/reference/predict.cv.corila.md)
returns an \\n_1\\-dimensional vector,
[fitted()](https://rauschenberger.github.io/corila/reference/fitted.cv.corila.md)
and
[residuals()](https://rauschenberger.github.io/corila/reference/residuals.cv.corila.md)
return an \\n_0\\-dimensional vector. See individual methods for
details.

## See also

Use
[`cv.corila()`](https://rauschenberger.github.io/corila/reference/cv.corila.md)
to fit the model.

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
#>  [1] 0.01806361 0.00000000 0.00000000 0.00000000 0.00000000 0.00000000
#>  [7] 0.00000000 0.00000000 0.00000000 0.00000000 0.00000000 0.00000000
#> [13] 0.00000000 0.00000000 0.00000000 0.00000000 0.00000000 0.00000000
#> [19] 0.00000000 0.00000000 0.00000000
predict(object, newx = x)
#>  [1] 0.01806361 0.01806361 0.01806361 0.01806361 0.01806361 0.01806361
#>  [7] 0.01806361 0.01806361 0.01806361 0.01806361
fitted(object)
#>  [1] 0.01806361 0.01806361 0.01806361 0.01806361 0.01806361 0.01806361
#>  [7] 0.01806361 0.01806361 0.01806361 0.01806361
residuals(object)
#>  [1] -0.9165558 -0.1793822 -0.5025691  2.6860880 -0.7268534  0.3963546
#>  [7] -0.7750733  0.4715784  0.6972766 -1.1508637
plot(object)

print(object)
#> object of class ‘cv.corila’ 
#> (contains multiple objects of class ‘cv.glmnet’)
#> selected 0 from 20 predictors
summary(object)
#> --- object of class “cv.corila” --- 
#> generalised linear model with gaussian family 
#> 20 features (13 primary and 7 auxiliary features)
#> initial coefficients: ridge regression 
#> final coefficients: adaptive lasso regression 
#> optimised regularisation parameter: lambda.min = 3.521 
#> selected weights: local = 0.9, global = 0.1
#> selected exponents: local = 0, global = 1
#> 1 non-zero coefficients (including intercept)
```
