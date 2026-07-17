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
n <- 10L; p <- 20L; q <- 5L
x <- matrix(rnorm(n * p), nrow = n , ncol = p)
y <- rnorm(n)
group <- rep(seq_len(q), length.out = p)
primary <- as.logical(rbinom(n = p, size = 1L, prob = 0.5))
object <- cv.corila(x = x, y = y, group = group, primary = primary)
#> Warning: Option grouped=FALSE enforced in cv.glmnet, since < 3 observations per fold

# using S3 methods
coef(object)
#>  [1] -0.3194595  0.0000000  0.0000000  0.0000000  0.0000000  0.0000000
#>  [7]  0.0000000  0.0000000  0.0000000  0.0000000  0.0000000  0.0000000
#> [13]  0.0000000  0.0000000  0.0000000  0.0000000  0.0000000  0.0000000
#> [19]  0.0000000  0.0000000  0.0000000
predict(object, newx = x)
#>  [1] -0.3194595 -0.3194595 -0.3194595 -0.3194595 -0.3194595 -0.3194595
#>  [7] -0.3194595 -0.3194595 -0.3194595 -0.3194595
fitted(object)
#>  [1] -0.3194595 -0.3194595 -0.3194595 -0.3194595 -0.3194595 -0.3194595
#>  [7] -0.3194595 -0.3194595 -0.3194595 -0.3194595
residuals(object)
#>  [1] -1.4694153  0.2787855  1.4097096  1.5532303 -0.1279948 -0.6676504
#>  [7] -1.0561274  1.0007138 -0.1721173 -0.7491339
plot(object)

print(object)
#> object of class ‘cv.corila’ 
#> (contains multiple objects of class ‘cv.glmnet’)
#> selected 0 from 20 predictors
summary(object)
#> --- object of class “cv.corila” --- 
#> generalised linear model with gaussian family 
#> 20 features (7 primary and 13 auxiliary features)
#> initial coefficients: ridge regression 
#> final coefficients: adaptive lasso regression 
#> optimised regularisation parameter: lambda.min = 0.8538 
#> selected weights: local = 0.1, global = 0.9
#> selected exponents: local = 0, global = 1
#> 1 non-zero coefficients (including intercept)
```
