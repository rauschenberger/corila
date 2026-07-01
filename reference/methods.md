# List of methods for class `"cv.corila"`

Lists implemented S3 methods for objects of class `"cv.corila"`.

- [coef()](https://rauschenberger.github.io/corila/reference/coef.cv.corila.md):
  extract estimated coefficients

- [predict()](https://rauschenberger.github.io/corila/reference/predict.cv.corila.md):
  calculate predicted values

- [fitted()](https://rauschenberger.github.io/corila/reference/fitted.cv.corila.md):
  extract fitted values

- [residuals()](https://rauschenberger.github.io/corila/reference/residuals.cv.corila.md):
  calculate deviance residuals

- [plot()](https://rauschenberger.github.io/corila/reference/plot.cv.corila.md):
  visualise observed vs fitted values and estimated coefficients

- [print()](https://rauschenberger.github.io/corila/reference/print.cv.corila.md):
  print information to the console

- [summary()](https://rauschenberger.github.io/corila/reference/summary.cv.corila.md):
  summarise the fitted model

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

# using S3 methods
n <- 10; p <- 20; q <- 5
x <- matrix(rnorm(n * p), nrow = n , ncol = p)
y <- rnorm(n)
group <- rep(seq_len(q), length.out = p)
primary <- as.logical(rbinom(n = p, size = 1, prob = 0.5))
object <- cv.corila(x = x, y = y, group = group, primary = primary)
#> Warning: Option grouped=FALSE enforced in cv.glmnet, since < 3 observations per fold

coef(object)
#>  [1] -0.05399003  0.00000000  0.00000000  0.00000000  0.00000000  0.00000000
#>  [7]  0.00000000  0.00000000  0.00000000  0.00000000  0.00000000  0.00000000
#> [13]  0.00000000
predict(object, newx = x)
#>  [1] -0.05399003 -0.05399003 -0.05399003 -0.05399003 -0.05399003 -0.05399003
#>  [7] -0.05399003 -0.05399003 -0.05399003 -0.05399003
fitted(object)
#>  [1] -0.05399003 -0.05399003 -0.05399003 -0.05399003 -0.05399003 -0.05399003
#>  [7] -0.05399003 -0.05399003 -0.05399003 -0.05399003
residuals(object)
#>  [1] -0.06718629  0.75422162  0.78452109 -0.40187775 -0.45572066 -0.41279919
#>  [7] -0.57563233 -1.03902697  0.81218256  0.60131791
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
#> optimised regularisation parameter: lambda.min = 1.328 
#> selected weights: local = 1, global = 0
#> selected exponents: local = 0, global = Inf
#> 1 non-zero coefficients (including intercept)
```
