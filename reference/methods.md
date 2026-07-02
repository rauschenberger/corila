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
#>  [1]  0.10978670  0.00000000 -0.23122594 -0.13002684  0.00000000 -0.04111522
#>  [7]  0.00000000  0.07875981 -0.02046811  0.12369290  0.00000000
predict(object, newx = x)
#>  [1]  0.09926603  0.42161457  0.95068360  0.43627218  0.31049695 -0.04566780
#>  [7] -0.85527265  0.75918734  0.20435338  0.25650187
fitted(object)
#>  [1]  0.09926603  0.42161457  0.95068360  0.43627218  0.31049695 -0.04566780
#>  [7] -0.85527265  0.75918734  0.20435338  0.25650187
residuals(object)
#>  [1] -0.14487701 -0.14115095  0.07226668  0.14535193  0.29639958  0.03748645
#>  [7] -0.18488309 -0.08963054 -0.21939662  0.22843357
plot(object)

print(object)
#> object of class ‘cv.corila’ 
#> (contains multiple objects of class ‘cv.glmnet’)
#> selected 6 from 20 predictors
summary(object)
#> --- object of class “cv.corila” --- 
#> generalised linear model with gaussian family 
#> 20 features (10 primary and 10 auxiliary features)
#> initial coefficients: ridge regression 
#> final coefficients: adaptive lasso regression 
#> optimised regularisation parameter: lambda.min = 0.2222 
#> selected weights: local = 0, global = 1
#> selected exponents: local = Inf, global = 1
#> 7 non-zero coefficients (including intercept)
```
