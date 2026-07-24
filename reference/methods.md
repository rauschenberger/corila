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
#> -0.24149791  0.00000000 -0.04191072  0.00000000  0.00000000 -0.32230634 
#>        <NA>        <NA>        <NA>        <NA>        <NA>        <NA> 
#>  0.00000000 -0.38686683 -0.22389187  0.00000000 -0.03457487 -0.08825507 
#>        <NA>        <NA>        <NA>        <NA>        <NA>        <NA> 
#>  0.00000000 -0.13812173  0.00000000  0.00000000  0.00000000  0.00000000 
#>        <NA>        <NA>        <NA> 
#>  0.00000000  0.00000000  0.17883403 
predict(object, newx = x)
#>  [1] -0.62285965 -1.25759559 -1.25067536 -0.61376529  0.37283625 -0.45344181
#>  [7]  0.71765790  0.27032945 -0.78264611 -0.02400301
fitted(object)
#>  [1] -0.62285965 -1.25759559 -1.25067536 -0.61376529  0.37283625 -0.45344181
#>  [7]  0.71765790  0.27032945 -0.78264611 -0.02400301
residuals(object)
#>  [1] -0.208843194  0.066932493 -0.096722771  0.001584804  0.102326943
#>  [6] -0.063491125 -0.014378403  0.013690764  0.075507917  0.123392572
plot(object)

print(object)
#> object of class ‘cv.corila’ 
#> (contains multiple objects of class ‘cv.glmnet’)
#> selected 8 from 20 predictors
summary(object)
#> --- object of class “cv.corila” --- 
#> generalised linear model with gaussian family 
#> 20 features (10 primary and 10 auxiliary features)
#> initial coefficients: ridge regression 
#> final coefficients: adaptive lasso regression 
#> optimised regularisation parameter: lambda.min = 0.04127 
#> selected weights: local = 0.5, global = 0.5
#> selected exponents: local = 0, global = 1
#> 9 non-zero coefficients (including intercept)
```
