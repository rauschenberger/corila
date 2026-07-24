# predict (S3 method)

Makes predictions from an object of class `"cv.corila"`.

## Usage

``` r
# S3 method for class 'cv.corila'
predict(object, newx, s = "lambda.min", ...)
```

## Arguments

- object:

  object of class `"cv.corila"`

- newx:

  \\n_0 \times p\\ predictor matrix (training data) to obtain fitted
  values, \\n_1 \times p\\ predictor matrix (testing data) to obtain
  predicted values

- s:

  character `"lambda.min"` or numeric value

- ...:

  (for compatibility with
  [stats::predict](https://rdrr.io/r/stats/predict.html))

## Value

Returns fitted or predicted values in an \\n_0 \times m\\-dimensional or
\\n_1 \times m\\-dimensional matrix, respectively.

## Details

This function calls
[`.expand_auxiliary()`](https://rauschenberger.github.io/corila/reference/dot-expand_auxiliary.md)
for handling auxiliary predictors,
[`.forescale()`](https://rauschenberger.github.io/corila/reference/dot-forescale.md)
for standardising the predictor matrix, and
[`.backscale()`](https://rauschenberger.github.io/corila/reference/dot-backscale.md)
for bringing predicted values back to the original scale (if
`family="gaussian"`).

## References

[Armin Rauschenberger](https://orcid.org/0000-0001-6498-4801) (2026).
"Sparse modelling with grouped and correlated features allowing for
privileged information". *In preparation*.

## See also

Fit models with
[`cv.corila()`](https://rauschenberger.github.io/corila/reference/cv.corila.md),
extract coefficients with
[coef()](https://rauschenberger.github.io/corila/reference/coef.cv.corila.md),
and extract fitted values with
[fitted()](https://rauschenberger.github.io/corila/reference/fitted.cv.corila.md).

## Examples

``` r
# \donttest{
data <- simulate_data()
model <- cv.corila(x = data$x_train,
                   y = data$y_train,
                   group = data$group,
                   primary = data$primary)
beta_hat <- coef(object = model)
y_hat <- predict(object = model, newx = data$x_test)
# }

# example for automatic mutation testing (with the R package autotest)
data <- simulate_data()
model <- cv.corila(x = data$x_train,
                   y = data$y_train,
                   group = as.double(data$group),
                   primary = data$primary,
                   alpha_init = 0.0,
                   foldid = rep(1:10, length.out = nrow(data$x_train)))
```
