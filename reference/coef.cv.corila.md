# Extract coefficients

Extracts coefficients from an object of class `"cv.corila"`.

## Usage

``` r
# S3 method for class 'cv.corila'
coef(object, s = "lambda.min", ...)
```

## Arguments

- object:

  object of class `"cv.corila"`

- s:

  character `"lambda.min"` or numeric value

- ...:

  (for compatibility with
  [stats::coef](https://rdrr.io/r/stats/coef.html))

## Value

Returns an \\(1 + p)\\-dimensional vector of the estimated coefficients.
The first entry is the estimated intercept, and the other \\p\\ entries
are the estimated slopes.

## Details

This function calls
[`.combine_slopes()`](https://rauschenberger.github.io/corila/reference/combine_slopes.md)
to combine positive and negative coefficients and
[`.backscale()`](https://rauschenberger.github.io/corila/reference/backscale.md)
to bring coefficients back to the original scale.

## References

[Armin Rauschenberger](https://orcid.org/0000-0001-6498-4801) (2026).
"Sparse modelling with grouped and correlated features allowing for
privileged information". *In preparation*.

## See also

Fit models with
[`cv.corila()`](https://rauschenberger.github.io/corila/reference/cv.corila.md)
and make predictions with
[predict()](https://rauschenberger.github.io/corila/reference/predict.cv.corila.md).

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
