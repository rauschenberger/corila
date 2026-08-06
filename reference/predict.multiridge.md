# Make predictions

**\[stable\]**

Makes predictions from a multi-penalty ridge regression model.

## Usage

``` r
# S3 method for class 'multiridge'
predict(object, newx, ...)
```

## Arguments

- object:

  object of type `"multiridge"`

- newx:

  \\n_0 \times p\\ predictor matrix (training data) to obtain fitted
  values, \\n_1 \times p\\ predictor matrix (testing data) to obtain
  predicted values

- ...:

  (for compatibility with
  [stats::predict](https://rdrr.io/r/stats/predict.html))

## Value

Returns an \\n_0\\-dimensional vector of fitted values or an
\\n_1\\-dimensional vector of predicted values.

## References

[Mark A. van de Wiel](https://orcid.org/0000-0003-4780-8472), [Mirrelijn
M. van Nee](https://orcid.org/0000-0001-7715-1446) and [Armin
Rauschenberger](https://orcid.org/0000-0001-6498-4801) (2021). "Fast
cross-validation for multi-penalty high-dimensional ridge regression"
*Journal of Computational and Graphical Statistics* 30(4):835-847.
[doi:10.1080/10618600.2021.1904962](https://doi.org/10.1080/10618600.2021.1904962)
.

## See also

Fit models with
[`multiridge()`](https://rauschenberger.github.io/corila/reference/multiridge.md)
and extract coefficients with
[coef()](https://rauschenberger.github.io/corila/reference/coef.multiridge.md).

## Examples

``` r
# \donttest{
data <- simulate_data(prob_primary = 1.0)
model <- multiridge(x = data$x_train, y = data$y_train, group = data$group)
beta_hat <- coef(model)
y_hat <- predict(object = model, newx = data$x_test)

#> Error in rep(1, n) : invalid 'times' argument
#> Error in rep(1, n): invalid 'times' argument
# }
```
