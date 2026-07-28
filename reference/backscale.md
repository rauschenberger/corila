# Inverse standardisation

Transforms response variable back to original scale or transforms
coefficients for predictor variables and response variable on original
scales.

## Usage

``` r
.backscale(pars, y = NULL, coef = NULL)
```

## Arguments

- pars:

  list with slots `mu.x` and `sd.x` (\\p\\-dimensional vectors of means
  and standard deviations of the predictor variables), `mu.y` and `sd.y`
  (mean and standard deviation of response variable for Gaussian family,
  0 and 1 for other families), and `family` (character string
  `"gaussian"`, `"binomial"`, `"poisson"`, or `"cox"`)

- y:

  \\n_1\\-dimensional response vector or response matrix with \\n_1\\
  rows and multiple columns (for multiple values of the regularisation
  parameter), or `NULL` (default)

- coef:

  \\(1 + p)\\-dimensional vector containing the estimated intercept and
  the estimated slopes, or `NULL` (default)

## Value

Returns a list with slots `y` or `coef`.

## Details

This function is called by
[`predict.cv.corila()`](https://rauschenberger.github.io/corila/reference/predict.cv.corila.md)
for the predicted values and by
[`coef.cv.corila()`](https://rauschenberger.github.io/corila/reference/coef.cv.corila.md)
for the estimated coefficients.

## See also

Use function
[`.forescale()`](https://rauschenberger.github.io/corila/reference/forescale.md)
to standardise variables.

## Examples

``` r
# \donttest{

# simulate data
family <- "gaussian"
data <- simulate_data(family = family, prob_primary = 1.0)

# regression without standardisation
if (identical(family, "cox")) {
  lm1 <- survival::coxph(data$y_train~., data = data.frame(data$x_train))
} else {
  lm1 <- stats::glm(data$y_train~., data = data.frame(data$x_train),
                    family = family)
}
coef1 <- stats::coef(lm1)
yhat1 <- predict(lm1, newdata = data.frame(data$x_test))

# regression with standardisation
scale <- .forescale(x = data$x_train,
                    y = data$y_train,
                    family = family)
if (identical(family, "cox")) {
  lm2 <- survival::coxph(scale$y~., data = data.frame(scale$x))
} else {
  lm2 <- stats::glm(scale$y~., data = data.frame(scale$x), family = family)
}
coef_temp <- stats::coef(lm2)
newx_temp <- .forescale(x = data$x_test,
                        pars = scale$pars)$x
yhat_temp <- predict(object = lm2, newdata = data.frame(newx_temp))
result <- .backscale(pars = scale$pars,
                     y = yhat_temp,
                     coef = coef_temp)
coef2 <- result$coef
yhat2 <- result$y

# equality
all.equal(coef1, coef2)
#> [1] TRUE
all.equal(yhat1, yhat2)
#> [1] TRUE
# }
```
