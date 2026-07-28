# Standardisation

Transforms variables to mean 0 and variance 1.

## Usage

``` r
.forescale(x, y = NULL, family = NULL, pars = NULL)
```

## Arguments

- x:

  \\n_0 \times p\\ predictor matrix, containing only numerical values
  (continuous, integer, or binary), where \\n_0\\ is the number of
  observations used for model training and \\p\\ is the number of
  predictors

- y:

  response vector (only required if `family="gaussian"`) or `NULL`

- family:

  character string `"gaussian"`, `"binomial"`, `"poisson"`, or `"cox"`;
  or `NULL` (if `pars` is provided)

- pars:

  list as defined in section *Value*, or `NULL` (if `family` is
  provided)

## Value

Returns a list with multiple slots:

- standardised \\n_0 \times p\\ or \\n_1 \times p\\ predictor matrix
  \\x\\

- standardised \\n_0\\-dimensional or \\n_1\\-dimensional response
  vector \\y\\ (only if \\y\\ is provided and `family = "gaussian"` or
  `pars$family = "gaussian"`; otherwise output equals input)

- character string `family` indicates the model (`"gaussian"`,
  `"binomial"`, `"poisson"`, or `"cox"`), determined by argument
  `family` or `pars$family`

- list `pars` with slots `mu.x` and `sd.x` (\\p\\-dimensional vectors of
  means and standard deviations of the predictor variables), `mu.y` and
  `sd.y` (mean and standard deviation of response variable for Gaussian
  family, 0 and 1 for other families), and `family` (character string
  `"gaussian"`, `"binomial"`, `"poisson"`, or `"cox"`)

## Details

This function is called by
[`corila()`](https://rauschenberger.github.io/corila/reference/corila.md)
for the training data and by
[`predict.corila()`](https://rauschenberger.github.io/corila/reference/predict.corila.md)
for the testing data.

## See also

Use function
[`.backscale()`](https://rauschenberger.github.io/corila/reference/backscale.md)
to bring coefficients and predictions back to original scale.

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
