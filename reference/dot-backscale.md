# Inverse Standardisation

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
[`.forescale()`](https://rauschenberger.github.io/corila/reference/dot-forescale.md)
to standardise variables.

## Examples

``` r
# \donttest{
# simulate data
family <- "gaussian"
n0 <- 100; n1 <- 50; p <- 3
n <- n0 + n1
fold <- rep(c(0, 1), times = c(n0, n1))
sd <- stats::rpois(n = p, lambda = 5)
x <- data.frame(x = sapply(X = sd,
                           FUN = function(x) stats::rnorm(n = n, sd = x)))
beta <- stats::rnorm(n = p)
eta <- as.matrix(x) %*% beta
if (identical(family, "gaussian")) {
  y <- stats::rnorm(n = n, mean = eta)
} else if (identical(family, "binomial")) {
  y <- stats::rbinom(n = n, size = 1, prob = 1 / (1 + exp(-eta)))
} else if (identical(family, "poisson")) {
  y <- stats::rpois(n = n, lambda = exp(eta))
} else if (identical(family, "cox")) {
  time <- stats::rexp(n = n, rate = exp(eta))
  status <- stats::rbinom(n = n, prob = 0.5, size = 1)
  y <- survival::Surv(time = time, event = status)
}

# regression without standardisation
if (identical(family, "cox")) {
  lm1 <- survival::coxph(y[fold == 0]~., data=x[fold == 0, ])
} else {
  lm1 <- stats::glm(y[fold == 0]~., data=x[fold == 0, ], family=family)
}
coef1 <- stats::coef(lm1)
yhat1 <- predict(lm1, newdata = x[fold == 1, ])

# regression with standardisation
scale <- corila:::.forescale(x = as.matrix(x)[fold == 0, ],
                   y = y[fold == 0],
                   family = family)
if (identical(family, "cox")) {
  lm2 <- survival::coxph(scale$y~., data = data.frame(scale$x))
} else {
  lm2 <- stats::glm(scale$y~., data = data.frame(scale$x), family = family)
}
coef_temp <- stats::coef(lm2)
newx_temp <- corila:::.forescale(x = as.matrix(x)[fold == 1, ],
                                 pars = scale$pars)$x
yhat_temp <- predict(object = lm2, newdata = data.frame(newx_temp))
result <- corila:::.backscale(pars = scale$pars,
                              y = yhat_temp,
                              coef = coef_temp)
coef2 <- result$coef
yhat2 <- result$y

# equality
all.equal(coef1, coef2, check.attributes = FALSE)
#> [1] TRUE
all.equal(yhat1, yhat2, check.attributes = FALSE)
#> [1] TRUE
# }
```
