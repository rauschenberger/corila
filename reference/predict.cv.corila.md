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
# minimal example
set.seed(1L)
n <- 50L; p <- 20L; q <- 5L
x <- matrix(rnorm(n * p), nrow = n , ncol = p)
y <- rnorm(n)
group <- rep(seq_len(q), length.out = p)
primary <- as.logical(rbinom(n = p, size = 1, prob = 0.5))
cv.corila(x = x, y = y, group = group, primary = primary, tune = "none")
#> object of class ‘cv.corila’ 
#> (contains an object of class ‘cv.glmnet’)
#> selected 0 from 20 predictors

# \donttest{
# simulation
set.seed(1L)
n0 <- 100
n1 <- 10000
n <- n0 + n1
p <- c(100, 50)
z <- rep(x = seq_along(p), times = p)
x <- sapply(X = z, FUN = function(x) stats::rnorm(n = n, sd = x))
beta <- stats::rnorm(n = sum(p), mean = 1, sd = 0) *
        stats::rbinom(n = sum(p), size = 1, prob = 0.2)
eta <- x %*% beta
family <- "gaussian"
if (identical(family, "gaussian")) {
  y <- eta + 0.5 * stats::rnorm(n = n, sd = stats::sd(eta))
} else if (identical(family, "binomial")) {
  y <- stats::rbinom(n = n, size = 1, prob = 1 / (1 + exp(-eta)))
} else if (identical(family, "cox")) {
  time <- stats::rexp(n = n, rate = exp(eta))
  status <- stats::rbinom(n = n, prob = 0.5, size = 1)
  y <- survival::Surv(time = time, event = status)
}
cond <- rep(x = c(TRUE, FALSE), times = c(n0, n1))

y_hat <- coef <- list()

# standard lasso regression
object <- glmnet::cv.glmnet(x = x[cond, ], y = y[cond],
                            family = family, alpha = 1)
coef$glmnet <- stats::coef(object = object, s = "lambda.min")
y_hat$glmnet <- stats::predict(object = object, newx = x[!cond, ],
                               type = "response", s = "lambda.min")

# flexible group lasso regression
object <- cv.corila(x = x[cond, ], y = y[cond], group = z, family = family)
coef$corila <- stats::coef(object = object)
y_hat$corila <- stats::predict(object = object, newx = x[!cond, ])

# selection performance
sapply(coef, function(x) mean(sign(x[-1]) == sign(beta)))
#>    glmnet    corila 
#> 0.8066667 0.7866667 
sapply(coef, function(x) {
  sum(sign(x[-1]) != 0 & sign(x[-1]) == sign(beta)) / sum(x[-1] != 0)
})
#>    glmnet    corila 
#> 0.5675676 0.5227273 

# predictive performance
if (identical(family, "gaussian")) {
  metric <- sapply(X = y_hat, FUN = function(x)
    mean((x-y[!cond])^2))
} else if (identical(family, "binomial")) {
  metric <- sapply(X = y_hat, FUN = function(x)
    pROC::auc(response = y[!cond],
              predictor = as.vector(x),
              levels = c(0, 1),
              direction = "<"))
} else if (identical(family, "cox")) {
  metric <- sapply(X = y_hat, FUN = function(x)
    survival::concordance(y[!cond]~I(-x))$concordance)
}
metric
#>   glmnet   corila 
#> 61.23431 49.25623 

# privileged information
#primary <- stats::rbinom(n = sum(p), size = 1, prob = 0.5) == 1
#object <- cv.corila(x = x[cond, ], y = y[cond], group = z,
#                     primary = primary, family = family)
# }
```
