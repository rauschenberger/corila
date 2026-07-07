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
[`.combine_slopes()`](https://rauschenberger.github.io/corila/reference/dot-combine_slopes.md)
to combine positive and negative coefficients and
[`.backscale()`](https://rauschenberger.github.io/corila/reference/dot-backscale.md)
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
# minimal example
set.seed(1)
n <- 50; p <- 20; q <- 5
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
set.seed(1)
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
