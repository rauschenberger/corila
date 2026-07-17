# Extract coefficients

Extracts coefficients from a multi-penalty ridge regression model.

## Usage

``` r
# S3 method for class 'multiridge'
coef(object, ...)
```

## Arguments

- object:

  object of type `"multiridge"`

- ...:

  (for compatibility with
  [stats::coef](https://rdrr.io/r/stats/coef.html))

## Value

Returns an \\(1 + p)\\-dimensional vector of estimated coefficients
(estimated intercept and estimated slopes).

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
and make predictions with
[predict()](https://rauschenberger.github.io/corila/reference/predict.multiridge.md).

## Examples

``` r
# minimal example
n <- 50L; p <- 20L; q <- 5L
x <- matrix(rnorm(n * p), nrow = n, ncol = p)
y <- rnorm(n)
group <- rep(seq_len(q), length.out = p)
model <- multiridge(x = x, y = y, group = group)

# fitting with given folds
foldid <- sample(seq_len(10L), size = n, replace = TRUE)
model <- multiridge(x = x, y = y, group = group, foldid = foldid)

# fitting with given penalties
penalties <- abs(rnorm(q))
model <- multiridge(x = x, y = y, group = group, penalties = penalties)

# \donttest{
# simulation
set.seed(1)
n0 <- 100
n1 <- 10000
n <- n0 + n1
p <- c(100, 50)
group <- rep(x = seq_along(p), times = p)
x <- sapply(X = group, FUN = function(x) stats::rnorm(n = n, sd = x))
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

# standard ridge regression
object <- glmnet::cv.glmnet(x = x[cond, ], y = y[cond],
                           family = family, alpha = 0)
coef$glmnet <- stats::coef(object = object, s = "lambda.min")
y_hat$glmnet <- stats::predict(object = object, newx = x[!cond, ],
                              type = "response", s = "lambda.min")

# multi-penalty ridge regression
object <- multiridge(x = x[cond, ], y = y[cond],
                     group = group, family = family)
coef$multiridge <- stats::coef(object = object)
y_hat$multiridge <- stats::predict(object = object, newx = x[!cond, ])

# estimation performance
sapply(coef, function(x) stats::cor(beta, x[-1]))
#>     glmnet multiridge 
#>  0.4692074  0.5982584 
sapply(coef, function(x) mean((beta-x[-1])^2))
#>     glmnet multiridge 
#>  0.1718059  0.1262343 

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
#>     glmnet multiridge 
#>   73.65774   50.10826 
# }
```
