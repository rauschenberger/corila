# predict (S3 method)

Makes prediction from an object of class `"corila"`.

## Usage

``` r
# S3 method for class 'corila'
predict(object, newx, index, s, ...)
```

## Arguments

- object:

  object of class `"corila"`

- newx:

  \\n_0 \times p\\ predictor matrix (training data) to obtain fitted
  values, \\n_1 \times p\\ predictor matrix (testing data) to obtain
  predicted values

- index:

  integer scalar specifying the index of the mixing hyperparameter(s)

- s:

  numeric vector specifying the values of the regularisation
  hyperparameter

- ...:

  (for compatibility with
  [stats::predict](https://rdrr.io/r/stats/predict.html))

## Value

Returns fitted or predicted values in an \\n_0 \times m\\-dimensional or
\\n_1 \times m\\-dimensional matrix, respectively.

## References

[Armin Rauschenberger](https://orcid.org/0000-0001-6498-4801) (2026).
"Sparse modelling with grouped and correlated features allowing for
privileged information". *In preparation*.

## See also

Estimate parameters with
[`corila()`](https://rauschenberger.github.io/corila/reference/corila.md),
or estimate parameters and tune hyperparameters with
[`cv.corila()`](https://rauschenberger.github.io/corila/reference/cv.corila.md).

## Examples

``` r
# \donttest{
# simulation
n <- 100
p <- 50
group <- rep(x = 1:10, each = 5)
primary <- rep(x = TRUE, times = p)
x <- matrix(data = rnorm(n * p), nrow = n, ncol = p)
y <- rnorm(n = n)

# model fitting
hyper <- data.frame(exp_local = 1, wgt_local = 0.5,
                    exp_global = 1, wgt_global = 0.5)
object <- corila(x = x,
                 y = y,
                 group = group,
                 primary = primary,
                 family = "gaussian",
                 alpha_init = 0,
                 alpha_final = 1,
                 cor = "spearman",
                 foldid = NULL,
                 nfolds = 10,
                 hyper = hyper,
                 lambda_init = NULL)
#> Warning: no non-missing arguments to max; returning -Inf
#> Error in glmnet::cv.glmnet(x = x, y = y, family = family, alpha = alpha_init,     foldid = foldid, nfolds = nfolds): nfolds must be bigger than 3; nfolds=10 recommended

y_hat <- stats::predict(object, newx = x, index = 1, s = 0)
#> Error: object 'object' not found
# }
```
