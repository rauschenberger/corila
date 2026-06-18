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

  (not used)

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
primary <- NULL
x <- matrix(data = rnorm(n * p), nrow = n, ncol = p)
y <- rnorm(n = n)

# model fitting
hyper <- data.frame(exp_local = 1, wgt_local = 0.5,
                    exp_global = 1, wgt_global = 0.5)
object <- corila(x, y, group, primary, family = "gaussian", hyper = hyper)

y_hat <- stats::predict(object, newx = x, index = 1, s = 0)
# }
```
