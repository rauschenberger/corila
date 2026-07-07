# Summarising sparse group lasso (S3 method)

Summary method for class `"cv.corila"`.

## Usage

``` r
# S3 method for class 'cv.corila'
summary(object, ...)

# S3 method for class 'summary.cv.corila'
print(x, ...)
```

## Arguments

- object:

  object of class `"cv.corila"`

- ...:

  (for compatibility with
  [base::summary](https://rdrr.io/r/base/summary.html))

- x:

  object of class `"summary.cv.corila"`

## Value

Returns an invisible list with multiple slots.

## Details

`print.summary.cv.corila()` uses the output from `summary.cv.corila()`
to print readable information to the console. It calls the helper
function
[`.type()`](https://rauschenberger.github.io/corila/reference/dot-type.md)
to name the methods used to estimate initial and final coefficients.

## See also

[`print.cv.corila()`](https://rauschenberger.github.io/corila/reference/print.cv.corila.md)

## Examples

``` r
n <- 12 # decrease to 10 to check LOOCV
p <- 20
q <- 5
x <- matrix(rnorm(n * p), nrow = n, ncol = p)
y <- rnorm(n)
group <- rep(seq_len(q), length.out = p)
primary <- as.logical(rbinom(n = p, size = 1, prob = 0.5))
object <- cv.corila(x = x, y = y, group = group, primary = primary)
#> Warning: Option grouped=FALSE enforced in cv.glmnet, since < 3 observations per fold
#> Error: from glmnet C++ code (error code 7777); All used predictors have zero variance
print(object)
#> Error: object 'object' not found
summary(object)
#> Error: object 'object' not found
```
