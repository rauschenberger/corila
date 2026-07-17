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
n <- 12L # decrease to 10 to check LOOCV
p <- 20L
q <- 5L
x <- matrix(rnorm(n * p), nrow = n, ncol = p)
y <- rnorm(n)
group <- rep(seq_len(q), length.out = p)
primary <- as.logical(rbinom(n = p, size = 1L, prob = 0.5))
object <- cv.corila(x = x, y = y, group = group, primary = primary)
#> Warning: Option grouped=FALSE enforced in cv.glmnet, since < 3 observations per fold
print(object)
#> object of class ‘cv.corila’ 
#> (contains multiple objects of class ‘cv.glmnet’)
#> selected 0 from 20 predictors
summary(object)
#> --- object of class “cv.corila” --- 
#> generalised linear model with gaussian family 
#> 20 features (9 primary and 11 auxiliary features)
#> initial coefficients: ridge regression 
#> final coefficients: adaptive lasso regression 
#> optimised regularisation parameter: lambda.min = 0.986 
#> selected weights: local = 1, global = 0
#> selected exponents: local = 0, global = Inf
#> 1 non-zero coefficients (including intercept)
```
