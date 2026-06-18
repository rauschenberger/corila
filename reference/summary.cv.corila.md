# Summarising Sparse Group Lasso (S3 method)

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

  (not used)

- x:

  object of class `"summary.cv.corila"`

## Value

Returns an invisible list with multiple slots.

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
print(object)
#> object of class ‘cv.corila’ 
#> (contains multiple objects of class ‘cv.glmnet’)
summary(object)
#> --- object of class “cv.corila” --- 
#> generalised linear model with gaussian family 
#> 20 features (7 primary and 13 auxiliary features)
#> initial coefficients: ridge regression 
#> final coefficients: adaptive lasso regression 
#> optimised regularisation parameter: lambda.min = 0.5673 
#> selected weights: local = 0.5, global = 0.5
#> selected exponents: local = 0, global = 1
#> 1 non-zero coefficients (including intercept)
```
