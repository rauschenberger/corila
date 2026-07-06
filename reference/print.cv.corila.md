# print (S3 method)

Print method for class `"cv.corila"`.

## Usage

``` r
# S3 method for class 'cv.corila'
print(x, ...)
```

## Arguments

- x:

  object of class `"cv.corila"`

- ...:

  (for compatibility with
  [base::print](https://rdrr.io/r/base/print.html))

## Value

Prints `"object of class 'cv.corila'"` to the console (with a note on
the number of cross-validated models). Returns `x` invisibly.

## See also

[summary()](https://rauschenberger.github.io/corila/reference/summary.cv.corila.md)

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
#> selected 0 from 20 predictors
summary(object)
#> --- object of class “cv.corila” --- 
#> generalised linear model with gaussian family 
#> 20 features (11 primary and 9 auxiliary features)
#> initial coefficients: ridge regression 
#> final coefficients: adaptive lasso regression 
#> optimised regularisation parameter: lambda.min = 0.8647 
#> selected weights: local = 0, global = 1
#> selected exponents: local = Inf, global = 1
#> 1 non-zero coefficients (including intercept)
```
