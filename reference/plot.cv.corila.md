# Plot Sparse Group Lasso (S3 method)

Plot method for class `"cv.corila"`.

## Usage

``` r
# S3 method for class 'cv.corila'
plot(x, ...)
```

## Arguments

- x:

  object of class `"cv.corila"`

- ...:

  (for compatibility with
  [base::plot](https://rdrr.io/r/base/plot.html))

## Value

Returns `NULL` invisibly.

## Details

This function generates two figures:

- a scatter plot of fitted versus observed values for the Gaussian and
  the Poisson families, a box plot of predicted probabilities for the
  two classes for the binomial family, or a histogram of fitted relative
  risks for the Cox model

- estimated coefficients versus indices of predictors

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
#> selected 8 from 20 predictors
summary(object)
#> --- object of class “cv.corila” --- 
#> generalised linear model with gaussian family 
#> 20 features (10 primary and 10 auxiliary features)
#> initial coefficients: ridge regression 
#> final coefficients: adaptive lasso regression 
#> optimised regularisation parameter: lambda.min = 0.08963 
#> selected weights: local = 0, global = 1
#> selected exponents: local = Inf, global = 1
#> 9 non-zero coefficients (including intercept)
```
