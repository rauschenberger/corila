# Expand auxiliary features

Add empty columns for auxiliary features.

## Usage

``` r
.expand_auxiliary(x, primary)
```

## Arguments

- x:

  matrix with \\n\\ rows and either \\p_0\\ or \\p_0 + p_1\\ features

- primary:

  logical vector of length \\p_0 + p_1\\ with \\p_0\\ entries equal to
  `TRUE` (primary features) and \\p_1\\ entries equal to `FALSE`
  (auxiliary features)

## Value

Returns a matrix with \\n\\ rows and \\p_0 + p_1\\ columns.

## Examples

``` r
n <- 5
p <- 10
x <- matrix(data = rnorm(n * p), nrow = n, ncol = p)
primary <- as.logical(rbinom(n = p, size = 1, prob = 0.5))
x_primary <- x[, primary]
x_expanded <- corila:::.expand_auxiliary(x = x_primary, primary = primary)
all(x_expanded[, primary] == x[, primary])
#> [1] TRUE
all(x_expanded[, !primary] == 0)
#> [1] TRUE
```
