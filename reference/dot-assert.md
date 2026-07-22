# Assertions

Check whether provided arguments satisfy expectations.

## Usage

``` r
.assert(
  x,
  type = "numeric",
  dim = 1L,
  na.rm = FALSE,
  support = NULL,
  family = NULL,
  min = -Inf,
  max = Inf
)
```

## Arguments

- x:

  scalar, vector (of length `dim`), matrix (of dimensions `dim`), or
  array (of dimensions `dim`) to be checked

  - `type = "numeric"`: numeric

  - `type = "integer"`: integer

  - `type = "nominal"`: character

  - `type = "logical"`: logical

  - `family = "binomial"`: integers 0 or 1

  - `family = "poisson"`: non-negative integers

  - `family = "cox"`: object created with
    [survival::Surv](https://rdrr.io/pkg/survival/man/Surv.html)

- type:

  character scalar `"numeric"` (default), `"integer"`, `"nominal"`, or
  `"logical"`

- dim:

  vector of length 1, 2 or 3 containing positive integers (minimum 1,
  maximum \\100,000\\) defining the dimensionality:

  - scalar `x`: `dim = 1`

  - vector `x` of length 100: `dim = 100`

  - vector `x` of arbitrary length: `dim = Inf`

  - matrix `x` with 100 rows: `dim = c(100, Inf)`

  - matrix `x` of arbitrary dimensions: `dim = c(Inf, Inf)`

  - array `x` of arbitrary dimensions: `dim = c(Inf, Inf, Inf)`

- na.rm:

  logical scalar (or numeric 0/1):

  - `na.rm=FALSE` (or `na.rm=0`): missing values are not allowed

  - `na.rm=TRUE` (or `na.rm=1`): missing values are allowed

- support:

  character vector (only used for `type = "nominal"`), (matching with
  `x` is case-insensitive)

- family:

  character string `"gaussian"`, `"binomial"`, `"poisson"`, or `"cox"`

- min:

  numeric scalar (not used for `type = "nominal"`)

- max:

  numeric scalar (not used for `type = "nominal"`)

## Value

Returns `NULL` invisibly, or throws an error.

## Details

This function is called by multiple function of the
[corila-package](https://rauschenberger.github.io/corila/reference/corila-package.md).

## See also

The function
[`.validate()`](https://rauschenberger.github.io/corila/reference/dot-validate.md)
verifies whether the main arguments have compatible dimensions (number
of samples and features).

## Examples

``` r
n <- 3L; p <- 4L
.assert(x = matrix(rnorm(n = n * p), nrow = n, ncol = p),
        dim = c(n, p),
        type = "numeric",
        family = "gaussian")
.assert(x = rpois(n = n, lambda = 4.0),
        dim = n,
        type = "integer",
        family = "poisson")
.assert(x = rbinom(n = n, size = 1L, prob = 0.5),
        dim = n,
        type = "integer",
        family = "binomial")
.assert(x = "a",
        dim = 1L,
        type = "nominal",
        support = letters)
```
