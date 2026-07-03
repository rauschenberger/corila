# Assertions

Check whether provided arguments satisfy expectations.

## Usage

``` r
.assert(
  x,
  type,
  dim = 1,
  na.rm = FALSE,
  support = NULL,
  family = NULL,
  min = -Inf,
  max = Inf
)
```

## Arguments

- x:

  scalar, vector, matrix, or array to be checked

- type:

  character `"numeric"`, `"integer"`, `"nominal"`, or `"logical"`

- dim:

  dimensionality: `dim = 1` for a scalar, `dim = Inf` for a vector of
  arbitrary length, `dim = c(Inf, Inf)` for a matrix of arbitrary
  dimensions, `dim = c(Inf, Inf, Inf)` for an array of arbitrary
  dimensions, `dim = 100` for a vector of length 100,
  `dim = c(Inf, 100)` for a matrix with 100 columns, etc.

- na.rm:

  logical; `FALSE`: missing values are not allowed, `TRUE`: missing
  values are allowed

- support:

  character vector (only used for `type = "nominal"`)

- family:

  character `"gaussian"`, `"binomial"`, `"poisson"`, or `"cox"`

- min:

  numerical value (not used for `type = "nominal"`)

- max:

  numerical value (not used for `type = "nominal"`)

## Value

Returns `NULL` invisibly, or an error message.

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
corila:::.assert(x = NULL)
corila:::.assert(x = rnorm(1), type = "numeric")
corila:::.assert(x = "A", type = "nominal", support = LETTERS)
```
