# Name (helper function)

Names the method used for obtaining initial or final coefficients.

## Usage

``` r
.type(alpha)
```

## Arguments

- alpha:

  elastic net mixing parameter or character string (see `alpha_init` and
  `alpha_final` in
  [`cv.corila()`](https://rauschenberger.github.io/corila/reference/cv.corila.md))

## Value

Returns a character string (`"ridge regression"`, `"lasso regression"`,
`"elastic net regression"`, `"multi-penalty ridge regression"`, or
`"Pearson/Spearman/Kendall correlation"`)

## See also

This function is called by
[`print.summary.cv.corila()`](https://rauschenberger.github.io/corila/reference/summary.cv.corila.md).

## Examples

``` r
corila:::.type(alpha = 0)
#> [1] "ridge regression"
```
