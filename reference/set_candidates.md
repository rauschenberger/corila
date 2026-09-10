# Candidate values

Sets candidate values for hyperparameters.

## Usage

``` r
.set_candidates(tune)
```

## Arguments

- tune:

  character string for determining the candidate values for the
  hyperparameters:

  - `"none"`: fixed weights and exponents (`wgt_local`=1, `exp_local`=1,
    `wgt_global`=0), no tuning (share information within groups, ignore
    correlations)

  - `"weight"` (default): fixed exponents (`exp_local`=0,
    `exp_global`=1), tuning `wgt_local`=1-`wgt_global` (find a
    compromise between sharing information within groups and between
    correlated predictors)

  - `"exponent"`: fixed weights (`wgt_local`=1, `wgt_global`=0), tuning
    `exp_local` (share information between correlated predictors in the
    same group, determine level of trust in correlation coefficients)

  - `"bivariate"`: tuning `wgt_local`=1-`wgt_global` and
    `exp_local`=`exp_global` (find a compromise between sharing
    information between predictors in the same group and between all
    predictors, determine level of trust in correlation coefficients)

  - `"factorial"`: tuning `wgt_local`, `exp_local`, `wgt_global`,
    `exp_global` (unrestricted information sharing with weights possibly
    not summing to one and possibly different exponents)

  (The internal function `.set_candidates()` uses this argument to
  create a grid of candidates values for `wgt_local`, `exp_local`,
  `wgt_global`, and `exp_global`.)

## Value

Returns a data frame with the slots `"wgt_local"` and `"exp_local"` for
the local prior information and the slots `"wgt_global"` and
`"exp_global"` for the global prior information.

## Details

- If the local weight equals 0, the local exponent has no influence. And
  if the global weight equals 0, the global exponent has no influence.
  Therefore, if a weight is close to zero, its exponent is set to
  infinity. This avoids redundant combinations of hyperparameters (i.e.,
  a local weight of zero with multiple local exponents, or a global
  weight of zero with multiple global exponents)

- The experimental hyperparameter `"threshold"` is currently always set
  to 0 (no thresholding of correlation coefficients).

## See also

This function is called by
[`cv.corila()`](https://rauschenberger.github.io/corila/reference/cv.corila.md).

## Examples

``` r
.set_candidates(tune = "none")
#>   wgt_local exp_local wgt_global exp_global threshold
#> 1         1         1          0        Inf         0
```
