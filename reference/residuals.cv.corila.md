# Residuals

Residuals

## Usage

``` r
# S3 method for class 'cv.corila'
residuals(object, ...)
```

## Arguments

- object:

  object of class `"cv.corila"`

- ...:

  (not used)

## Value

Returns a numeric vector of length \\n_0\\ (one entry for each training
observation).

## Details

This function extracts the observed and fitted values from the fitted
model and calls the internal function
[`.residuals()`](https://rauschenberger.github.io/corila/reference/dot-residuals.md)
to calculate the residuals.
