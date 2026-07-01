# Deviance

Calculates the deviance.

## Usage

``` r
# S3 method for class 'cv.corila'
deviance(object, ...)
```

## Arguments

- object:

  object of class `"cv.corila"`

- ...:

  (not used)

## Value

Returns a scalar.

## Details

Returns the deviance calculated by
[`glmnet::deviance.glmnet()`](https://glmnet.stanford.edu/reference/deviance.glmnet.html)
for the model with the optimised mixing and regularisation
hyperparameters.

## See also

The internal function
[`.deviance()`](https://rauschenberger.github.io/corila/reference/dot-deviance.md)
calculates the deviance from fitted and observed values.
