# Example data

This is example data for modelling a response based on grouped and
correlated primary and auxiliary predictors.

## Usage

``` r
data
```

## Format

The object `data` contains a list with multiple slots:

- `x_train`: predictor matrix of the training observations (\\n_0\\
  rows, \\p\\ columns)

- `y_train`: response vector of the training observations (length
  \\n_0\\)

- `group`: integer vector indicating the group of the predictors (length
  \\p\\)

- `primary`: logical vector indicating primary (`TRUE`) and auxiliary
  (`FALSE`) predictors (length \\p\\)

- `beta`: numeric vector of the effects of the predictors on the
  response (length \\p\\)

- `x_test`: \\n_1 \times p\\ predictor matrix for the test observations

- `y_test`: response vector for the test observations of length \\n_1\\

## Details

Use the objects `x_train`, `y_train`, `group`, and `primary` for model
training. Estimated coefficients can be compared with `beta`. Use the
object `x_test` for model testing. Predicted values can be compared with
`y_test`.
