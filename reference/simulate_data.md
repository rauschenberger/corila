# Data simulation

Simulates a predictor matrix, an effect vector and a response vector.
The simulated datasets can be used for modelling a response based on
grouped and correlated primary and auxiliary predictors.

## Usage

``` r
simulate_data(
  n0 = 50L,
  n1 = 20L,
  p = 30L,
  q = 10L,
  family = "gaussian",
  rho = 0.5,
  prob_primary = 0.5,
  signal_strength = 1,
  prob_group = 0.5,
  prob_predictor = 0.8,
  seed = 1L
)
```

## Arguments

- n0:

  number of training observations: positive integer scalar (minimum 1,
  maximum \\10\\000\\)

- n1:

  number of testing observations: non-negative integer scalar (minimum
  0, maximum \\100\\000\\)

- p:

  number of predictors: positive integer scalar (minimum 1 leads to a
  single predictor, maximum \\1\\000\\)

- q:

  number of predictor groups: positive integer scalar (minimum 1 assigns
  all predictors to the same group. maximum `p` assigns each predictor
  to its own group)

- family:

  character string `"gaussian"`, `"binomial"`, `"poisson"`, or `"cox"`

- rho:

  correlation coefficient for predictors within the same group: numeric
  scalar in the unit interval (minimum 0 leads to uncorrelated
  predictors within each group, maximum 1 leads to identical predictors
  within each group)

- prob_primary:

  probability for each predictor to be primary (rather than auxiliary):
  numeric scalar in the unit interval (minimum 0 leads to auxiliary
  predictors only, maximum 1 leads to primary predictors only)

- signal_strength:

  non-negative numeric scalar for multiplying the effect sizes (default:
  `signal_strength=1.0`, minimum 0 sets all effect sizes to 0, maximum 2
  to avoid undefined values)

- prob_group:

  probability for each predictor group to be active: numeric scalar in
  the unit interval (minimum 0 makes all groups inactive, maximum 1
  makes all groups active)

- prob_predictor:

  probability for each predictor in an active group to be active:
  numeric scalar in the unit interval (minimum 0 makes all predictors
  inactive, maximum 1 makes all predictors in active groups active)

- seed:

  random seed for reproducibility: integer scalar (unrestricted)

## Value

Returns a named list with the following slots:

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
training. Estimated coefficients can be compared with `beta`.

Use the object `x_test` for model testing. Predicted values can be
compared with `y_test`.

Training and testing observations are named `train_` or `test_`,
respectively, followed by a number indexing the observations (e.g.,
`train_1` or `test_1`).

Primary and auxiliary predictors are named `pri_` or `aux_`,
respectively, followed by a number indexing the predictor groups, a
point, and a number indexing the predictors within this group (e.g.,
`pri_1.1` or `aux_1.1`).

## See also

This function calls the internal functions
[`.simulate_predictors()`](https://rauschenberger.github.io/corila/reference/dot-simulate_predictors.md),
[`.simulate_effects()`](https://rauschenberger.github.io/corila/reference/dot-simulate_effects.md),
and
[`.simulate_response()`](https://rauschenberger.github.io/corila/reference/dot-simulate_response.md)
for simulating the predictor matrix, the effect vector, or the response
vector, respectively.

## Examples

``` r
data <- simulate_data(n0 = 50L, n1 = 20L, p = 30L, q = 10L,
                     family = "gaussian", rho = 0.5,
                     prob_primary = 0.5, signal_strength = 1.0,
                     prob_group = 0.5, prob_predictor = 0.8, seed = 1L)
utils::str(data, vec.len = 2L)
#> List of 7
#>  $ x_train: num [1:50, 1:30] 0.0142 0.2414 ...
#>   ..- attr(*, "dimnames")=List of 2
#>   .. ..$ : chr [1:50] "train_1" "train_2" ...
#>   .. ..$ : chr [1:30] "aux_1.1" "aux_1.2" ...
#>  $ y_train: Named num [1:50] 3.15 0.28 ...
#>   ..- attr(*, "names")= chr [1:50] "train_1" "train_2" ...
#>  $ group  : Named int [1:30] 1 1 1 2 2 ...
#>   ..- attr(*, "names")= chr [1:30] "aux_1.1" "aux_1.2" ...
#>  $ primary: Named logi [1:30] FALSE FALSE TRUE ...
#>   ..- attr(*, "names")= chr [1:30] "aux_1.1" "aux_1.2" ...
#>  $ beta   : Named num [1:30] -0.0449 -0.0162 ...
#>   ..- attr(*, "names")= chr [1:30] "aux_1.1" "aux_1.2" ...
#>  $ x_test : num [1:20, 1:30] NA NA NA NA NA ...
#>   ..- attr(*, "dimnames")=List of 2
#>   .. ..$ : chr [1:20] "test_1" "test_2" ...
#>   .. ..$ : chr [1:30] "aux_1.1" "aux_1.2" ...
#>  $ y_test : Named num [1:20] -0.0501 -1.1288 ...
#>   ..- attr(*, "names")= chr [1:20] "test_1" "test_2" ...
```
