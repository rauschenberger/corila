# Data simulation

Simulates data with grouped predictor variables.

## Usage

``` r
simulate(
  family = "gaussian",
  n0 = 100L,
  n1 = 10000L,
  n_group = 20L,
  n_type = 2L,
  size_group = c(5L, 3L),
  effect_size = c(1, 1),
  corfac_feature = 0.5,
  corfac_type = 0.5,
  corfac_group = 0.25,
  n_group_causal = 2L,
  prop_causal = 0.5,
  noise_factor = 1,
  plot = FALSE,
  trial = FALSE
)
```

## Arguments

- family:

  character `"gaussian"`, `"binomial"`, `"poisson"`, or `"cox"`

- n0:

  number of training observations (positive integer)

- n1:

  number of testing observations (positive integer)

- n_group:

  number of variable groups (positive integer)

- n_type:

  number of variable types (positive integer)

- size_group:

  size of variable groups (per variable type): integer vector of length
  `n_type`

- effect_size:

  effect sizes (per variable type): numeric vector of length `n_type`

- corfac_feature:

  decrease of correlation if different variable: scalar in unit interval

- corfac_type:

  decrease of correlation if different type: scalar in unit interval

- corfac_group:

  decrease of correlation if different group: scalar in unit interval

- n_group_causal:

  number of causal groups: integer

- prop_causal:

  proportion of causal features within causal groups: scalar in unit
  interval

- noise_factor:

  noise factor: numeric scalar

- plot:

  Attempt to visualise effects of and correlation between variables?
  (`TRUE` or `FALSE`)

- trial:

  logical (groups of negatively correlated subgroups)

## Value

Returns a list with the following slots:

- \\n_0 \times p\\ matrix `x_train`

- \\p\\-dimensional vector `type`

- \\p\\-dimensional vector `group`

- \\n_0\\-dimensional vector `y_train`

- \\n_1 \times p\\ matrix `x_test`

- \\n_1\\-dimensional vector `y_test`

- \\p\\-dimensional vector `beta`

- data frame `info` with entries \\n_0\\, \\n_1\\, \\p\\, `n_type`,
  `n_group`, and `family`

## Examples

``` r
data <- corila:::simulate()
dims <- function(x) {
   if (is.matrix(x)||is.data.frame(x)) {
     paste(base::dim(x), collapse = " x ")
   } else {
     paste0(base::length(x))
   }
}
sapply(X = data, FUN = dims)
#>       x_train          type         group       y_train        x_test 
#>   "100 x 160"         "160"         "160"         "100" "10000 x 160" 
#>        y_test          beta          info 
#>       "10000"         "160"       "1 x 6" 
```
