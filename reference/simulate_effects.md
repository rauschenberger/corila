# Simulate effects

Simulates effect vector.

## Usage

``` r
.simulate_effects(
  group,
  prob_group = 0.5,
  prob_predictor = 0.8,
  signal_strength = 1,
  seed = 1L
)
```

## Arguments

- group:

  group indicator: integer vector of length \\p\\ with entries between 1
  and \\q\\, where \\p\\ is the number of predictors and \\q\\ is the
  number of predictor groups (maximum length \\1\\000\\, minimum entry
  1, maximum entry \\1\\000\\)

- prob_group:

  probability for each predictor group to be active: numeric scalar in
  the unit interval (minimum 0 makes all groups inactive, maximum 1
  makes all groups active)

- prob_predictor:

  probability for each predictor in an active group to be active:
  numeric scalar in the unit interval (minimum 0 makes all predictors
  inactive, maximum 1 makes all predictors in active groups active)

- signal_strength:

  non-negative numeric scalar for multiplying the effect sizes (default:
  `signal_strength=1.0`, minimum 0 sets all effect sizes to 0, maximum 2
  to avoid undefined values)

- seed:

  random seed for reproducibility: integer scalar (unrestricted)

## Value

Returns a numeric vector of length \\p\\.

## See also

This function is called by
[`simulate_data()`](https://rauschenberger.github.io/corila/reference/simulate_data.md).

## Examples

``` r
group <- rep(c(1L:5L), each = 3L)
.simulate_effects(group = group)
#>  [1]  0.0000000  0.0000000  0.0000000  0.0000000  0.0000000  0.0000000
#>  [7]  0.0000000 -0.4115108 -0.2522234  0.0000000  0.0000000  0.0000000
#> [13]  0.2242679  0.3773956  0.1333364
.simulate_effects(group = group, signal_strength = 1.5)
#>  [1]  0.0000000  0.0000000  0.0000000  0.0000000  0.0000000  0.0000000
#>  [7]  0.0000000 -0.6172662 -0.3783352  0.0000000  0.0000000  0.0000000
#> [13]  0.3364018  0.5660935  0.2000045
```
