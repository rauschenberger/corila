# Multi-penalty ridge regression

Fits multi-penalty ridge regression (tuning regularisation
hyperparameters and estimating regression coefficients). This is a
wrapper function of some functions from the
[multiridge-package](https://rdrr.io/pkg/multiridge/man/multiridge-package.html).

## Usage

``` r
multiridge(
  x,
  y,
  group,
  family = "gaussian",
  foldid = NULL,
  nfolds = 10L,
  penalties = NULL
)
```

## Arguments

- x:

  predictors: \\n \times p\\ numeric matrix

- y:

  response: \\n\\-dimensional vector

- group:

  \\p\\-dimensional integer vector with entries in \\\\1, \ldots, q\\\\

- family:

  character `"gaussian"` (not `"linear"`), `"binomial"` (not
  `"logistic"`), or `"cox"`

- foldid:

  \\n_0\\-dimensional vector containing the fold identifiers (minimum
  \\1\\, maximum `nfolds`)

- nfolds:

  positive integer specifying the number of folds (minimum \\3\\,
  maximum \\n\\)

  NB: If `foldid` is provided, `nfolds` is overwritten by `max(foldid)`.

- penalties:

  \\q\\-dimensional vector of non-negative penalty parameters, or `NULL`
  (cross-validation)

## Value

Returns an object of class `"multiridge"`, a list with the following
slots:

- slots from
  [IWLSridge()](https://rdrr.io/pkg/multiridge/man/IWLSridge.html) or
  [IWLSCoxridge()](https://rdrr.io/pkg/multiridge/man/IWLSCoxridge.html)

- character `family` with value `"gaussian"` (also for `"linear"`),
  `"binomial"` (also for `"logistic"`), or `"cox"`

- \\q\\-dimensional vector `penalties` containing optimised
  regularisation hyperparameters (one for each predictor group)

- list `indices` with `nfolds` slots (one for each cross-validation
  fold), each containing the indices of the observations

- list `datablocks` with \\q\\ slots (one for each predictor group),
  each containing an \\n_0 \times p_k\\ matrix, where \\k \in \\1,
  \ldots, q\\\\

- \\p\\-dimensional group vector `group` (see argument)

- list `pars` with slots `family` (see above), the \\p\\-dimensional
  vectors `mu.x` and `sd.x` and the scalars `mu.y` and `sd.y`

## Details

The numbers of observations (samples) for training or testing are
indicated by \\n_0\\ and \\n_1\\, respectively, the number of predictors
(features) is indicated by \\p\\, and the number of predictor group is
indicated by \\q\\. Observations are indexed by \\i\\ in \\\\1, \ldots,
n\\\\, predictors are indexed by \\j\\ in \\\\1, \ldots, p\\\\, and
predictor groups are indexed by \\k\\ in \\\\1, \ldots, q\\\\. The
number of predictors in the \\k^{\text{th}}\\ group is indicated by
\\p_k\\, with \\\sum\_{k=1}^q p_k = p\\ for non-overlapping groups.

## References

[Mark A. van de Wiel](https://orcid.org/0000-0003-4780-8472), [Mirrelijn
M. van Nee](https://orcid.org/0000-0001-7715-1446) and [Armin
Rauschenberger](https://orcid.org/0000-0001-6498-4801) (2021). "Fast
cross-validation for multi-penalty high-dimensional ridge regression"
*Journal of Computational and Graphical Statistics* 30(4):835-847.
[doi:10.1080/10618600.2021.1904962](https://doi.org/10.1080/10618600.2021.1904962)
.

## See also

Extract coefficients with
[coef()](https://rauschenberger.github.io/corila/reference/coef.multiridge.md)
or make predictions with
[predict()](https://rauschenberger.github.io/corila/reference/predict.multiridge.md).
Use
[`cv.corila()`](https://rauschenberger.github.io/corila/reference/cv.corila.md)
to estimate sparse models.

This wrapper function calls various functions from the
[multiridge-package](https://rdrr.io/pkg/multiridge/man/multiridge-package.html),
namely
[createXXblocks()](https://rdrr.io/pkg/multiridge/man/createXXblocks.html),
[fastCV2()](https://rdrr.io/pkg/multiridge/man/fastCV2.html),
[CVfolds()](https://rdrr.io/pkg/multiridge/man/CVfolds.html),
[optLambdasWrap()](https://rdrr.io/pkg/multiridge/man/optLambdasWrap.html),
[SigmaFromBlocks()](https://rdrr.io/pkg/multiridge/man/SigmaFromBlocks.html),
[IWLSridge()](https://rdrr.io/pkg/multiridge/man/IWLSridge.html), and
[IWLSCoxridge()](https://rdrr.io/pkg/multiridge/man/IWLSCoxridge.html).

The
[multiridge-package](https://rdrr.io/pkg/multiridge/man/multiridge-package.html)
accepts not only an \\n \times p\\ matrix but also a list of length
\\q\\ of \\n \times p_k\\ matrices, with \\k\\ in \\\\1, \ldots, q\\\\.

## Examples

``` r
warning("Re-activate examples.")
#> Warning: Re-activate examples.
data <- simulate_data()

## standard model fitting
#model <- multiridge(x = data$x_train, y = data$y_train, group = data$group)

## fitting with given folds
#foldid <- sample(seq_len(10L), size = nrow(data$x_train), replace = TRUE)
#model <- multiridge(x = data$x_train, y = data$y_train, group = data$group,
#                    foldid = foldid)

## fitting with given penalties
#penalties <- abs(rnorm(length(unique(data$group))))
#model <- multiridge(x = data$x_train, y = data$y_train, group = data$group,
#                    penalties = penalties)
```
