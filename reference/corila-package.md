# Sparse modelling with grouped and correlated features allowing for privileged information

The R package `corila` implements "Sparse modelling with grouped and
correlated features allowing for privileged information"
(*Rauschenberger, 2026*). This is the first implementation of a novel
algorithm. It builds upon adaptive lasso regression with the
[glmnet-package](https://glmnet.stanford.edu/reference/glmnet-package.html).

## Details

Use function
[`cv.corila()`](https://rauschenberger.github.io/corila/reference/cv.corila.md)
for model fitting. Type
[`library(corila)`](https://github.com/rauschenberger/corila) and then
[`?cv.corila`](https://rauschenberger.github.io/corila/reference/cv.corila.md)
or
[`help("cv.corila")`](https://rauschenberger.github.io/corila/reference/cv.corila.md)
to open its help file.

See the vignette for further examples. Type `vignette("corila")` or
`browseVignettes("corila")` to open the vignette.

This package also includes the wrapper function
[`multiridge()`](https://rauschenberger.github.io/corila/reference/multiridge.md)
for multi-penalty ridge regression with the
[multiridge-package](https://rdrr.io/pkg/multiridge/man/multiridge-package.html).

## References

[Armin Rauschenberger](https://orcid.org/0000-0001-6498-4801) (2026).
"Sparse modelling with grouped and correlated features allowing for
privileged information". *In preparation*.

## See also

First use
[`cv.corila()`](https://rauschenberger.github.io/corila/reference/cv.corila.md)
to fit the model, and then
[coef()](https://rauschenberger.github.io/corila/reference/coef.cv.corila.md)
to extract coefficients or
[predict()](https://rauschenberger.github.io/corila/reference/predict.cv.corila.md)
to make predictions.

## Author

[Armin Rauschenberger](https://orcid.org/0000-0001-6498-4801)

## Examples

``` r
?cv.corila
?coef.cv.corila
?predict.cv.corila
```
