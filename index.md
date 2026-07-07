# Sparse modelling with grouped and correlated features

## Scope

The R package `corila` implements sparse modelling with grouped and
correlated features allowing for privileged information
(*Rauschenberger*, 2026).

## Installation

Install the latest development version from
[GitHub](https://github.com/rauschenberger/corila):

``` r

#install.packages("remotes")
remotes::install_github("rauschenberger/corila")
```

## Usage

Use the function
[`cv.corila`](https://rauschenberger.github.io/corila/reference/cv.corila.html)
to model an outcome (`n`-dimensional vector *y*) based on many
predictors (`n x p` matrix *X*) that are structured by groups (e.g.,
`p`-dimensional vector `group`) and potentially split into primary and
auxiliary predictors (`p`-dimensional vector `primary`). See the
[vignette](https://rauschenberger.github.io/corila/articles/vignette.html)
for detailed examples.

``` r

library(corila)
#load(toydata)
object <- cv.corila(x = x_train, y = y_train, group = group, primary = primary)
coef(object)
predict(object, newx = x_test)
```

## Reference

Armin Rauschenberger
[![AR](https://info.orcid.org/wp-content/uploads/2019/11/orcid_16x16.png)](https://orcid.org/0000-0001-6498-4801)
(2026). “Sparse modelling with grouped and correlated features allowing
for privileged information”. *Manuscript in preparation*.

(Presented at [SIS-FENStatS](https://sis2026.sis-statistica.it/) in
Rome, Italy, on 25 June 2026.)

## Disclaimer

This public repository is on a personal GitHub account, but it has
private pull mirrors on two institutional GitLab instances (see
https://git.lih.lu/arauschenberger/corila and
https://gitlab.com/uniluxembourg/Personalfolders/armin.rauschenberger/corila)).

Large-language models (mainly Claude Sonnet 4.6 and GPT-5.4) were used
for reviewing R code and documentation and for adapting configuration
files (`.yaml`).

**Copyright** © 2025 Armin Rauschenberger; Luxembourg Institute of
Health (LIH), Department of Medical Informatics (DMI), Bioinformatics
and Artificial Intelligence (BioAI); University of Luxembourg,
Luxembourg Centre for Systems Biomedicine (LCSB), Biomedical Data
Science (BDS). This R package is distributed under a [GPL 3
license](https://rauschenberger.github.io/corila/LICENSE.md).
