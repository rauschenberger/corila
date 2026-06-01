
[![R-CMD-check](https://github.com/rauschenberger/corila/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/rauschenberger/corila/actions/workflows/R-CMD-check.yaml)
[![lint](https://github.com/rauschenberger/corila/actions/workflows/lint.yaml/badge.svg)](https://github.com/rauschenberger/corila/actions/workflows/lint.yaml)
[![cyclocomp](https://github.com/rauschenberger/corila/actions/workflows/cyclocomp.yaml/badge.svg)](https://github.com/rauschenberger/corila/actions/workflows/cyclocomp.yaml)
[![autotest](https://github.com/rauschenberger/corila/actions/workflows/autotest.yaml/badge.svg)](https://github.com/rauschenberger/corila/actions/workflows/autotest.yaml)
[![goodpractice](https://github.com/rauschenberger/corila/actions/workflows/goodpractice.yaml/badge.svg)](https://github.com/rauschenberger/corila/actions/workflows/goodpractice.yaml)
[![pkgcheck](https://github.com/rauschenberger/corila/actions/workflows/pkgcheck.yaml/badge.svg)](https://github.com/rauschenberger/corila/actions/workflows/pkgcheck.yaml)
[![Codecov test coverage](https://codecov.io/gh/rauschenberger/corila/graph/badge.svg)](https://app.codecov.io/gh/rauschenberger/corila)

# Sparse modelling with grouped and correlated features

## Scope

The R package `corila` implements sparse modelling with grouped and correlated features allowing for privileged information (_Rauschenberger_, 2026.)

##  Installation

Install the latest development version from [GitHub](https://github.com/rauschenberger/corila):

``` r
#install.packages("remotes")
remotes::install_github("rauschenberger/corila")
```

## Usage

Use the function `cv.corila` to model an outcome (`n`-dimensional vector _y_) based on many predictors (`n x p` matrix _X_) that are structured by groups (e.g., `p`-dimensional vector `group`) and potentially split into primary and auxiliary predictors (`p`-dimensional vector `include`). See the vignette for detailed examples.

``` r
library(corila)
object <- cv.corila(x = x_train, y = y_train, group = group, include = include)
coef(object)
predict(object, newx = x_test)
```

## Reference

Armin Rauschenberger 
[![AR](https://info.orcid.org/wp-content/uploads/2019/11/orcid_16x16.png)](https://orcid.org/0000-0001-6498-4801)
(2026).
"Sparse modelling with grouped and correlated features allowing for privileged information".
*Manuscript in preparation*.

## Disclaimer

This public repository is on a personal GitHub account, but it has private pull mirrors on two institutional GitLab instances (see [LIH](https://git.lih.lu/arauschenberger/corila) and [LCSB](https://gitlab.com/uniluxembourg/Personalfolders/armin.rauschenberger/corila)).

**Copyright** &copy; 2025 Armin Rauschenberger; Luxembourg Institute of Health (LIH), Department of Medical Informatics (DMI), Bioinformatics and Artificial Intelligence (BioAI); University of Luxembourg, Luxembourg Centre for Systems Biomedicine (LCSB), Biomedical Data Science (BDS). This R package is distributed under an [GPL 3 license](LICENSE.md).
