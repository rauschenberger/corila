
<!-- badges: start -->
[![R-CMD-check](https://github.com/rauschenberger/corila/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/rauschenberger/corila/actions/workflows/R-CMD-check.yaml)
[![lint](https://github.com/rauschenberger/corila/actions/workflows/lint.yaml/badge.svg)](https://github.com/rauschenberger/corila/actions/workflows/lint.yaml)
[![cyclocomp](https://github.com/rauschenberger/corila/actions/workflows/cyclocomp.yaml/badge.svg)](https://github.com/rauschenberger/corila/actions/workflows/cyclocomp.yaml)
[![autotest](https://github.com/rauschenberger/corila/actions/workflows/autotest.yaml/badge.svg)](https://github.com/rauschenberger/corila/actions/workflows/autotest.yaml)
[![goodpractice](https://github.com/rauschenberger/corila/actions/workflows/goodpractice.yaml/badge.svg)](https://github.com/rauschenberger/corila/actions/workflows/goodpractice.yaml)
[![pkgcheck](https://github.com/rauschenberger/corila/actions/workflows/pkgcheck.yaml/badge.svg)](https://github.com/rauschenberger/corila/actions/workflows/pkgcheck.yaml)
[![Codecov test coverage](https://codecov.io/gh/rauschenberger/corila/graph/badge.svg)](https://app.codecov.io/gh/rauschenberger/corila)
[![life cycle: experimental](https://img.shields.io/badge/lifecycle-experimental-lightgray.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN status](https://img.shields.io/badge/CRAN-not%20yet%20published-lightgray)](https://github.com/rauschenberger/corila)
<!-- badges: end -->

# Sparse modelling with grouped and correlated features

## Scope

The R package `corila` implements sparse modelling with grouped and correlated features allowing for privileged information (_Rauschenberger_, 2026).

##  Installation

Install the latest development version from [GitHub](https://github.com/rauschenberger/corila):

``` r
#install.packages("remotes")
remotes::install_github("rauschenberger/corila")
```

## Usage

Use the function [`cv.corila`](https://rauschenberger.github.io/corila/reference/cv.corila.html) to model an outcome (`n`-dimensional vector _y_) based on many predictors (`n x p` matrix _X_) that are structured by groups (e.g., `p`-dimensional vector `group`) and potentially split into primary and auxiliary predictors (`p`-dimensional vector `primary`). See the [vignette](https://rauschenberger.github.io/corila/articles/vignette.html) for detailed examples.

``` r
library(corila)
data <- simulate_data()
object <- cv.corila(x = data$x_train,
                    y = data$y_train,
                    group = data$group,
                    primary = data$primary)
coef(object)
predict(object, newx = data$x_test)
```

## Reference

Armin Rauschenberger 
[![AR](https://info.orcid.org/wp-content/uploads/2019/11/orcid_16x16.png)](https://orcid.org/0000-0001-6498-4801)
(2026).
"Sparse modelling with grouped and correlated features allowing for privileged information".
*Manuscript in preparation*.

(Presented at [SIS-FENStatS](https://sis2026.sis-statistica.it/) in Rome, Italy, on 25 June 2026.)

## Disclaimer

This public repository is on a personal GitHub account, but it has private pull mirrors on two institutional GitLab instances (see https<wbr>://<wbr>git.<wbr>lih.<wbr>lu/<wbr>arauschenberger/<wbr>corila and https<wbr>://<wbr>gitlab.<wbr>com/<wbr>uniluxembourg/<wbr>Personalfolders/<wbr>armin.<wbr>rauschenberger/<wbr>corila)).

<!--
These links are not clickable because urlchecker would complain about forbidden access:
https://git.lih.lu/arauschenberger/corila
https://gitlab.com/uniluxembourg/Personalfolders/armin.rauschenberger/corila
--->

Large-language models (mainly Claude Sonnet 4.6 and GPT-5.4) were used for reviewing R code and documentation and for adapting configuration files (`.yaml`).

**Copyright** &copy; 2025 Armin Rauschenberger; Luxembourg Institute of Health (LIH), Department of Medical Informatics (DMI), Bioinformatics and Artificial Intelligence (BioAI); University of Luxembourg, Luxembourg Centre for Systems Biomedicine (LCSB), Biomedical Data Science (BDS). This R package is distributed under a [GPL 3 license](LICENSE.md).
