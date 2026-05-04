
# --- code for package development ---

rm(list=ls(all.names=TRUE))
#install.packages(c("roxygen2","pkgdown","rcmdcheck","usethis","remotes","testthat","devtools","goodpractice","checkglobals","roxylint"))
setwd("C:/Users/arauschenberger/Desktop/corila/package")
# usethis::use_mit_license()
roxygen2::roxygenise()
rcmdcheck::rcmdcheck()
goodpractice::gp("C:/Users/arauschenberger/Desktop/corila/package")

devtools::spell_check()
checkglobals::checkglobals(pkg=".")

pkgdown::check_pkgdown()
lintr::lint_package()


#devtools::build()
#devtools::submit_cran()
#pkgdown::build_site()

