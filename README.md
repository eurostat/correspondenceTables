<!-- badges: start -->
[![last commit](https://img.shields.io/github/last-commit/eurostat/correspondenceTables?style=flat)](https://github.com/eurostat/correspondenceTables/commits/)
[![R build
status](https://github.com/eurostat/correspondenceTables/workflows/R-CMD-check/badge.svg)](https://github.com/eurostat/correspondenceTables/actions)
[![dependencies](https://tinyverse.netlify.com/badge/correspondenceTables)](https://CRAN.R-project.org/package=correspondenceTables)
[![CRAN version](https://www.r-pkg.org/badges/version/correspondenceTables)](https://CRAN.R-project.org/package=correspondenceTables)
[![CRAN status](https://badges.cranchecks.info/summary/correspondenceTables.svg)](https://cran.r-project.org/web/checks/check_results_correspondenceTables.html)
[![license](https://img.shields.io/badge/license-EUPL-success)](https://joinup.ec.europa.eu/collection/eupl/eupl-text-eupl-12)
[![weekly downloads](https://cranlogs.r-pkg.org/badges/last-week/correspondenceTables)](https://CRAN.R-project.org/package=correspondenceTables)
[![monthly downloads](https://cranlogs.r-pkg.org/badges/correspondenceTables)](https://CRAN.R-project.org/package=correspondenceTables)
[![all downloads](https://cranlogs.r-pkg.org/badges/grand-total/correspondenceTables)](https://CRAN.R-project.org/package=correspondenceTables)
<!-- badges: end -->

# correspondenceTables

Creating or updating correspondence tables between two statistical classifications

## installation

'correspondenceTables' can be installed from [CRAN](https://CRAN.R-project.org/package=correspondenceTables) by 

```R
install.packages("correspondenceTables")
```

or use the development version from GitHub

```R
devtools::install_github("eurostat/correspondenceTables", build_vignettes = TRUE)
```

## background

This package is a tool to help you in the creation and updates of a correspondence table between 2 classification like NACE, CPA, CN, etc.

## content

The package contains 2 functions:

1. `newCorrespondenceTable` to create a candidate correspondence table between two classifications when there are correspondence tables leading from the first classification to the second one via intermediate 'pivot' classifications.

2. `updateCorrespondenceTable` to update the correspondence table between two statistical classifications when one of the classifications gets updated to a new version.


## examples

For the examples see the vignettes.
```R
browseVignettes("correspondenceTables")
```
