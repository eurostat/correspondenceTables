<!-- badges: start -->
[![last commit](https://img.shields.io/github/last-commit/eurostat/correspondenceTables?style=flat)](https://github.com/eurostat/correspondenceTables/commits/)
[![R build
status](https://github.com/eurostat/correspondenceTables/workflows/R-CMD-check/badge.svg)](https://github.com/eurostat/correspondenceTables/actions)
[![license](https://img.shields.io/badge/license-EUPL-success)](https://joinup.ec.europa.eu/collection/eupl/eupl-text-eupl-12)
<!-- badges: end -->

# correspondenceTables
Functions for creating or updating correspondence tables between two statistical classifications

## installation

'correspondenceTables' can be installed from source from GitHub
```R
devtools::install_github("eurostat/correspondenceTables")
```

or

```R
remotes::install_github("eurostat/correspondenceTables")
```

## background

This package is a tool to help you in the creation and updates of correspondence table between 2 classification like NACE, CPA, CN, etc.

## content

The package contains 2 functions:
1. `newCorrespondenceTable` to create a candidate correspondence table between two classifications when there are correspondence tables leading from the first classification to the second one via intermediate 'pivot' classifications.
2. `updateCorrespondenceTable` to update the correspondence table between two statistical classifications when one of the classifications gets updated to a new version.


## examples

For the examples see the vignettes.
```R
browseVignettes("correspondenceTables")
```
