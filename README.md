<!-- badges: start -->
[![last commit](https://img.shields.io/github/last-commit/eurostat/correspondenceTables?style=flat)](https://github.com/eurostat/correspondenceTables/commits/)
[![R build
status](https://github.com/eurostat/correspondenceTables/workflows/R-CMD-check/badge.svg)](https://github.com/eurostat/correspondenceTables/actions)
[![dependencies](https://tinyverse.netlify.com/badge/correspondenceTables)](https://CRAN.R-project.org/package=correspondenceTables)
[![CRAN version](https://www.r-pkg.org/badges/version/correspondenceTables)](https://CRAN.R-project.org/package=correspondenceTables)
[![CRAN status](https://cranchecks.info/badges/flavor/release/correspondenceTables)](https://cran.r-project.org/web/checks/check_results_correspondenceTables.html)
[![license](https://img.shields.io/badge/license-EUPL-success)](https://joinup.ec.europa.eu/collection/eupl/eupl-text-eupl-12)
[![weekly downloads](https://cranlogs.r-pkg.org/badges/last-week/correspondenceTables)](https://CRAN.R-project.org/package=correspondenceTables)
[![monthly downloads](https://cranlogs.r-pkg.org/badges/correspondenceTables)](https://CRAN.R-project.org/package=correspondenceTables)
[![all downloads](https://cranlogs.r-pkg.org/badges/grand-total/correspondenceTables)](https://CRAN.R-project.org/package=correspondenceTables)
<!-- badges: end -->

# CorrespondenceTables

Empowering the seamless creation and refinement of correspondence tables between two statistical classifications, such as NACE, CPA, CN, and more.
This toolkit not only streamlines the process of generating and updating these tables but also provides functionality for retrieving classification and correspondence tables.
It includes features for conducting quality control on classifications and offers a comprehensive analysis of correspondence tables with the ability to aggregate them effectively.

## Installation

'correspondenceTables' can be installed from [CRAN](https://CRAN.R-project.org/package=correspondenceTables) by 

```R
install.packages("correspondenceTables")
```

or use the development version from GitHub

```R
devtools::install_github("eurostat/correspondenceTables")
```

## Background

This package serves as a tool to assist you in creating and updating a correspondence table between two classifications, such as NACE, CPA, CN, and others. It enables the retrieval of classification tables and correspondence tables. Additionally, it facilitates quality control on a classification and allows for the analysis and aggregation of correspondence tables.

## Content

The package contains 11 functions:

1. `newCorrespondenceTable` to create a candidate correspondence table between two classifications when there are correspondence tables leading from the first classification to the second one via intermediate 'pivot' classifications.

2. `updateCorrespondenceTable` to update the correspondence table between two statistical classifications when one of the classifications gets updated to a new version.

3. `prefixList` Create a list of prefixes for both CELLAR and FAO repositories.

4. `CorrespondenceList` Overview of all the available correspondence classification from CELLAR and FAO repository.

5. `dataStructure`  Retrieve information about the structure of each classification tables from CELLAR and FAO repositories.

6. `classificationEndpoint` Retrieve a list of classification tables from CELLAR and FAO repositories or both.

7. `retrieveClassificationTable` Retrieve stastistical classification tables from CELLAR and FAO repositories.

8. `retrieveCorrespondenceTable.` Retrieve correspondence tables between statistical classifications from CELLAR and FAO repositories.

9. `classificationQC` performs quality control checks on statistical classifications.

10. `analyseCorrespondancetable` performs analysis on correspondence tables.  

11. `aggregateCorrespondenceTable` aggregates correspondence tables to higher hierarchical levels.

## Examples

For the examples see the vignettes.
```R
browseVignettes("correspondenceTables")
```
