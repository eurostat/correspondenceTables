## ---- include = FALSE---------------------------------------------------------
library(correspondenceTables)
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## -----------------------------------------------------------------------------
A <- system.file("extdata", "CN2021.csv", package = "correspondenceTables")
AStar <- system.file("extdata", "CN2022.csv", package = "correspondenceTables")
B <- system.file("extdata", "CPA21.csv", package = "correspondenceTables")
AB <- system.file("extdata", "CN2021_CPA21.csv", package = "correspondenceTables")
AAStar <- system.file("extdata", "CN2021_CN2022.csv", package = "correspondenceTables")

## -----------------------------------------------------------------------------
UPC <- updateCorrespondenceTable(A, B, AStar, AB, AAStar, "updateCorrespondenceTableCase1.csv",
                                 "B", 0.4, 0.4)
print(UPC[[1]][1:10, 1:7])
print(UPC[[2]])

## -----------------------------------------------------------------------------
A <- system.file("extdata", "CN2021.csv", package = "correspondenceTables")
AStar <- system.file("extdata", "CN2022.csv", package = "correspondenceTables")
B <- system.file("extdata", "PRODCOM2021.csv", package = "correspondenceTables")
AB <- system.file("extdata", "CN2021_PRODCOM2021.csv", package = "correspondenceTables")
AAStar <- system.file("extdata", "CN2021_CN2022.csv", package = "correspondenceTables")

## ---- results = "hide"--------------------------------------------------------
UPC <- updateCorrespondenceTable(A, B, AStar, AB, AAStar, "updateCorrespondenceTableCase2.csv", "A", 0.4, 0.3)

## -----------------------------------------------------------------------------
A <- system.file("extdata", "NAICS2017.csv", package = "correspondenceTables")
AStar <- system.file("extdata", "NAICS2022.csv", package = "correspondenceTables")
B <- system.file("extdata", "NACE.csv", package = "correspondenceTables")
AB <- system.file("extdata", "NAICS2017_NACE.csv", package = "correspondenceTables")
AAStar <- system.file("extdata", "NAICS2017_NAICS2022.csv", package = "correspondenceTables")

## ---- results = "hide"--------------------------------------------------------
UPC <- updateCorrespondenceTable(A, B, AStar, AB, AAStar, "updateCorrespondenceTableCase3.csv", "none", 0.5, 0.3)

## -----------------------------------------------------------------------------
A <- system.file("extdata", "CN2021.csv", package = "correspondenceTables")
AStar <- system.file("extdata", "CN2022.csv", package = "correspondenceTables")
B <- system.file("extdata", "NST2007.csv", package = "correspondenceTables")
AB <- system.file("extdata", "CN2021_NST2007.csv", package = "correspondenceTables")
AAStar <- system.file("extdata", "CN2021_CN2022.csv", package = "correspondenceTables")

## ---- results = "hide"--------------------------------------------------------
UPC <- updateCorrespondenceTable(A, B, AStar, AB, AAStar, "updateCorrespondenceTableCase4.csv", "B", 0.4, 0.3)

## -----------------------------------------------------------------------------
A <- system.file("extdata", "CN2021.csv", package = "correspondenceTables")
AStar <- system.file("extdata", "CN2022.csv", package = "correspondenceTables")
B <- system.file("extdata", "SITC4.csv", package = "correspondenceTables")
AB <- system.file("extdata", "CN2021_SITC4.csv", package = "correspondenceTables")
AAStar <- system.file("extdata", "CN2021_CN2022.csv", package = "correspondenceTables")

## ---- results = "hide"--------------------------------------------------------
UPC <- updateCorrespondenceTable(A, B, AStar, AB, AAStar, "updateCorrespondenceTableCase5.csv", "B", 0.3, 0.7)

## -----------------------------------------------------------------------------
A <- system.file("extdata", "CN2021.csv", package = "correspondenceTables")
AStar <- system.file("extdata", "CN2022.csv", package = "correspondenceTables")
B <- system.file("extdata", "BEC4.csv", package = "correspondenceTables")
AB <- system.file("extdata", "CN2021_BEC4.csv", package = "correspondenceTables")
AAStar <- system.file("extdata", "CN2021_CN2022.csv", package = "correspondenceTables")

## ---- results = "hide"--------------------------------------------------------
UPC <- updateCorrespondenceTable(A, B, AStar, AB, AAStar, "updateCorrespondenceTableCase6.csv", "B", 0.3, 0.6)

## -----------------------------------------------------------------------------
fullPath <- function(CSVraw, CSVappended){
  NamesCsv <- system.file("extdata", CSVraw, package = "correspondenceTables")
  A <- read.csv(NamesCsv, header = FALSE, sep = ",")
   for (i in 1:nrow(A)) {
    for (j in 1:ncol(A)) {
      if (A[i,j]!="") {
        A[i, j] <- system.file("extdata", A[i, j], package = "correspondenceTables")
      }}}
  write.table(x = A, file = CSVappended, row.names = FALSE, col.names = FALSE, sep = ",")
  return(A)
}

## ---- results = "hide"--------------------------------------------------------
fullPath("names1.csv", "names.csv")

## ---- results = "hide"--------------------------------------------------------
NCT <- newCorrespondenceTable("names.csv", "newCorrespondenceTableCase1.csv", "A", 0.5)

## -----------------------------------------------------------------------------
print(NCT[[1]][1:10, 1:6])
print(NCT[[2]])

## ---- results = "hide"--------------------------------------------------------
fullPath("names2.csv", "names.csv")

## ---- results = "hide"--------------------------------------------------------
NCT <- newCorrespondenceTable("names.csv", "newCorrespondenceTableCase2.csv", "B", 0.5)

## ---- results = "hide"--------------------------------------------------------
fullPath("names3.csv", "names.csv")

## ---- results = "hide"--------------------------------------------------------
NCT <- newCorrespondenceTable("names.csv", "newCorrespondenceTableCase3.csv", "B", 0.5)

## ---- results = "hide"--------------------------------------------------------
fullPath("names4.csv", "names.csv")

## ---- results = "hide"--------------------------------------------------------
NCT <- newCorrespondenceTable("names.csv", "newCorrespondenceTableCase4.csv", "none", 0.96)

