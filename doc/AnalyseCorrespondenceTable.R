## ----include=FALSE------------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## -----------------------------------------------------------------------------
library(correspondenceTables)

## ----results='hide'-----------------------------------------------------------
# Perform analysis
result <- analyseCorrespondenceTable(AB =system.file("extdata", "ExempleAnnexe.csv", package = "correspondenceTables")
                                     , A = NULL, longestAcodeOnly = FALSE, B = NULL, longestBcodeOnly = FALSE,
                                     CSVcorrespondenceInventory = NULL, CSVcorrespondenceAnalysis = NULL)



# Print the results
print(result$Inventory)
print(result$Analysis)


## -----------------------------------------------------------------------------
result2 <-analyseCorrespondenceTable(AB = system.file("extdata", "ExempleAnnexe.csv", package = "correspondenceTables"), 
                                     A =  NULL , longestAcodeOnly = FALSE , B = NULL, longestBcodeOnly = FALSE, 
                                     CSVcorrespondenceInventory = NULL, CSVcorrespondenceAnalysis = NULL)

print(result2$Inventory)
###for Inventory run this code : View(result2$Inventory)
print(result2$Analysis)

