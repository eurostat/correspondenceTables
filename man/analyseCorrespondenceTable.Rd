% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/analyseCorrespondancetable.R
\name{analyseCorrespondenceTable}
\alias{analyseCorrespondenceTable}
\title{analyseCorrespondenceTable performs analysis on correspondence tables}
\usage{
analyseCorrespondenceTable(
  AB,
  A = NULL,
  longestAcodeOnly = FALSE,
  B = NULL,
  longestBcodeOnly = FALSE,
  CSVcorrespondenceInventory = NULL,
  CSVcorrespondenceAnalysis = NULL
)
}
\arguments{
\item{AB}{a mandatory argument containing a CSV file provide by the user contains the correspondence table data with columns "Acode" and "Bcode".}

\item{A}{a path to a CSV file containing source classification data with "Acode" column.}

\item{longestAcodeOnly}{A Boolean argument to filter source classification data based on "Acode" retaining only the maximum length, thus the lowest level Acode .}

\item{B}{a path to a CSV file containing target classification data with "Bcode" column.}

\item{longestBcodeOnly}{A Boolean argument to filter source classification data based on "Bcode" retaining only the maximum length, thus the lowest level Bcode.}

\item{CSVcorrespondenceInventory}{The valid values are not NULL if the user put a path with a empty csv file it will return it with the correspondeceInventory or just a path with a csv file . By default no CSV is produce}

\item{CSVcorrespondenceAnalysis}{Provide an output containing the correpondenceAnalysis. the user put a path a empty file it will return with correpondenceAnalysis. by default no CSV is produce}
}
\value{
A list containing two data frames: Inventory and Analysis.
The `CSVcorrespondenceInventory` contains statistics related to components, correspondence types, and source/target positions.
The `CSVcorrespondenceAnalysis` contains statistics for each class in the correspondence table.
}
\description{
The `analyseCorrespondenceTable` function takes input correspondence tables (AB) and related data (A and B) to perform analysis and generate various statistics.
It checks the validity of the input data, identifies components, calculates correspondence types, and creates summary tables.
}
\examples{
# Use data from the folder extdata


  





# Perform analysis
result <- analyseCorrespondenceTable(AB =system.file("extdata", "ExempleAnnexe.csv", package = "correspondenceTables"),A = NULL, longestAcodeOnly = FALSE, B = NULL, longestBcodeOnly = FALSE, CSVcorrespondenceInventory = NULL, CSVcorrespondenceAnalysis = NULL) 
print(result$Inventory)
print(result$Analysis)
}
