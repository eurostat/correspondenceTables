% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lengthsFile.R
\name{lengthsFile}
\alias{lengthsFile}
\title{Retrieve correspondance tables lenghts for each level tables between classification from CELLAR and FAO repositories}
\usage{
lengthsFile(endpoint, prefix, conceptScheme, correction = TRUE)
}
\arguments{
\item{endpoint}{SPARQL endpoints provide a standardized way to access data sets, 
making it easier to retrieve specific information or perform complex queries on linked data.
The valid values are \code{"CELLAR"} or \code{"FAO"}.}

\item{prefix}{Prefixes are typically defined at the beginning of a SPARQL query 
and are used throughout the query to make it more concise and easier to read. 
Multiple prefixes can be defined in a single query to cover different namespaces used in the dataset.
The function 'classEndpoint()' can be used to generate the prefixes for the selected correspondence table.}

\item{conceptScheme}{Refers to a unique identifier associated to specific classification table. 
The conceptScheme can be obtained by utilizing the "classEndpoint()" function.}

\item{correction}{The valid values are \code{FALSE} or \code{TRUE}. In both cases the lengths table as an R object.
If the output wants to have a correction for hierarchy levels \code{TRUE}. By default is set to "TRUE".}
}
\value{
\code{lenghtsFile()} returns a table containing the lengths for each hierarchical level of the classification.
   \itemize{
      \item charb: contains the length for each code for each hierarchical level
      \item chare: contains the concatenated length of char b for each code for each hierarchical level
}
}
\description{
The aim of this function is to provide a table showing the different levels of hierarchy for each classification and the length of each level.
}
\examples{
{
endpoint = "CELLAR"
prefix = "nace2"
conceptScheme = "nace2"

lengthsTable = lengthsFile(endpoint, prefix, conceptScheme, correction = TRUE)

#View lengthsTable
View(lengthsTable)

} 
}
