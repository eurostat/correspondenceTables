## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----cars---------------------------------------------------------------------
library(correspondenceTables)

## ----results = "hide"---------------------------------------------------------
 AB <- (system.file("extdata", "ab_data.csv", package = "correspondenceTables"))
 A <- (system.file("extdata", "a_data.csv", package = "correspondenceTables"))
 B <- (system.file("extdata", "b_data.csv", package = "correspondenceTables"))

 
 result <- aggregateCorrespondenceTable(AB = AB, A = A, B = B, CSVout = NULL)
 
 print(result)


## ----results = "hide"---------------------------------------------------------

 AB <- (system.file("extdata", "ab_data.csv", package = "correspondenceTables"))
 A <- (system.file("extdata", "a_data.csv", package = "correspondenceTables"))
 B <- (system.file("extdata", "b_data.csv", package = "correspondenceTables"))

 
 result <- aggregateCorrespondenceTable(AB = AB, A = A, B = B, CSVout = NULL)
 
 print(result)

