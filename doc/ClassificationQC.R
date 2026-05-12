## ----include=FALSE------------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## -----------------------------------------------------------------------------
library(correspondenceTables)

## ----results = "hide"---------------------------------------------------------
lengths <- read.csv( system.file("extdata", "lenghtsNaceNoColumn.csv", package = "correspondenceTables"))
## Here lengths has no column
colnames(lengths)
singleChildCode <- read.csv(system.file("extdata","SingleChild.csv", package = "correspondenceTables"))
# SingleChildCode has the good column
colnames(singleChildCode)
sequencing <- read.csv(system.file("extdata","Sequencing.csv", package = "correspondenceTables"))
#Sequencing has string as a column it will be replaced see next chunk 
colnames(sequencing)



## ----results = "hide"---------------------------------------------------------
output <- classificationQC(classification = system.file("extdata", "Nace2.csv", package ="correspondenceTables"),
                             lengths = system.file("extdata", "lenghtsNace.csv", package = "correspondenceTables"),
                             fullHierarchy = TRUE,
                             labelUniqueness  = TRUE, labelHierarchy = TRUE,
                             singleChildCode = NULL, sequencing = NULL,
                             CSVout = NULL) 


output$QC_output

## ----results = "hide"---------------------------------------------------------
output2 <-  classificationQC(
            classification = system.file("extdata", "Nace2.csv", package ="correspondenceTables"),
            lengths = system.file("extdata", "lenghtsNace.csv", package = "correspondenceTables"),
            fullHierarchy = TRUE,
            labelUniqueness  = TRUE,
            labelHierarchy = TRUE,
            singleChildCode = system.file("extdata","SingleChild.csv", package = "correspondenceTables"),            
            sequencing = NULL,
            CSVout = NULL)

output2$QC_multipleCodeError
output2$QC_output


## ----results = "hide"---------------------------------------------------------

sequencing = data.frame(
  level = c(3, 4),
  multipleCode = c("1,2,3", "1,2,3")
)

output3 <-  classificationQC(
            classification = system.file("extdata", "Nace2.csv", package ="correspondenceTables"),
            lengths = system.file("extdata", "lenghtsNace.csv", package = "correspondenceTables"),
            fullHierarchy = TRUE,
            labelUniqueness  = TRUE,
            labelHierarchy = TRUE,
            singleChildCode = system.file("extdata","SingleChild2.csv", package = "correspondenceTables"),
            sequencing = sequencing,
            CSVout = NULL)

output3$QC_output
output3$QC_gapBefore
output3$QC_lastSibling

