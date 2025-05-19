#' @title Retrieve information about the structure of each classification tables from CELLAR and FAO repositories.
#' @description Retrieve information, for all the classification available in the repositories (CELLAR and FAO),
#' about the level names their hierarchy and the numbers of records the function "dataStructure()" can be used.
#' @param endpoint SPARQL endpoints provide a standardized way to access data sets, 
#' making it easier to retrieve specific information or perform complex queries on linked data.
#' The valid values are \code{"CELLAR"} or \code{"FAO"}.
#' @param prefix Prefixes are typically defined at the beginning of a SPARQL query 
#' and are used throughout the query to make it more concise and easier to read. 
#' Multiple prefixes can be defined in a single query to cover different namespaces used in the data set.
#' The function 'classificationList()' can be used to generate the prefixes for the selected classification table. 
#' @param conceptScheme Refers to a unique identifier associated to specific classification table. 
#' The conceptScheme can be obtained by utilizing the "classificationList()" function.
#' @param language Refers to the specific language used for providing label, include and exclude information in the selected classification table. 
#' By default is set to "en". This is an optional argument. 
#' @param showQuery The valid values are \code{FALSE} or \code{TRUE}. In both cases the correspondence table as an R object. 
#' If needed to view the SPARQL query used, the argument should be set as \code{TRUE}. By default, NO SPARQL query is produced.
#' @import httr
#' @import jsonlite
#' @export
#' @return
#' \code{structureData()} returns the structure of a classification table from CELLAR and FAO in form a table with the following colums:        
#'  \itemize{
#'     \item Concept_Scheme: taxonomy of the SKOS object to be retrieved
#'     \item Level: the levels of the objects in the collection 
#'     \item Depth: identify the hierarchy of each level
#'     \item Count: the number of objects retrieved in each level
#' }
#' @examples
#' {
#'    ## Obtain a list including the structure of each classification available 
#'    ## CELLAR
#'    #data_CELLAR = list()
#'    #endpoint = "CELLAR"
#'    #list_data = classificationList("ALL")
#'   # 
   # #for (i in 1:nrow(list_data$CELLAR)){
   #   #  prefix = list_data$CELLAR[i,1]
   #  #   conceptScheme = list_data$CELLAR[i,2]
   # #   data_CELLAR[[i]] = dataStructure(endpoint, prefix, conceptScheme)
   ## }
   ## names(data_CELLAR) = list_data$CELLAR[,1]

#'}
  

dataStructure = function(endpoint, prefix, conceptScheme, language = "en", showQuery = FALSE) {
  #Check correctness of endpoint argument
  endpoint <- toupper(endpoint)
  if (!(endpoint %in% c("ALL", "FAO", "CELLAR"))) {
    stop(simpleError(paste("The endpoint value:", endpoint, "is not accepted")))
  }
  # Check the useLocalDataForVignettes option
  if (getOption("useLocalDataForVignettes", FALSE)) {
    
    localDataPath <- system.file("extdata", paste0("dataStructure_", prefix, ".csv"), package = "correspondenceTables")
    
    if (file.exists(localDataPath)) {
      # Read data from the local file if it exists
      data <- read.csv(localDataPath)
      if (showQuery) {
        print("Data loaded from local file.")
      }
      return(data)
    }
  } else {
  
  ### Load the configuration file from GitHub
  config <- fromJSON("https://raw.githubusercontent.com/eurostat/correspondenceTables/refs/heads/main/inst/extdata/endpoint_source_config.json")    
  ### Define endpoint
  if (endpoint == "CELLAR") {
    source = config$CELLAR
    url = "data.europa.eu/"
  }
  if (endpoint == "FAO") {
    source = config$FAO
    url = "unstats.un.org/"
  }
  
  ## Create Prefixes list 
  prefix_ls = prefixList(endpoint, prefix = prefix)
  prefix_ls = as.character(paste(prefix_ls, collapse = "\n"))
  ### Load prefixes from Excel file
  #prefix_file = read.csv(paste0("//lu-fsp01/Data_Lux/AgSTAT/Projects/CorrespondenceTables_Rpck/Task 3/prefix_", endpoint, ".csv"))
  #prefix = as.character(paste(prefix_file$List, collapse = "\n"))
  tryCatch(
    {
  ### SPARQL query
  SPARQL.query = paste0(prefix_ls, "
  SELECT  DISTINCT ?Concept_Scheme  ?Depth ?Level (COUNT (distinct ?s) AS ?Count) 
 

        WHERE {
          ?s skos:prefLabel ?Label ;
          skos:inScheme ", prefix, ":", conceptScheme, " ;
          skos:inScheme ?Scheme ;
          ^skos:member ?Member ;
          skos:prefLabel ?Label ;
          skos:notation ?notation .
          
          ?Member a xkos:ClassificationLevel .
          OPTIONAL {?member xkos:levels ?levels_temp . }
          OPTIONAL {?member xkos:depth ?Depth . }
        
          FILTER (?Scheme = ", prefix, ":", conceptScheme, ")
          FILTER (lang(?Label) = '", language, "')
          #FILTER (datatype(?notation) = rdf:PlainLiteral) 
        
          BIND (STR(?s) AS ?URL)
          #BIND (STR(?notation) AS ?CODE ) 
        
          BIND (STRAFTER(STR(", prefix, ":), 'http') AS ?CLASS_URL)
          #BIND (STRAFTER((?CLASS_URL), '/') AS ?Class)
          BIND (STRAFTER(STR(?Scheme), STR(?CLASS_URL)) AS ?Concept_Scheme)
          BIND (STRAFTER(str(?Member), STR(?CLASS_URL)) As ?Level)
         
          FILTER (STRLEN(?Concept_Scheme) != 0)
          FILTER (STRLEN(?Level) != 0)
  		    #FILTER (?Concept_Scheme != STR('ag'))
        }
        
      GROUP BY ?Concept_Scheme ?Depth ?Level
      ORDER BY ?Concept_Scheme ?Depth ?Level
      
  ")
          
  
  response = httr::POST(url = source, accept("text/csv"), body = list(query = SPARQL.query), encode = "form")
  table = read.csv(text=content(response, "text"), sep= ",")  
  table = table[order(table[,3],decreasing=FALSE),]
  
    }, error = function(e) {
      stop(simpleError(paste0("Error in function datastructure('", endpoint,"', '", prefix,"', '", conceptScheme,"'), endpoint is not available or is returning unexpected data\n")))
      cat("The following SPARQL code was used in the call:\n", SPARQL.query, "\n")
      cat("The following response was given for by the SPARQL call:\n", response)
    })

  if (nrow(table) == 0){
     message("This classification has no level. Please use level = 'ALL' when retrieving it using the retrieveClassificationTable")
  }
  
  if (showQuery) {
    result=list()
    result[[1]]= SPARQL.query
    result[[2]]= table
    names(result)=c("SPARQL.query", "dataStructure")
    cat(result$SPARQL.query, sep ="/n")
    return(result)
  } else {
    return(table)
  }
  
 }
}
