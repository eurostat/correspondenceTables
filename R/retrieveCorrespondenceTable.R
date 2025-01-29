#' @title Retrieve correspondence tables between statistical classifications from CELLAR and FAO repositories.
#' @description To facilitate the utilization of correspondence tables as inputs for the newCorrespondenceTable and updateCorrespondenceTable functions, 
#' "retrieveCorrespondenceTable" utility function has been developed. This utility function leverage R packages that enable SPARQL queries.
#' @param endpoint SPARQL endpoints provide a standardized way to access data sets, 
#' making it easier to retrieve specific information or perform complex queries on linked data.
#' The valid values are \code{"CELLAR"} or \code{"FAO"}. 
#' @param prefix Prefixes are typically defined at the beginning of a SPARQL query 
#' and are used throughout the query to make it more concise and easier to read. 
#' Multiple prefixes can be defined in a single query to cover different namespaces used in the dataset.
#' The function 'prefixList()' can be used to generate the prefixes for the selected correspondence table.
#' @param ID_table Refers to a unique identifier associated with a specific correspondence table. 
#' The ID_table can be obtained by utilizing the "correspondenceTableList()" function.
#' @param language Refers to the specific language used for providing label, include and exclude information in the selected correspondence table. 
#' By default is set to "en". This is an optional argument.
#' @param CSVout The valid value is a valid path to a csv file including file name and extension. By default, no csv file is produced, \code{NULL} 
#' @param showQuery The valid values are \code{FALSE} or \code{TRUE}. In both cases the correspondence table as an R object. 
#' If not needed to view the SPARQL query used, the argument should be set as \code{FALSE}. By default, the SPARQL query is produced.
#' @param localData this parameter allow the user to retrieve static data from the package in order to avoid any issues from the api
#' @import httr
#' @export
#' @return
#' \code{retrieveCorrespondenceTable()} returns a classification tables from CELLAR and FAO. The table includes the following variables:
#'  \itemize{
#'     \item Source Classification name (e.g. cn2019): the code of each object in the source classification
#'     \item Source Classification name (e.g. cn2021): the code of each object in the target classification
#'     \item Target Classification label: the corresponding label of each object (e.g. cn2019)
#'     \item Include: include details on each object (e.g. cn2019)
#'     \item Exclude: details on each object (e.g. cn2019)
#'     \item Target Classification label: the corresponding label of each object (e.g. cn2021)
#'     \item Include: include details on each object (e.g. cn2021)
#'     \item Exclude: details on each object (e.g. cn2021)
#'     \item Comment: details on each object, if available 
#'     \item URL: the URL from which the SPARQL query was retrieved
#' }
#' @examples 
#' {
#'     endpoint = "CELLAR"
#'     prefix = "cn2022"
#'     ID_table = "CN2022_NST2007"
#'     
#'     results_ls = retrieveCorrespondenceTable( endpoint, prefix, ID_table)
#'     
#'     # View SPARQL Query
#'     cat(results_ls[[1]])
#'     
#'     #View Classification Table
#'     #View(results_ls[[2]])
#'     }
 

retrieveCorrespondenceTable = function(endpoint, prefix, ID_table, language = "en", CSVout = NULL, showQuery = TRUE) {
  #Check correctness of endpoint argument
  endpoint <- toupper(endpoint)
  if (!(endpoint %in% c("ALL", "FAO", "CELLAR"))) {
    stop(simpleError(paste("The endpoint value:", endpoint, "is not accepted")))
  }
  # Check the useLocalDataForVignettes option
  if (getOption("useLocalDataForVignettes", FALSE)) {
    localDataPath <- system.file("extdata", paste0(ID_table, "_", language, ".csv"), package = "correspondenceTables")
    
    if (file.exists(localDataPath)) {
      # Read data from the local file if it exists
      data <- read.csv(localDataPath)
      if (showQuery) {
        print("Data loaded from local file.")
      }
      return(data)
    }
  } else {
    tryCatch(
      {
    ### Load the configuration file from GitHub
    config <- fromJSON("https://raw.githubusercontent.com/eurostat/correspondenceTables/refs/heads/main/inst/extdata/endpoint_source_config.json")
    ### Define endpoint
    if (endpoint == "CELLAR") {
      source <- config$CELLAR
    }
    if (endpoint == "FAO") {
      source <- config$FAO
    }
    
    ## Define A and B
    ID_table_temp = gsub("-", "_", ID_table)
    ID_table_temp = gsub("__", "_", ID_table_temp)
    A = sub("_.*", "", ID_table_temp)
    B = sub(".*_", "", ID_table_temp)
    
    ### Load prefixes using prefixList function
    prefixlist = prefixList(endpoint, prefix = tolower(c(A,B)))
    prefixlist = as.character(paste(prefixlist, collapse = "\n"))
    
      }, error = function(e) {
        stop(simpleError(paste("Error in function retrieveCorrespondenceTable, building of the SPARQL query failed.",endpoint,"is not available or is returning unexpected data.")))
      })
    
    
    ### CLASSIFICATION TABLE SPARQL QUERIES
    ### Define SPARQL query -- BASE
    tryCatch(
      {
    SPARQL.query_0 = paste0(prefixlist, "
        SELECT ?", A ," ?", B ," ?Label_", A ," ?Label_", B ," ?Include_", A ," ?Exclude_", A ," ?Include_", B ," ?Exclude_", B ," ?Comment ?URL  ?Sourcedatatype ?Targetdatatype 
        
        WHERE {
         ", prefix, ":", ID_table, " xkos:madeOf ?Associations .
         ?Associations xkos:sourceConcept ?Source .
         OPTIONAL  {?Associations xkos:targetConcept ?Target .}
         OPTIONAL  {?Associations rdfs:comment ?Comment . }  
    
         ?Source   skos:notation ?SourceNotation .
         ?Target   skos:notation ?TargetNotation .
    
         #FILTER ( datatype(?SourceNotation) = rdf:PlainLiteral)
         #FILTER ( datatype(?TargetNotation) = rdf:PlainLiteral)
         
         BIND (STR(?Associations ) AS ?URL)
         BIND (STR(?SourceNotation) as ?", A ,") 
         BIND (STR(?TargetNotation) as ?", B ,")
         BIND (datatype(?SourceNotation) AS ?Sourcedatatype)
         BIND (datatype(?TargetNotation) AS ?Targetdatatype)
    
         OPTIONAL { ?Source skos:altLabel ?Label_", A ,"  FILTER (LANG(?Label_", A ,") = '", language, "') .}
         OPTIONAL { ?Target skos:altLabel ?Label_", B ,"  FILTER (LANG(?Label_", B ,") = '", language, "') .}
         OPTIONAL {?Source skos:scopeNote ?Include_", A ,".     FILTER (LANG(?Include_", A ,") = '", language, "') .}
         OPTIONAL {?Source xkos:exclusionNote ?Exclude_", A ,".    FILTER (LANG(?Exclude_", A ,") = '", language, "') .}
         OPTIONAL {?Target skos:scopeNote ?Include_", B ,".     FILTER (LANG(?Include_", B ,") = '", language, "') .}
         OPTIONAL {?Target xkos:exclusionNote ?Exclude_", B ,".    FILTER (LANG(?Exclude_", B ,") = '", language, "') .}
    
       ")
    
    ### End SPARQL query ", prefix 
    SPARQL.query_end = paste0("}
              ORDER BY ?Source
             ")
    
    SPARQL.query = paste0(SPARQL.query_0, SPARQL.query_end)
    
    response = httr::POST(url = source, accept("text/csv"), body = list(query = SPARQL.query), encode = "form")
    data = data.frame(content(response, show_col_types = FALSE))
      }, error = function(e) {
        cat("The following SPARQL code was used in the call:\n", SPARQL.query, "\n")
        cat("The following response was given for by the SPARQL call:\n", response)
        stop(simpleError("Error in function retrieveCorrespondenceTable, SPARQL query execution failed ."))
      })
    
    
    #keep only plainLiteral if more than one datatype // 
    #FAO - "http://www.w3.org/2001/XMLSchema#string"
    #CELLAR - "http://www.w3.org/2001/XMLSchema#string" - "http://www.w3.org/1999/02/22-rdf-syntax-ns#PlainLiteral"
    Source_type = unique(data$Sourcedatatype)
    Target_type = unique(data$Targetdatatype)
    if (length(Source_type) > 1 | length(Target_type) > 1){
      data = data[which(data$Sourcedatatype == "http://www.w3.org/1999/02/22-rdf-syntax-ns#PlainLiteral"), ]
      data = data[which(data$Targetdatatype == "http://www.w3.org/1999/02/22-rdf-syntax-ns#PlainLiteral"), ]
    }
    
    #remove datatype col
    data = data[, 1:(ncol(data)-2)]
    
    data <- lapply(data, function(x) gsub("\n", " ", x))
    data <- as.data.frame(data)
    
    # Save results as CSV and show where it was stored
    # if (CSVout == TRUE) {
    #   name_csv = paste0(ID_table,"_",language,"_table.csv")
    #   write.csv(data, file= name_csv, row.names=FALSE)
    #   message(paste0("The correspondence table was saved in ", getwd(), name_csv))
    # } else if (is.character(CSVout)) {
    #   # if user provide a csv file data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABIAAAASCAYAAABWzo5XAAAAWElEQVR42mNgGPTAxsZmJsVqQApgmGw1yApwKcQiT7phRBuCzzCSDSHGMKINIeDNmWQlA2IigKJwIssQkHdINgxfmBBtGDEBS3KCxBc7pMQgMYE5c/AXPwAwSX4lV3pTWwAAAABJRU5ErkJggg==
    #   write.csv(data, file = CSVout, row.names = FALSE)
    #   message(paste0("The table was saved in ", getwd(), CSVout))
    # }
      
    
    
    CsvFileSave(CSVout, data )
    
    
  
  if (showQuery) {
    result=list()
    result[[1]]=SPARQL.query
    result[[2]]=data
    names(result)=c("SPARQL.query", "CorrespondenceTable")
    cat(result$SPARQL.query, sep ="/n")
  }
  
  if (showQuery == FALSE){
    result=data
  }
  
  return(result)
 }
}
