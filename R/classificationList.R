#' @title Retrieve a list of classification tables from CELLAR and FAO repositories or both.
#' @description The purpose of this function is to provide a comprehensive summary 
#' of the data structure for each classification in CELLAR and FAO endpoint. 
#' The summary includes information such as the prefix name, URI, key, concept scheme, and title associated with each classification.
#' @param endpoint SPARQL endpoints provide a standardized way to access data sets, 
#' making it easier to retrieve specific information or perform complex queries on linked data. This is an optional
#' parameter, which by default is set to \code{"ALL"}.
#' The valid values are \code{"CELLAR"}, \code{"FAO"} and \code{"ALL"} for both endpoints.
#' @param showQuery The valid values are \code{FALSE} or \code{TRUE}. In both cases the correspondence table as an R object. 
#' If needed to view the SPARQL query used, the argument should be set as \code{TRUE}. By default, NO SPARQL query is produced.
#' @import httr
#' @import jsonlite
#' @export
#' @return
#' \code{classificationList()} returns a table with information needed to retrieve the classification table:
#' \itemize{
#'     \item Prefix name: the  SPARQL instruction for a declaration of a namespace prefix
#'     \item Conceptscheme: taxonomy of the SKOS object to be retrieved
#'     \item URI: the URL from which the SPARQL query was retrieved
#'     \item Name: the name of the table retrieved
#' }
#' @examples
#' {
#'     endpoint = "ALL"
#'     list_data = classificationList(endpoint)
#'     }

classificationList = function(endpoint = "ALL", showQuery = FALSE) {
  #Check correctness of endpoint argument
  endpoint <- toupper(endpoint)
  if (!(endpoint %in% c("ALL", "FAO", "CELLAR"))) {
    stop(simpleError(paste("The endpoint value:", endpoint, "is not accepted")))
  }
  # Check the useLocalDataForVignettes option
  if (getOption("useLocalDataForVignettes", FALSE)) {
    
    localDataPath <- system.file("extdata", paste0("classificationlList_", endpoint, ".csv"), package = "correspondenceTables")
    
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
      
  if (endpoint == "ALL" | endpoint == "CELLAR") {
  ### Datasets in CELLAR
  endpoint_cellar <- config$CELLAR

  SPARQL.query_cellar = paste0("
  SELECT DISTINCT ?s ?Title
  WHERE { ?s a skos:ConceptScheme ;
          skos:prefLabel ?Title ;
          ?p <http://publications.europa.eu/resource/authority/corporate-body/ESTAT> 
           FILTER (LANG(?Title) = 'en')}
  ORDER BY ?Title
  ")

  response = POST(url = endpoint_cellar, accept("text/csv"), body = list(query = SPARQL.query_cellar), encode = "form")
  data_cellar = read.csv(text=content(response, "text"), sep= ",")  

  ## add prefix name
  str_dt = t(sapply(data_cellar[,1], function(x) unlist(strsplit(as.character(x), "/+"))))
  uri = paste0(str_dt[,1],"/", "/", str_dt[,2],"/",str_dt[,3],"/",str_dt[,4] )
  prefix = str_dt[,4]
  prefix = gsub("\\.","",prefix)
  #key = str_dt[,4]
  conceptscheme = str_dt[,5]
  title = data_cellar[,2]
  data_cellar = cbind(prefix, conceptscheme, uri, title)
  rownames(data_cellar) = 1:nrow(data_cellar)
  colnames(data_cellar) = c("Prefix", "ConceptScheme", "URI", "Title")
  }
    }, error = function(e) {
      cat("The following SPARQL code was used in the call:\n", SPARQL.query_cellar, "\n")
      cat("The following response was given for by the SPARQL call:\n", response)
      stop(simpleError(paste("Error in function ClassificationList(", endpoint,"), Endpoint Cellar is not available or is returning unexpected data")))
    })
  
  tryCatch(
    {
  if (endpoint == "ALL" | endpoint == "FAO") {
  ### Datasets in FAO
  endpoint_fao <- config$FAO
  SPARQL.query_fao = paste0("
     PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX skos: <http://www.w3.org/2004/02/skos/core#>

SELECT  ?scheme ?notation ?label_en WHERE {
   ?scheme rdf:type skos:ConceptScheme .
   ?scheme skos:notation ?notation .
  ?scheme skos:prefLabel ?label_en . FILTER(lang(?label_en)='en') .
  }
ORDER BY ASC(?notation)
  ")
  
  response = httr::POST(url = endpoint_fao, accept("text/csv"), body = list(query = SPARQL.query_fao), encode = "form")
  data_fao = read.csv(text=content(response, "text"), sep= ",")                                 
  
  ## add prefix name
  # str_dt = t(sapply(data_fao[,1], function(x) unlist(strsplit(as.character(x), "/+"))))
  str_dt<- strsplit(data_fao[, 1], "/")
  mat_str_dt <- suppressWarnings(do.call(rbind, str_dt))
  df_str_dt <- as.data.frame(mat_str_dt)
  prefix = data_fao[,2]
  # prefix = paste0(df_str_dt[,5], df_str_dt[,6])
  # prefix = gsub("\\.","",prefix)
  #uri = paste0(df_str_dt[,1],  "/", df_str_dt[,2], "/", df_str_dt[,3], "/", df_str_dt[,4], "/", prefix)
  uri = read.csv(system.file("extdata", "classificationlList_FAO.csv", package = "correspondenceTables"))[,3]
  #class = prefix
  ConceptScheme = paste0(df_str_dt[,5], df_str_dt[,6])
  data_fao = cbind(prefix, ConceptScheme, uri, data_fao[,3])
  rownames(data_fao) = 1:nrow(data_fao)
  colnames(data_fao) = c("Prefix", "ConceptScheme", "URI", "Title")
  }
    }, error = function(e) {
      cat("The following SPARQL code was used in the call:\n", SPARQL.query_fao, "\n")
      cat("The following response was given for by the SPARQL call:\n", response)
      stop(simpleError(paste("Error in function ClassificationList(",endpoint,"),Endpoint Fao is not available or is returning unexpected data")))
    })
  if (endpoint == "ALL") {
    data = list("CELLAR" = data_cellar, "FAO" = data_fao)
    SPARQL.query <- paste0(SPARQL.query_cellar, "\n", SPARQL.query_fao )
  }
  
  if (endpoint == "CELLAR") {
    data = list("CELLAR" = data_cellar)
    SPARQL.query <- SPARQL.query_cellar
  }
  
  if (endpoint == "FAO") {
    data = list("FAO" = data_fao)
    SPARQL.query <- SPARQL.query_fao
  }  
  
  if (showQuery) {
    result=list()
    result[[1]]= SPARQL.query
    result[[2]]= data
    names(result)=c("SPARQL.query" ,"ClassificationList")
    cat(result$SPARQL.query, sep ="/n")
    return(result)
  } else {
    return(data)
  }
  
 }
}

