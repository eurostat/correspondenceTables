#' @title Retrieve a list of classification tables from CELLAR and FAO repositories or both.
#' @description The purpose of this function is to provide a comprehensive summary 
#' of the data structure for each classification in CELLAR and FAO endpoint. 
#' The summary includes information such as the prefix name, URI, key, concept scheme, and title associated with each classification.
#' @param endpoint SPARQL endpoints provide a standardized way to access data sets, 
#' making it easier to retrieve specific information or perform complex queries on linked data. This is an optional
#' parameter, which by default is set to \code{"ALL"}.
#' The valid values are \code{"CELLAR"}, \code{"FAO"} and \code{"ALL"} for both endpoints. 
#' @import httr
#' @export
#' @return
#' \code{classificationEndpoint()} returns a table with information needed to retrieve the classification table:
#' \itemize{
#'     \item Prefix name: the  SPARQL instruction for a declaration of a namespace prefix
#'     \item Conceptscheme: taxonomy of the SKOS object to be retrieved
#'     \item URI: the URL from which the SPARQL query was retrieved
#'     \item Name: the name of the table retrieved
#' }
#' @examples
#' {
#'     endpoint = "ALL"
#'     list_data = classificationEndpoint(endpoint)
#'     }

classificationEndpoint = function(endpoint = "ALL") {
  endpoint <- toupper(endpoint)
  tryCatch(
    {
  if (endpoint == "ALL" | endpoint == "CELLAR") {
  ### Datasets in CELLAR
  endpoint_cellar = "http://publications.europa.eu/webapi/rdf/sparql"
  
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
      print(SPARQL.query_cellar)
      stop(simpleError(paste("Error in function ClassificationEndpoint(",endpoint,"),Endpoint Cellar is not available or is returning unexpected data")))
    })
  
  tryCatch(
    {
  if (endpoint == "ALL" | endpoint == "FAO") {
  ### Datasets in FAO
  endpoint_fao = "https://stats.fao.org/caliper/sparql/AllVocs"
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
  uri = paste0(df_str_dt[,1],  "/", df_str_dt[,2], "/", df_str_dt[,3], "/", df_str_dt[,4], "/", prefix)
  #class = prefix
  ConceptScheme = paste0(df_str_dt[,5], df_str_dt[,6])
  data_fao = cbind(prefix, ConceptScheme, uri, data_fao[,3])
  rownames(data_fao) = 1:nrow(data_fao)
  colnames(data_fao) = c("Prefix", "ConceptScheme", "URI", "Title")
  }
    }, error = function(e) {
      print(SPARQL.query_fao)
      stop(simpleError(paste("Error in function ClassificationEndpoint(",endpoint,"),Endpoint Fao is not available or is returning unexpected data")))
    })
  if (endpoint == "ALL") {
    data = list("CELLAR" = data_cellar, "FAO" = data_fao)
  }
  
  if (endpoint == "CELLAR") {
    data = list("CELLAR" = data_cellar)
  }
  
  if (endpoint == "FAO") {
    data = list("FAO" = data_fao)
  }  
  
  return(data)

}

