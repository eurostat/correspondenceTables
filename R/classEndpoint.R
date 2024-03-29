#' @title Retrieve a list of classification tables in CELLAR, FAO or both.
#' @description Retrieve a list of classification tables in CELLAR, FAO or both.
#' @param endpoint A string of type character containing the endpoint where the table is stored. 
#' The valid values are \code{"CELLAR"}, \code{"FAO"} and \code{"ALL"} for both endpoints. 
#' @export
#' @return
#' \code{classEndpoint()} returns a table with information needed to retrieve the classification table:
#' \itemize{
#'     \item Prefix name: the  SPARQL instruction for a declaration of a namespace prefix
#'     \item Conceptscheme: taxonomy of the SKOS object to be retrieved
#'     \item URI: the URL from which the SPARQL query was retrieved
#'     \item Name: the name of the table retrieved
#' }
#' @examples
#' {
#'     endpoint = "ALL"
#'     list_data = classEndpoint(endpoint)
#'     }

classEndpoint = function(endpoint) {

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

  response = httr::POST(url = endpoint_cellar, config = httr::accept("text/csv"), body = list(query = SPARQL.query_cellar), encode = "form")
  data_cellar = read.csv(text=httr::content(response, "text"), sep= ",")  

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
  
  ### Datasets in FAO
  endpoint_fao = "https://stats.fao.org/caliper/sparql/AllVocs"
  SPARQL.query_fao = paste0("
      PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
      SELECT DISTINCT ?classification ?label

      WHERE {
          ?classification a skos:ConceptScheme .
          ?classification skos:prefLabel ?label .
          FILTER(regex(?label, 'classification', 'i'))
      }
    ORDER BY ?label
  ")
  
  response = httr::POST(url = endpoint_fao, config = httr::accept("text/csv"), body = list(query = SPARQL.query_fao), encode = "form")
  data_fao = read.csv(text=httr::content(response, "text"), sep= ",")                                 
  
  ## add prefix name
  str_dt = t(sapply(data_fao[,1], function(x) unlist(strsplit(as.character(x), "/+"))))
  prefix = paste0(str_dt[,4], str_dt[,5])
  prefix = gsub("\\.","",prefix)
  uri = paste0(str_dt[,1], "/", "/", str_dt[,2], "/", str_dt[,3], "/", str_dt[,4], "/", str_dt[,5])
  #class = prefix
  ConceptScheme = str_dt[,6]
  data_fao = cbind(prefix, ConceptScheme, uri, data_fao[,2])
  rownames(data_fao) = 1:nrow(data_fao)
  colnames(data_fao) = c("Prefix", "ConceptScheme", "URI", "Title")

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

