#' @title Create a list of prefixes for both CELLAR and FAO
#' @description  Create a list of prefixes to be used when defying the SPARQL query to retrieve the tables
#' @param endpoint. A string of type character containing the endpoint where the table is stored. 
#' The valid values are \code{"CELLAR"} and \code{"FAO"}.
#' @export
#' @return
#' \code{prefixList()} returns a list of prefixes to be used when defying the SPARQL query.
#' @examples
#' {
#'     endpoint = "CELLAR"
#'     prefix_lst = prefixList(endpoint)
#'     }

prefixList = function(endpoint) {
  if (endpoint == "ALL"){
      stop("Specify the endpoint: CELLAR or FAO.")
    }
       prefix_init = as.matrix(rbind( 
      "PREFIX dc: <http://purl.org/dc/elements/1.1/>",
      "PREFIX dct: <http://purl.org/dc/terms/>",
      "PREFIX cb: <http://cbasewrap.ontologycentral.com/vocab#>",
      "PREFIX eli: <http://data.europa.eu/eli/ontology#>",
      "PREFIX euvoc: <http://publications.europa.eu/ontology/euvoc#>",
      "PREFIX owl: <http://www.w3.org/2002/07/owl#>", 
      "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>",
      "PREFIX skosxl: <http://www.w3.org/2008/05/skos-xl#>",
      "PREFIX xml: <http://www.w3.org/XML/1998/namespace>",
      "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>",
      "PREFIX skos: <http://www.w3.org/2004/02/skos/core#>",
      "PREFIX xkos: <http://rdf-vocabulary.ddialliance.org/xkos#>",
      "PREFIX is: <http://purl.org/ontology/is/core#>",
      "PREFIX isi: <http://purl.org/ontology/is/inst/>",
      "PREFIX cpc: <https://data.epo.org/linked-data/def/cpc/>", 
      "PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>"))

    
  ### Define List
        uri  = classEndpoint(endpoint)[[1]][,3]
        prefix = classEndpoint(endpoint)[[1]][,1]
        prefix = gsub("\\.", "", prefix)
        prefix_all = as.matrix(paste0("PREFIX ", prefix, ": <", uri, "/>"))
        prefix_all = rbind(prefix_init, prefix_all)
    
  return(prefix_all)
}