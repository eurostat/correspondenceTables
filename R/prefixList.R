#' @title Create a list of prefixes for both CELLAR and FAO repositories. 
#' @description  Create a list of prefixes to be used when defying the SPARQL query to retrieve the tables
#' @param endpoint SPARQL endpoints provide a standardized way to access data sets, 
#' making it easier to retrieve specific information or perform complex queries on linked data.  
#' The valid values are \code{"CELLAR"} and \code{"FAO"}.
#' @import httr
#' @export
#' @return
#' \code{prefixList()} returns a list of prefixes to be used when defying the SPARQL query.
#' @examples
#' {
#'     endpoint = "CELLAR"
#'     desired_prefix = "nace2"
#'     prefix_list = prefixList(endpoint,desired_prefix )
#'     }

  prefixList = function(endpoint, desired_prefix) {
    if (endpoint != "CELLAR" & endpoint != "FAO"){
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
          uri  = classificationEndpoint(endpoint)[[1]][,3]
          prefix = classificationEndpoint(endpoint)[[1]][,1]
          prefix = gsub("\\.", "", prefix)
          # Include the predefined prefixes
          prefix_all <- rbind(prefix_init)
          
          # Check if the desired prefix is available for the given endpoint
          if (desired_prefix %in% prefix) {
            # Find the URI corresponding to the desired prefix
            uri_for_prefix <- uri[prefix == desired_prefix]
            
            # Construct the PREFIX statement for the desired prefix
            prefix_selected <- matrix(paste0("PREFIX ", desired_prefix, ": <", uri_for_prefix, "/>"))
            prefix_all <- rbind(prefix_all, prefix_selected)
          } else {
            stop("Desired prefix not found.")
          }
          
    return(prefix_all)
  }
