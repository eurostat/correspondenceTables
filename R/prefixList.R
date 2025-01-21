#' @title Create a list of prefixes for both CELLAR and FAO repositories. 
#' @description  Create a list of prefixes to be used when defying the SPARQL query to retrieve the tables
#' @param endpoint SPARQL endpoints provide a standardized way to access data sets, 
#' making it easier to retrieve specific information or perform complex queries on linked data.  
#' The valid values are \code{"CELLAR"} and \code{"FAO"}.
#' @import httr
#' @return
#' \code{prefixList()} returns a list of prefixes to be used when defying the SPARQL query.
#' @examples
#' {
#'     endpoint = "CELLAR"
#'     prefix_list = prefixList(endpoint)
#'     }

prefixList = function(endpoint, prefix = NULL) {
  #Check correctness of endpoint argument
  endpoint <- toupper(endpoint)
  if (endpoint != "CELLAR" & endpoint != "FAO") {
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
    "PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>"
  ))
  
  ### Define List
  uri = classificationList(endpoint)[[1]][, 3]
  prefix_endpoint = classificationList(endpoint)[[1]][, 1]
  prefix_endpoint = gsub("\\.", "", prefix_endpoint)
  # Include the predefined prefixes
  prefix_all = as.matrix(paste0("PREFIX ", prefix_endpoint, ": <", uri, "/>"))
  prefix_all = rbind(prefix_init, prefix_all)
  # remove duplicates
  prefix_all = prefix_all[!duplicated(prefix_all)]
  
  # Check if desired prefixes are available for the given endpoint
  if (!is.null(prefix)) {
    # Check if the desired prefixes are available for the given endpoint
    valid_prefixes = prefix[prefix %in% prefix_endpoint]
    if (length(valid_prefixes) > 0) {
      # Find the URIs corresponding to the desired prefixes
      uri_for_prefix <- uri[prefix_endpoint %in% valid_prefixes]
      
      # Construct the PREFIX statements for the desired prefixes
      prefix_selected <- matrix(paste0("PREFIX ", valid_prefixes, ": <", uri_for_prefix, "/>"))
      prefix_all <- rbind(prefix_init, prefix_selected)
    } else {
      stop("Desired prefixes not found.")
    }
  }
  
  return(prefix_all)
}
