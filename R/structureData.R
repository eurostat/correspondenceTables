#' @title Obtain the structure of the classification tables from CELLAR and FAO.
#' @description Obtain the structure of the classification tables from CELLAR and FAO.
#' @param prefix. the  SPARQL instruction for a declaration of a namespace prefix. It can be found using the classEndpoint() function.
#' @param conceptScheme. taxonomy of the SKOS object to be retrieved. It can be found using the classEndpoint() function.
#' @param endpoint. the SPARQL Endpoint  
#' @param language. language of the table. By default is set to \code{"en"}. This is an optional argument.  
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
#'     endpoint = "CELLAR"
#'     prefix = "nace2"
#'     conceptScheme = "nace2"
#'     language = "en"
#'     structure_dt = structureData(prefix, conceptScheme, endpoint, language)
#'     }

structureData = function(prefix, conceptScheme, endpoint, language = "en") {
  
  ### Define endpoint
  if (endpoint == "CELLAR") {
    source = "http://publications.europa.eu/webapi/rdf/sparql"
    url = "data.europa.eu/"
  }
  if (endpoint == "FAO") {
    source = "https://stats.fao.org/caliper/sparql/AllVocs"
    url = "unstats.un.org/"
  }
  
  ## Create Prefixes list 
  prefix_ls = prefixList(endpoint)
  prefix_ls = as.character(paste(prefix_ls, collapse = "\n"))
  ### Load prefixes from Excel file
  #prefix_file = read.csv(paste0("//lu-fsp01/Data_Lux/AgSTAT/Projects/CorrespondenceTables_Rpck/Task 3/prefix_", endpoint, ".csv"))
  #prefix = as.character(paste(prefix_file$List, collapse = "\n"))
  
  ### SPARQL query
  SPARQL.query = paste0(prefix_ls, "
  SELECT  DISTINCT ?Concept_Scheme ?Level ?Depth (COUNT (distinct ?s) AS ?Count) 

        WHERE {
          ?s skos:prefLabel ?Label ;
          #skos:inScheme ", prefix, ":", conceptScheme, " ;
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
        
      GROUP BY ?Concept_Scheme ?Level  ?Depth 
      ORDER BY ?Concept_Scheme ?Level  ?Depth
  ")
          
  
  response = httr::POST(url = source, accept("text/csv"), body = list(query = SPARQL.query), encode = "form")
  table = read.csv(text=content(response, "text"), sep= ",")  
  table = table[order(table[,3],decreasing=FALSE),]

  return(table)
}