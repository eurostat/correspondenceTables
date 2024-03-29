#' @title Retrieve a classification tables from CELLAR and FAO
#' @description Retrieve a classification tables from CELLAR and FAO
#' @param prefix The  SPARQL instruction for a declaration of a namespace prefix. It can be found using the classEndpoint() function. 
#' @param endpoint The SPARQL Endpoint, the valid values are \code{"CELLAR"} or \code{"FAO"}.
#' @param conceptScheme Taxonomy of the SKOS object to be retrieved. It can be found using the classEndpoint() function. 
#' @param level The levels of the objects in the collection to be retrieved, it can be found using the structureData() function. 
#' By default is set to \code{"ALL"}. This is an optional argument.  
#' @param language Language of the table. By default is set to \code{"en"}. This is an optional argument.
#' @param CSVout The valid values are \code{FALSE} or \code{TRUE}. In both cases the correspondence table as an R object. 
#' If output should be saved as a csv file, the argument should be set as \code{TRUE}. By default, no csv file is produced. 
#' @export
#' @return
#' \code{retrieveClassificationTable()} returns a classification tables from CELLAR and FAO. The table includes the following variables: 
#'  \itemize{
#'     \item Classification name (e.g. nace2): the code of each object
#'     \item NAME: the corresponding name of each object 
#'     \item Include: details on each object
#'     \item Include_Also: details on each object
#'     \item Exclude: details on each object
#'     \item URL: the URL from which the SPARQL query was retrieved
#' }
#' @examples
#' {
#'     endpoint = "CELLAR"
#'     prefix = "nace2"
#'     endpoint = "CELLAR"
#'     conceptScheme = "nace2"
#'     dt = retrieveClassificationTable(prefix, endpoint, conceptScheme)
#'     # By default retrieved all levels and only English
#'     head(dt)
#'     }



retrieveClassificationTable = function(prefix, endpoint, conceptScheme, level = "ALL", language = "en", CSVout = FALSE) {

  ### Define endpoint
    if (endpoint == "CELLAR") {
      source = "http://publications.europa.eu/webapi/rdf/sparql"
    }
    if (endpoint == "FAO") {
      source = "https://stats.fao.org/caliper/sparql/AllVocs"
    }
  
  ### Load prefixes using prefixList function
  prefixlist = prefixList(endpoint)
  prefixlist = as.character(paste(prefixlist, collapse = "\n"))
  
  ## Define key to get code as the last letter of the prefix name
  #str = unlist(strsplit(prefix, ""))
  #key = paste0(str[(length(str))-1],str[length(str)])
  
  ### CLASSIFICATION TABLE SPARQL QUERIES
  ### Define SPARQL query -- BASE: all levels
  SPARQL.query_0 = paste0(prefixlist, "

        SELECT DISTINCT ?", prefix, " ?NAME ?Include ?Include_Also ?Exclude ?URL ?datatype

        WHERE {
            ?s skos:prefLabel ?Label ;
                skos:inScheme ?Scheme ;
                ^skos:member ?Member ;
                skos:prefLabel ?Label ;
                skos:notation ?notation .
            
                #FILTER (datatype(?notation) = xsd:string)
                
                FILTER (?Scheme = ", prefix, ":", conceptScheme, ")
                FILTER (lang(?Label) = '", language, "')
                
                BIND (STR(?s) AS ?URL)
                BIND (STR(?notation) as ?", prefix, " )
                BIND (STR(?Label) as ?NAME)
                BIND (datatype(?notation) AS ?datatype)
                
                OPTIONAL {?s skos:scopeNote ?Include . FILTER (LANG(?Include) = '", language, "') .}
                OPTIONAL {?s xkos:exclusionNote ?Exclude . FILTER (LANG(?Exclude) = '", language, "').}
                OPTIONAL {?s xkos:additionalContentNote ?Include_Also . FILTER (LANG(?Include_Also) = '", language, "').}
              
              ")


  ### Define SPARQL query -- FILTER LEVEL
  SPARQL.query_level = paste0("FILTER (?Member = ", prefix, ":", level, ")")
  
  ### End SPARQL query ", prefix 
  SPARQL.query_end = paste0("}
          ORDER BY ?", prefix
          )
  
  if (length(level) == 0 ){
    stop("Classification has no levels. Set level = ALL to retrieve the table.") 
  } else {  
      if (level == "ALL") {
        SPARQL.query = paste0(SPARQL.query_0, SPARQL.query_end)
      } else {
        SPARQL.query = paste0(SPARQL.query_0, SPARQL.query_level, SPARQL.query_end)
      }
  }

  response = httr::POST(url = source, config = httr::accept("text/csv"), body = list(query = SPARQL.query), encode = "form")
  data = read.csv(text=httr::content(response, "text"), sep= ",") # data.frame(httr::content(response))
  
  #keep only plainLiteral if more than one datatype // 
  #FAO - "http://www.w3.org/2001/XMLSchema#string"
  #CELLAR - "http://www.w3.org/2001/XMLSchema#string" - "http://www.w3.org/1999/02/22-rdf-syntax-ns#PlainLiteral"
  type = unique(data$datatype)
  if (length(type) > 1){
    data = data[which(data$datatype == "http://www.w3.org/1999/02/22-rdf-syntax-ns#PlainLiteral"), ]
  }

  #remove datatype col
  data = data[, 1:(ncol(data)-1)]
  
  #remove duplicates when only code name is different 
  #data = data[which(duplicated(data[,-1]) == FALSE), ]
  
  #are there other duplicates? URL is the same and the other changes
  xcol = which(colnames(data) == "URL")
  dup = length(which(duplicated(data[,-xcol]) == TRUE))
  
  if (dup > 0) {
    warning("There are duplicates codes in the classification table.") 
  }
  
  # Save results as CSV and show where it was stored
  if (CSVout == TRUE) {
    name_csv = paste0(prefix, "_table.csv")
    write.csv(data, file= name_csv, row.names=FALSE)
    message(paste0("The table was saved in ", getwd(), name_csv))
  } 

  return(data)

}