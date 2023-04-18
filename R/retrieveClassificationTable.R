#' @title Retrieve a classification tables from CELLAR/FAO or both.
#' @description Retrieve a classification tables from CELLAR/FAO or both.
#' @param prefix 
#' @param endpoint 
#' @param conceptScheme 
#' @param level. By default set to "ALL". Optional
#' @param language By default set to "en". Optional 
#' @export
#' @return
#' \code{retrieveClassificationTable()} returns a classification tables from CELLAR/FAO or both.

retrieveClassificationTable = function(prefix, endpoint, conceptScheme, level = "ALL", language = "en") {
  
  ## Define source from class --- classification

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

  response = httr::POST(url = source, accept("text/csv"), body = list(query = SPARQL.query), encode = "form")
  data = data.frame(content(response))
  
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
  
  #result = list(data, status)
  return(data)
}