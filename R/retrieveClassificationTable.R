#' @title Retrieve classification tables from CELLAR and FAO repositories.
#' @description To facilitate the utilization of European classifications as inputs for the newCorrespondenceTable and updateCorrespondenceTable functions, 
#' "retrieveClassificationTable()" utility function has been developed. This utility function leverage R packages that enable SPARQL queries.
#' @param prefix Prefixes are typically defined at the beginning of a SPARQL query and are used throughout the query to make it more concise and easier to read. 
#' Multiple prefixes can be defined in a single query to cover different namespaces used in the data set.
#' The function 'classificationEndpoint()' can be used to generate the prefixes for the selected classification table. 
#' @param endpoint SPARQL endpoints provide a standardized way to access data sets, 
#' making it easier to retrieve specific information or perform complex queries on linked data.
#' The valid values are \code{"CELLAR"} or \code{"FAO"}. 
#' @param conceptScheme Refers to a unique identifier associated to specific classification table. 
#' The conceptScheme can be obtained by utilizing the "classificationEndpoint()" function.
#' @param level Refers  to the hierarchical levels of the selected classification table. 
#' The detailed level information can be obtained by utilizing the "structureData() " function. 
#' By default is set to \code{"ALL"}. This is an optional argument.  
#' @param language Refers to the specific language used for providing label, include and exclude information in the selected classification table. 
#' By default is set to "en". This is an optional argument.
#' @param CSVout The valid values are \code{FALSE} or \code{TRUE}. In both cases the classification table as an R object. 
#' If output should be saved as a csv file, the argument should be set as \code{TRUE}. By default, no csv file is produced. 
#' @param showQuery The valid values are \code{FALSE} or \code{TRUE}. In both cases the classification table as an R object. 
#' If needed to view the SPARQL query used, the argument should be set as \code{TRUE}. By default, no SPARQL query is produced.
#' @import httr
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
#'     conceptScheme = "nace2"
#'     
#'     results_ls = retrieveClassificationTable(prefix, endpoint, conceptScheme)
#'     
#'     # View SPARQL Query
#'     cat(results_ls[[1]])
#'     
#'     #View Classification Table
#'     #View(results_ls[[2]])
#'     }



retrieveClassificationTable = function(prefix, endpoint, conceptScheme, level = "ALL",  language = "en", CSVout = FALSE, showQuery=TRUE) {
  
  ### Define endpoint
  if (endpoint == "CELLAR") {
    source = "http://publications.europa.eu/webapi/rdf/sparql"
  }
  if (endpoint == "FAO") {
    source = "https://stats.fao.org/caliper/sparql/AllVocs"
  }
  
  ### Load prefixes using prefixList function
  prefixlist = prefixList(endpoint, desired_prefix = prefix)
  prefixlist = as.character(paste(prefixlist, collapse = "\n"))
  
  
  # # Check if classification has level, if not, set level = "ALL"
  dt_level = suppressMessages(dataStructure(prefix, conceptScheme, endpoint, language))
   
   if (nrow(dt_level) == 0 & level != "ALL") {
     level = "ALL"
     message("Classification has no levels, so level = ALL was set to retrieve the table.")
   }
   
  ### Define SPARQL query -- BASE: all levels
  SPARQL.query_0 = paste0(prefixlist, "
        SELECT DISTINCT ?", prefix, " ?NAME ?Parent ?Include ?Include_Also ?Exclude ?URL

        WHERE {
            ?s skos:altLabel ?Label ;
                skos:inScheme ?Scheme ;
                ^skos:member ?Member ;
                #skos:broader ?Broader;.
                # skos:altLabel ?Label ;
                skos:notation ?notation .
                  OPTIONAL {?s skos:broader ?Broader. 
            ?Broader skos:notation ?BT_Notation.}
                #FILTER (datatype(?notation) = xsd:string)
                 BIND (STR(?BT_Notation) as ?Parent)
                FILTER (?Scheme = ", prefix, ":", conceptScheme, ")
                FILTER (lang(?Label) = '", language, "')
                
                BIND (STR(?s) AS ?URL)
                BIND (STR(?notation) as ?", prefix, " )
                BIND (STR(?Label) as ?NAME)
               # BIND (datatype(?notation) AS ?datatype)
                
                OPTIONAL {?s skos:scopeNote ?Include . FILTER (LANG(?Include) = '", language, "') .}
                OPTIONAL {?s xkos:exclusionNote ?Exclude . FILTER (LANG(?Exclude) = '", language, "').}
                OPTIONAL {?s xkos:additionalContentNote ?Include_Also . FILTER (LANG(?Include_Also) = '", language, "').}
              
              ")
  
  
  

  
  ### Define SPARQL query -- FILTER LEVEL
  SPARQL.query_level = paste0("FILTER (?Member = ", prefix, ":", "division", ")")
  
  ### End SPARQL query ", prefix 
  SPARQL.query_end = paste0("}
          ORDER BY ?", prefix
  )
  
  if (length(level) == 0 ){
    stop("Classification level was not specified.") 
  } else {  
    if (level == "ALL") {
      SPARQL.query = paste0(SPARQL.query_0, SPARQL.query_end)
    } else {
      SPARQL.query = paste0(SPARQL.query_0, SPARQL.query_level, SPARQL.query_end)
    }
    
  }
  response = httr::POST(url = source, accept("text/csv"), body = list(query = SPARQL.query), encode = "form")
  data = data.frame(content(response, show_col_types = FALSE))
  
  #keep only plainLiteral if more than one datatype // 
  #FAO - "http://www.w3.org/2001/XMLSchema#string"
  #CELLAR - "http://www.w3.org/2001/XMLSchema#string" - "http://www.w3.org/1999/02/22-rdf-syntax-ns#PlainLiteral"
  type = unique(data$datatype)
  if (length(type) > 1){
    data = data[which(data$datatype == "http://www.w3.org/1999/02/22-rdf-syntax-ns#PlainLiteral"), ]
  }
  
  #remove datatype col
  data = data[, 1:(ncol(data)-1)]
  
  #are there other duplicates? URL is the same and the other changes
  xcol = which(colnames(data) == "URL")
  dup = length(which(duplicated(data[,-xcol]) == TRUE))
  
  if (dup > 0) {
    warning("There are duplicates codes in the classification table.") 
  }
  
  #Get the good format before we got 
  data <- lapply(data, function(x) gsub("\n", " ", x))
  data <- as.data.frame(data)
  
  # Save results as CSV and show where it was stored
  if (CSVout == TRUE) {
    name_csv = paste0(prefix, "_table.csv")
    write.csv(data, file= name_csv, row.names=FALSE)
    message(paste0("The table was saved in ", getwd(), name_csv))
  } else if (is.character(CSVout)) {
    # if user provide a csv file 
    write.csv(data, file = CSVout, row.names = FALSE)
    message(paste0("The table was saved in ", getwd(), CSVout))
  }
  
  if (showQuery) {
    result=list()
    result[[1]]=SPARQL.query
    result[[2]]=data
    
    names(result)=c("SPARQL.query", "ClassificationTable")
    cat(result$SPARQL.query, sep ="/n")
  }
  
  if (showQuery==FALSE){
    result=data
  }
  
  return(result)
}
