#' @title Overview of all the available correspondence classification from CELLAR and FAO repository.
#' @description Provides an overview of all the available correspondence classification from CELLAR and FAO repository.
#' @param endpoint SPARQL endpoints provide a standardized way to access data sets, 
#' making it easier to retrieve specific information or perform complex queries on linked data. 
#' The valid values are \code{"CELLAR"}, \code{"FAO"} or \code{"ALL"} for both.
#' @import httr
#' @return
#' \code{correspondenceTableList()} returns a list of the correspondence tables available with prefix name, ID, Source classification, 
#' Target classification, Table name and URI.
#' @examples
#' {
#'     corr_list = correspondenceTableList("ALL")
#'     }


    
correspondenceTableList = function(endpoint) {
  #Check correctness of endpoint argument
endpoint <- toupper(endpoint)
if (!(endpoint %in% c("ALL", "FAO", "CELLAR"))) {
  stop(simpleError(paste("The endpoint value:", endpoint, "is not accepted")))
}
  
  if (endpoint == "ALL") {
    cycle = 1:2
  }
  if (endpoint == "CELLAR") {
    cycle = 1
  }
  if (endpoint == "FAO") {
    cycle = 2
  }
  
  data = list()
  
  for (j in cycle) {
    e = c("CELLAR", "FAO")[j]
  
    ### Define endpoint
    if (e == "CELLAR") {
      source = "https://publications.europa.eu/webapi/rdf/sparql"
      sep = "_"
      rm  = 1:16
    }
    if (e == "FAO") {
      source = "https://stats.fao.org/caliper/sparql/AllVocs"
      sep = "-"
      rm  = 1:16
    }
  
    ## Create Prefixes list 
    prefixlist = prefixList(e)
    prefixlist = as.character(paste(prefixlist, collapse = "\n"))
    
    prefixes_loop = unlist(lapply(strsplit(as.character(prefixList(e)), " "), function(x) x[2]))
    prefixes_loop = prefixes_loop[-rm]
  
    data_t = list()
  
    for (i in 1:length(prefixes_loop)){
    prefix = prefixes_loop[i]
    
    tryCatch(
      {
    
    SPARQL.query = paste0(prefixlist, "
       SELECT ?ID_table ?A ?B ?Table ?URL 

       WHERE {
             ?s a xkos:Correspondence ;
             skos:prefLabel ?Label .
    
            BIND (STR(?s) AS ?URL)
            BIND (STR(?Label) AS ?Table)
  
            BIND (STRAFTER(STR(?s), STR(", prefix, ")) AS ?ID_table)
   		      BIND (STRAFTER(STR(?ID_table), '", sep, "') AS ?B)
  		      BIND (STRBEFORE(STR(?ID_table), '", sep, "') AS ?A)  
  
            FILTER (STRLEN(?ID_table) != 0) 
        }
      ")
    
    response = httr::POST(url = source, accept("text/csv"), body = list(query = SPARQL.query), encode = "form")
    data_t[[i]] = data.frame(content(response, show_col_types = FALSE))
    
      }, error = function(e) {
        stop(simpleError(paste0("Error in function correspondenceTableList(", endpoint,"), endpoint is not available or is returning unexpected data")))
        cat("The following SPARQL code was used in the call:\n", SPARQL.query, "\n")
        cat("The following response was given for by the SPARQL call:\n", response)
      })
    
    if (nrow(data_t[[i]]) == 0){
      data_t[[i]] = cbind(prefix = character(), data_t[[i]])
    } else {
      data_t[[i]] = cbind(prefix = rep(gsub(":","",prefix), nrow(data_t[[i]])), data_t[[i]])
    }
      
    #if (nrow(data_t[[i]]) > 0){
    #  data_t[[i]]$prefix = gsub(":","",prefix)
    #  data_t[[i]] = data_t[[i]][,c(6, 1:5)]
    #  names(data_t)[2] = "ID_table"
    #}
    names(data_t)[i] = prefix
    }
    
    data[[j]] = data_t
    names(data)[j] = c("CELLAR", "FAO")[j]
  }
    
  return(data)
  
}


