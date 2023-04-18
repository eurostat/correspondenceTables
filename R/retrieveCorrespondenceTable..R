#' @title Retrieve a correspondence tables from CELLAR/FAO or both.
#' @description Retrieve a correspondence tables from CELLAR/FAO or both.
#' @param prefix 
#' @param endpoint 
#' @param ID_table 
#' @param language By default set to "en". Optional 
#' @export
#' @return
#' \code{retrieveCorrespondenceTable()} returns a classification tables from CELLAR/FAO or both.


retrieveCorrespondenceTable = function(prefix, endpoint, ID_table, language = "en") {
  
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
  
  ## Define A and B
  ID_table_temp = gsub("-", "_", ID_table)
  ID_table_temp = gsub("__", "_", ID_table_temp)
  A = sub("_.*", "", ID_table_temp)
  B = sub(".*_", "", ID_table_temp)
  
  ### CLASSIFICATION TABLE SPARQL QUERIES
  ### Define SPARQL query -- BASE
  SPARQL.query_0 = paste0(prefixlist, "
    SELECT ?", A ," ?Label_", A ," ?", B ," ?Label_", B ,"  ?Comment ?URL 

    WHERE {
     ", prefix, ":", ID_table, " xkos:madeOf ?Associations .
     ?Associations xkos:sourceConcept ?Source .
     OPTIONAL  {?Associations xkos:targetConcept ?Target .}
     OPTIONAL  {?Associations rdfs:comment ?Comment . }  

     ?Source   skos:notation ?SourceNotation .
     ?Target   skos:notation ?TargetNotation .

     FILTER ( datatype(?SourceNotation) = xsd:string)
     FILTER ( datatype(?TargetNotation) = xsd:string)
     
     BIND (STR(?Associations ) AS ?URL)
     BIND (STR(?SourceNotation) as ?", A ,") 
     BIND (STR(?TargetNotation) as ?", B ,")

     OPTIONAL { ?Source skos:prefLabel ?Label_", A ,"  FILTER (LANG(?Label_", A ,") = '", language, "') .}
     OPTIONAL { ?Target skos:prefLabel ?Label_", B ,"  FILTER (LANG(?Label_", B ,") = '", language, "') .}
     

   ")

  ### End SPARQL query ", prefix 
  SPARQL.query_end = paste0("}
          ORDER BY ?Source
         ")
  
  SPARQL.query = paste0(SPARQL.query_0, SPARQL.query_end)
  
  response = httr::POST(url = source, accept("text/csv"), body = list(query = SPARQL.query), encode = "form")
  data = data.frame(content(response))

  #remove duplicates when only code name is different 
  #data = data[which(duplicated(data[,-c(1,3)]) == FALSE), ]
  #View(data)
  
  #are there other duplicates? URL is the same and the other changes
  #xcol = which(colnames(data) == "URL")
  #dup = length(which(duplicated(data[,-xcol]) == TRUE))
  
  #if (dup > 0) {
  #  warning("There are duplicates codes in the classification table.") 
  #}
  
  #result = list(data, status)
  return(data)
}