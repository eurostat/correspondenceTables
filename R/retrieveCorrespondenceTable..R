#' @title Retrieve a correspondence tables from CELLAR and FAO.
#' @description Retrieve a correspondence tables from CELLAR and FAO.
#' @param prefix The SPARQL instruction for a declaration of a namespace prefix. It can be found using the classEndpoint() function.
#' @param endpoint The SPARQL Endpoint, the valid values are \code{"CELLAR"} or \code{"FAO"}. 
#' @param ID_table The ID of the correspondence table which can be found using the correspondenceList() function.
#' @param language Language of the table. By default is set to "en". This is an optional argument.
#' @param CSVout The valid values are \code{FALSE} or \code{TRUE}. In both cases the correspondence table as an R object. 
#' If output should be saved as a csv file, the argument should be set as \code{TRUE}. By default, no csv file is produced. 
#' @import httr
#' @export
#' @return
#' \code{retrieveCorrespondenceTable()} returns a classification tables from CELLAR and FAO. The table includes the following variables:
#'  \itemize{
#'     \item Source Classification name (e.g. cn2019): the code of each object in the source classification
#'     \item Source Classification label: the corresponding label of each object 
#'     \item Target Classification name (e.g. cn2021): the code of each object in the target classification
#'     \item Target Classification label: the corresponding label of each object 
#'     \item Comment: details on each object, if available 
#'     \item URL: the URL from which the SPARQL query was retrieved
#' }
#' @examples
#' {
#'     endpoint = "CELLAR"
#'     prefix = "nace2"
#'     ID_table = "NACE2_PRODCOM2021"
#'     language = "fr"
#'     dt = retrieveCorrespondenceTable(prefix, endpoint, ID_table, language)
#'     View(dt)
#'     }


retrieveCorrespondenceTable = function(prefix, endpoint, ID_table, language = "en", CSVout =  FALSE) {
  
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

  # Save results as CSV and show where it was stored
  if (CSVout == TRUE) {
    name_csv = paste0(ID_table, "_table.csv")
    write.csv(data, file= name_csv, row.names=FALSE)
    message(paste0("The correspondence table was saved in ", getwd(), name_csv))
  } 
  
  
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