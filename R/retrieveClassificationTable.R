#' @title Retrieve classifications and correspondence tables stored as Linked Open Data
#' @description Retrieve correspondence tables from the CELLAR and FAO repositories.
#' @param endpoint Character. SPARQL endpoint to query. Valid values: \code{"CELLAR"} or \code{"FAO"}.
#' @param prefix The namespace prefix identifying the classification. You can retrieve available prefixes using the \code{classificationList()} function.
#' @param conceptScheme Character. Unique identifier associated with a specific classification table.
#' @param level Character. Level to retrieve in a hierarchical classification. Default is \code{"ALL"}.
#' @param language Character. Language for labels, includes, and excludes. Default: \code{"en"}.
#' @param CSVout Logical or character. If \code{TRUE}, saves the table to a default CSV file. If a file path is provided, saves to that file.
#' Default is \code{NULL} (no file is saved).
#' @param showQuery Logical. If \code{TRUE}, returns the SPARQL query used along with the data. Default is \code{TRUE}.
#' @param localData Logical. If \code{TRUE}, retrieves internal (local) data rather than live SPARQL results. Default: \code{FALSE}.
#'
#' @return A list (if \code{showQuery = TRUE}) with the SPARQL query and the correspondence table, or just the table otherwise.
#'
#' @details
#' The behaviour of this function is contingent on the global option \code{useLocalDataForVignettes}:
#' The default behaviour (when the option is not set, or set to something else than \code{TRUE}), is that is queries live SPARQL endpoints online.
#' When the option is set to \code{TRUE} via \code{options(useLocalDataForVignettes = TRUE)}, the function returns local (embedded) data instead of querying live SPARQL endpoints.
#' This is useful for building vignettes or offline testing.
#'
#' @import httr
#' @export
#'
#' @examples
#' # \dontrun{
#' endpoint <- "CELLAR"
#' prefix <- "cn2022"
#' ID_table <- "CN2022_NST2007"
#' result <- tryCatch({
#'   retrieveCorrespondenceTable(endpoint, prefix, ID_table)
#' }, error = function(e) {
#'   message("SPARQL query failed: ", e$message)
#'   NULL
#' })
#'
#' if (!is.null(result)) {
#'   cat(result[[1]])      # Show SPARQL query
#'   head(result[[2]])     # Show top of result table
#' }
#' # }

retrieveClassificationTable = function(endpoint, prefix, conceptScheme, language = "en", level = "ALL", CSVout = NULL, showQuery = TRUE,localData = NULL) {
  #Check correctness of endpoint argument
  endpoint <- toupper(endpoint)
  if (!(endpoint %in% c("ALL", "FAO", "CELLAR"))) {
    stop(simpleError(paste("The endpoint value:", endpoint, "is not accepted")))
  }
  # Check the useLocalDataForVignettes option
  if (getOption("useLocalDataForVignettes", FALSE)) {
    localDataPath <- system.file("extdata", paste0(prefix, "_", language, ".csv"), package = "correspondenceTables")

    if (file.exists(localDataPath)) {
      # Read data from the local file if it exists
      data <- read.csv(localDataPath)
      # filter data according to level if not all
      if(level != "ALL"){
      data <- data[nchar(gsub("\\.", "", data[[prefix]])) == as.numeric(level), ]
      }
      if (showQuery) {
        print("Data loaded from local file.")
      }
      return(data)
    }
  } else {
    tryCatch(
      {

  ### Load the configuration file from GitHub
  config <- fromJSON("https://raw.githubusercontent.com/eurostat/correspondenceTables/refs/heads/main/inst/extdata/endpoint_source_config.json")
  ### Define endpoint
  if (endpoint == "CELLAR") {
    source <- config$CELLAR
  }
  if (endpoint == "FAO") {
    source <- config$FAO
  }

  ### Load prefixes using prefixList function
  prefixlist = prefixList(endpoint, prefix = prefix)
  prefixlist = as.character(paste(prefixlist, collapse = "\n"))

      }, error = function(e) {
        stop(simpleError(paste("Error in function retrieveClassificationTable, building of the SPARQL query failed.",endpoint,"is not available or is returning unexpected data.")))
      })

    tryCatch(
      {
  # # Check if classification has level, if not, set level = "ALL"
  dt_level = suppressMessages(dataStructure(endpoint, prefix, conceptScheme, language))

  if (nrow(dt_level) == 0 & level != "ALL") {
    level = "ALL"
    message("Classification has no levels, so level = ALL was set to retrieve the table.")
  }
      }, error = function(e) {
        stop(simpleError("Error in function retrieveClassificationTable, dataStructure() failed. Unable to check classification level"))
      })

    tryCatch(
      {
  ### Define SPARQL query -- BASE: all levels
  SPARQL.query_0 = paste0(prefixlist, "
        SELECT DISTINCT ?", prefix, " ?Name ?Level ?Parent ?Include ?Include_Also ?Exclude ?URL

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
                BIND (STR(?Label) as ?Name)
               #BIND (datatype(?notation) AS ?datatype)

               ?Member a xkos:ClassificationLevel;
               xkos:depth ?Depth;
              xkos:organizedBy ?L.
              ?L skos:prefLabel ?Level_Name.
               FILTER (LANG(?Level_Name)= '",language, "')
                BIND (STR(?Level_Name) as ?LEVEL_S )
                BIND (STR(?Depth) as ?Level )

                OPTIONAL {?s skos:scopeNote ?Include . FILTER (LANG(?Include) = '", language, "') .}
                OPTIONAL {?s xkos:exclusionNote ?Exclude . FILTER (LANG(?Exclude) = '", language, "').}
                OPTIONAL {?s xkos:additionalContentNote ?Include_Also . FILTER (LANG(?Include_Also) = '", language, "').}

              ")





  ### Define SPARQL query -- FILTER LEVEL
  #SPARQL.query_level = paste0("FILTER (?Member = ", prefix, ":", "division", ")")
  SPARQL.query_level = paste0("FILTER (?Depth =", level,")")

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

  }, error = function(e) {
    cat("The following SPARQL code was used in the call:\n", SPARQL.query, "\n")
    cat("The following response was given for by the SPARQL call:\n", response)
    stop(simpleError("Error in function retrieveClassificationTable, SPARQL query execution failed ."))
  })

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
  # if (CSVout == TRUE) {
  #   name_csv = paste0(prefix,"_", language, ".csv")
  #   write.csv(data, file= name_csv, row.names=FALSE)
  #   message(paste0("The table was saved in ", getwd(), name_csv))
  # } else if (is.character(CSVout)) {
  #   # if user provide a csv file
  #   write.csv(data, file = CSVout, row.names = FALSE)
  #   message(paste0("The table was saved in ", getwd(), CSVout))
  # }
  CsvFileSave(CSVout, data )

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
}

