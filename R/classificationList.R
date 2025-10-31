#' @title List available classification schemes from CELLAR or FAO
#' @description List all classifications tables in the CELLAR or FAO repositories, 
#' a list of all classifications that are available in that endpoint.
#' Apart from basic information, the list also contains, for each classification listed,
#' those elements (prefix name, URI, key, concept scheme, and title) that are necessary for parameterising a SPARQL call to extract the actual classification structure.
#' @param endpoint One of "CELLAR", "FAO", or "ALL" (default).
#' @param showQuery Logical. If TRUE, returns the SPARQL query along with the data.
#' @details
#' The behaviour of this function is contingent on the global option \code{useLocalDataForVignettes}:
#' The default behaviour (when the option is not set, or set to something else than \code{TRUE}), is that is queries live SPARQL endpoints online.
#' When the option is set to \code{TRUE} via \code{options(useLocalDataForVignettes = TRUE)}, the function returns local (embedded) data instead of querying live SPARQL endpoints.
#' This is useful for building vignettes or offline testing.
#'
#' @return 
#' If \code{endpoint} is "CELLAR" or "FAO", the function returns a data frame with the following columns:
#' \itemize{
#'   \item \strong{Prefix}: The classification prefix (e.g., domain or acronym)
#'   \item \strong{ConceptScheme}: The identifier for the concept scheme
#'   \item \strong{URI}: The full URI of the concept scheme
#'   \item \strong{Title}: The human-readable title of the classification scheme
#' }
#'
#' If \code{endpoint} is "ALL", the function returns a named list with two such data frames: one for \code{"CELLAR"} and one for \code{"FAO"}.
#'
#' @import httr
#' @import jsonlite
#' @export

classificationList <- function(endpoint = "ALL", showQuery = FALSE) {
  endpoint <- toupper(endpoint)
  if (!(endpoint %in% c("ALL", "FAO", "CELLAR"))) {
    stop(simpleError(paste("The endpoint value:", endpoint, "is not accepted")))
  }
  
  if (endpoint == "ALL") {
    return(list(
      CELLAR = classificationList("CELLAR", showQuery = showQuery),
      FAO = classificationList("FAO", showQuery = showQuery)
    ))
  }
  
  # When the global option `useLocalDataForVignettes` is set to TRUE,
  # this function returns pre-saved static data instead of querying SPARQL endpoints.
  # This behaviour ensures that vignettes build reproducibly, even without internet access.
  # The corresponding CSV files are stored under inst/extdata in the package.
  if (getOption("useLocalDataForVignettes", FALSE)) {
    path <- system.file("extdata", paste0("classificationList_", endpoint, ".csv"), package = "correspondenceTables")
    if (file.exists(path)) {
      return(read.csv(path))
    } else {
      stop(paste("Local file for", endpoint, "is missing."))
    }
  }
  
  # SPARQL setup
  SPARQL.query <- ""
  endpoint_url <- ""
  
  tryCatch({
    config <- fromJSON("https://raw.githubusercontent.com/eurostat/correspondenceTables/refs/heads/main/inst/extdata/endpoint_source_config.json")
    
    if (endpoint == "CELLAR") {
      endpoint_url <- config$CELLAR
      SPARQL.query <- "
        SELECT DISTINCT ?s ?Title
        WHERE { ?s a skos:ConceptScheme ;
                skos:prefLabel ?Title ;
                ?p <http://publications.europa.eu/resource/authority/corporate-body/ESTAT> 
                 FILTER (LANG(?Title) = 'en')}
        ORDER BY ?Title
      "
    } else {
      # The only remaining valid case here is "FAO"
      endpoint_url <- config$FAO
      SPARQL.query <- "
        PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
        PREFIX skos: <http://www.w3.org/2004/02/skos/core#>

        SELECT ?scheme ?notation ?label_en WHERE {
          ?scheme rdf:type skos:ConceptScheme .
          ?scheme skos:notation ?notation .
          ?scheme skos:prefLabel ?label_en . FILTER(lang(?label_en)='en')
        }
        ORDER BY ASC(?notation)
      "
    }
    
    response <- POST(url = endpoint_url, httr::accept("text/csv"), body = list(query = SPARQL.query), encode = "form")
    df <- read.csv(text = content(response, "text"), sep = ",")
    print(head(df))
    print(str(df))
    
    if (endpoint == "CELLAR") {
      str_dt <- t(sapply(df[, 1], function(x) unlist(strsplit(as.character(x), "/+"))))
      uri <- paste0(str_dt[, 1], "/", "/", str_dt[, 2], "/", str_dt[, 3], "/", str_dt[, 4])
      prefix <- gsub("\\.", "", str_dt[, 4])
      conceptscheme <- str_dt[, 5]
      title <- df[, 2]
    } else {
      # The only remaining valid case here is "FAO"
      str_dt <- strsplit(df[, 1], "/")
      mat_str_dt <- suppressWarnings(do.call(rbind, str_dt))
      df_str_dt <- as.data.frame(mat_str_dt)
      
      prefix <- df[, 2]
      
      if (ncol(df_str_dt) >= 6) {
        conceptscheme <- paste0(df_str_dt[, 5], df_str_dt[, 6])
      } else {
        conceptscheme <- rep("unknown", nrow(df))
      }
      
      # NOTE: workaround for FAO URIs
      # FAO endpoint sometimes returns incomplete URIs, so we use the local fallback as backup.
      # (This behavior was introduced by Bienvenu, Loïc)
      # See review request 0.10.22/015
      uri_path <- system.file("extdata", "classificationList_FAO.csv", package = "correspondenceTables")
      if (file.exists(uri_path)) {
        uri <- read.csv(uri_path)[, 3]
      } else {
        uri <- df[, 1]
      }
      
      title <- df[, 3]
    }
    
    result <- data.frame(
      Prefix = prefix,
      ConceptScheme = conceptscheme,
      URI = uri,
      Title = title,
      stringsAsFactors = FALSE
    )
    
    rownames(result) <- 1:nrow(result)
    
    if (showQuery) {
      return(list("SPARQL.query" = SPARQL.query, "ClassificationList" = result))
    } else {
      return(result)
    }
    
  }, error = function(e) {
    message("The following SPARQL code was used in the call:\n", SPARQL.query)
    message("The above SPARQL call to ", endpoint_url, " generated the following error message:\n", conditionMessage(e))
    stop(simpleError(paste("Error in function ClassificationList(", endpoint, "), Endpoint", endpoint, "is not available or is returning unexpected data")))
  })
}
