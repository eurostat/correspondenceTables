#' @title List available classification schemes from CELLAR or FAO
#'
#' @description
#' This function extracts, from the endpoint of interest (CELLAR or FAO),
#' a list of all classifications that are available in that endpoint.
#' Apart from basic information, the list also contains, for each
#' classification listed, all elements that are required to parameterise
#' a SPARQL call to extract the actual classification structure.
#' These elements include:
#' \itemize{
#'   \item the prefix name,
#'   \item the full URI of the concept scheme,
#'   \item the concept-scheme identifier (derived from the URI),
#'   \item the human-readable title (English label),
#'   \item and the list of languages observed among the concepts belonging
#'         to the scheme.
#' }
#'
#' The function can operate in two modes. By default, it queries live SPARQL
#' endpoints online. If the global option
#' \code{options(useLocalDataForVignettes = TRUE)} is set, the function
#' instead loads embedded CSV files located in \code{inst/extdata} and
#' bypasses live SPARQL calls, which is useful for vignettes or offline work.
#'
#' When \code{endpoint = "ALL"}, the function queries both CELLAR and FAO and
#' returns the two results in a named list. If \code{showQuery = TRUE},
#' the function also returns the SPARQL query (or queries) used to generate
#' the output.
#'
#' @details
#' The URLs of the SPARQL endpoints are obtained from an online JSON
#' configuration file (\code{endpoint_source_config.json}). If this file
#' cannot be loaded, the function falls back to built-in default URLs.
#'
#' In online mode, languages are inferred from the language tags of the
#' \code{skos:prefLabel} of all concepts belonging to each scheme. All
#' distinct languages are returned as a comma-separated character vector.
#'
#' When operating offline (via
#' \code{options(useLocalDataForVignettes = TRUE)}), the function returns the
#' pre-computed CSV data for the requested endpoint(s). This allows vignettes
#' to be built reproducibly and without network access.
#'
#' @return
#' If \code{endpoint} is \code{"CELLAR"} or \code{"FAO"} and
#' \code{showQuery = FALSE}, the function returns a data frame with the
#' following columns:
#' \itemize{
#'   \item \strong{Prefix}: Short identifier of the classification scheme.
#'   \item \strong{ConceptScheme}: Identifier of the concept scheme
#'         (derived from the URI).
#'   \item \strong{URI}: Full URI of the concept scheme.
#'   \item \strong{Title}: Human-readable English title of the scheme.
#'   \item \strong{Languages}: Comma-separated list of language tags
#'         observed for concepts in the scheme.
#' }
#'
#' If \code{endpoint} is \code{"CELLAR"} or \code{"FAO"} and
#' \code{showQuery = TRUE}, the function returns a list with two
#' elements:
#' \itemize{
#'   \item \code{"ClassificationList"}: The data frame described above.
#'   \item \code{"SPARQL.query"}: The SPARQL query string that was sent
#'         to the endpoint.
#' }
#'
#' If \code{endpoint = "ALL"} and \code{showQuery = FALSE}, the function
#' returns a named list with two elements, \code{$CELLAR} and \code{$FAO},
#' each containing a data frame structured as described above.
#'
#' If \code{endpoint = "ALL"} and \code{showQuery = TRUE}, the function
#' returns a named list with two elements, \code{$CELLAR} and \code{$FAO};
#' each of these elements is itself a list with two components:
#' \code{"ClassificationList"} (the data frame for that endpoint) and
#' \code{"SPARQL.query"} (the query used to obtain it).
#'
#' @examples
#' \donttest{
#' # Example 1: default behaviour (live query — requires internet)
#' # head(classificationList("CELLAR"))
#'
#' # Example 2: offline mode using embedded CSVs
#' options(useLocalDataForVignettes = TRUE)
#' head(classificationList("FAO"))
#'
#' # Example 3: retrieve SPARQL query for debugging
#' res <- classificationList("CELLAR", showQuery = TRUE)
#' names(res)
#' }
#'
#' @import httr
#' @import jsonlite
#' @export

classificationList <- function(endpoint = "ALL", showQuery = FALSE) {
  endpoint <- toupper(endpoint)
  if (!(endpoint %in% c("ALL", "FAO", "CELLAR"))) {
    stop(simpleError(paste("The endpoint value:", endpoint, "is not accepted")))
  }
  
  # ---- Offline / vignette mode ----
  if (getOption("useLocalDataForVignettes", FALSE)) {
    if (endpoint == "ALL") {
      return(list(
        CELLAR = classificationList("CELLAR", showQuery = showQuery),
        FAO    = classificationList("FAO",    showQuery = showQuery)
      ))
    }
    path <- system.file("extdata", paste0("classificationList_", endpoint, ".csv"),
                        package = "correspondenceTables")
    if (file.exists(path)) {
      return(utils::read.csv(path, stringsAsFactors = FALSE))
    } else {
      stop(paste("Local file for", endpoint, "is missing."))
    }
  }
  
  # ---- Helpers ----
  basename_uri <- function(x) {
    out <- sub("^.*[/#]", "", x)
    gsub("[^A-Za-z0-9_\\-\\.]", "", out)
  }
  
  # ---- Load endpoints config (with fallback) ----
  cfg_url <- "https://raw.githubusercontent.com/eurostat/correspondenceTables/refs/heads/main/inst/extdata/endpoint_source_config.json"
  config <- tryCatch(jsonlite::fromJSON(cfg_url), error = function(e) NULL)
  if (is.null(config)) {
    # Fallbacks (ajuste au besoin)
    config <- list(
      CELLAR = "https://publications.europa.eu/webapi/rdf/sparql",
      FAO    = "https://stats.fao.org/caliper/sparql"
    )
  }
  
  # ---- Build SPARQL per endpoint (Variant B) ----
  SPARQL.query <- ""
  endpoint_url <- ""
  
  if (endpoint == "ALL") {
    return(list(
      CELLAR = classificationList("CELLAR", showQuery = showQuery),
      FAO    = classificationList("FAO",    showQuery = showQuery)
    ))
  } else if (endpoint == "CELLAR") {
    endpoint_url <- config$CELLAR
    # Variant B + data.europa.eu filter + exclude eurovoc + ESTAT link; languages from concepts
    SPARQL.query <- "
PREFIX skos: <http://www.w3.org/2004/02/skos/core#>

SELECT
  (?scheme AS ?URI)
  (COALESCE(STR(?notation),
            REPLACE(REPLACE(STR(?scheme), '^.*/', ''), '[^A-Za-z]+.*', '')) AS ?Prefix)
  (COALESCE(SAMPLE(STR(?title_en)), '') AS ?Title)
  (GROUP_CONCAT(DISTINCT LANG(?clab); SEPARATOR=', ') AS ?Languages)
WHERE {
  ?scheme a skos:ConceptScheme ;
          skos:prefLabel ?title_en .
  FILTER (LANG(?title_en) = 'en')

  # Garder seulement data.europa.eu (http ou https) sans REGEX
  FILTER (
    STRSTARTS(STR(?scheme), 'http://data.europa.eu/') ||
    STRSTARTS(STR(?scheme), 'https://data.europa.eu/')
  )

  # (Inutile d’exclure EuroVoc si on restreint à data.europa.eu)
  ?scheme ?p <http://publications.europa.eu/resource/authority/corporate-body/ESTAT> .

  OPTIONAL { ?scheme skos:notation ?notation }

  # Langues observées au niveau des concepts
  ?concept a skos:Concept ;
           skos:inScheme ?scheme ;
           skos:prefLabel ?clab .
  FILTER (LANG(?clab) != '')
}
GROUP BY ?scheme ?notation
ORDER BY ?Prefix
"
  } else {
    endpoint_url <- config$FAO
    # Variant B for FAO (languages from concepts, EN title at scheme level)
    SPARQL.query <- "
PREFIX rdf:  <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
SELECT
  (?scheme AS ?URI)
  (COALESCE(STR(?notation),
            REPLACE(REPLACE(STR(?scheme), '^.*/', ''), '[^A-Za-z]+.*', '')) AS ?Prefix)
  (COALESCE(SAMPLE(STR(?title_en)), '') AS ?Title)
  (GROUP_CONCAT(DISTINCT LANG(?clab); SEPARATOR=', ') AS ?Languages)
WHERE {
  ?scheme rdf:type skos:ConceptScheme ;
          skos:prefLabel ?title_en .
  FILTER (LANG(?title_en) = 'en')

  OPTIONAL { ?scheme skos:notation ?notation }

  ?concept a skos:Concept ;
           skos:inScheme ?scheme ;
           skos:prefLabel ?clab .
  FILTER (LANG(?clab) != '')
}
GROUP BY ?scheme ?notation
ORDER BY ?Prefix
"
  }

if (isTRUE(showQuery)) {
  message("Endpoint: ", endpoint_url)
  message(SPARQL.query)
}

# ---- Execute query ----
out <- tryCatch({
  resp <- httr::POST(
    url    = endpoint_url,
    httr::accept("text/csv"),
    body   = list(query = SPARQL.query),
    encode = "form"
  )
  txt <- httr::content(resp, "text", encoding = "UTF-8")
  df  <- utils::read.csv(text = txt, sep = ",", stringsAsFactors = FALSE, check.names = FALSE)
  
  needed <- c("URI", "Prefix", "Title", "Languages")
  if (!all(needed %in% names(df))) {
    stop("Unexpected columns returned by SPARQL endpoint. Got: ",
         paste(names(df), collapse = ", "))
  }
  
  uri    <- df[["URI"]]
  prefix <- if (endpoint == "CELLAR") gsub("\\.", "", df[["Prefix"]]) else df[["Prefix"]]
  title  <- df[["Title"]]
  langs  <- df[["Languages"]]
  
  result <- data.frame(
    Prefix        = prefix,
    ConceptScheme = basename_uri(uri),
    URI           = uri,
    Title         = title,
    Languages     = langs,
    stringsAsFactors = FALSE
  )
  rownames(result) <- seq_len(nrow(result))
  result
}, error = function(e) {
  message("The following SPARQL code was used in the call:\n", SPARQL.query)
  message("The above SPARQL call to ", endpoint_url, " generated the following error message:\n", conditionMessage(e))
  stop(simpleError(paste("Error in function ClassificationList(", endpoint, "), Endpoint", endpoint, "is not available or is returning unexpected data")))
})

if (showQuery) {
  return(list("SPARQL.query" = SPARQL.query, "ClassificationList" = out))
} else {
  return(out)
}
}
