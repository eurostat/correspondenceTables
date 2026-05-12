#' @title List available classification schemes from CELLAR or FAO
#'
#' @description
#' List all classification schemes available on the selected repository
#' (CELLAR, FAO, or both). Returns, for each scheme, its prefix, identifier,
#' URI, English title, and the list of languages detected on its concepts.
#'
#' @param endpoint Character scalar. One of "CELLAR", "FAO" or "ALL".
#'                 "ALL" queries both repositories and returns a named list.
#' @param showQuery Logical; if TRUE, returns also the SPARQL query used.
#'
#' @return
#' If endpoint is "CELLAR" or "FAO" and showQuery = FALSE: a data.frame with
#' columns Prefix, ConceptScheme, URI, Title, Languages.
#'
#' If showQuery = TRUE: a list with "ClassificationList" and "SPARQL.query".
#'
#' If endpoint = "ALL": a named list with $CELLAR and $FAO. If showQuery = TRUE,
#' each element is itself a list (ClassificationList + SPARQL.query).
#'
#' @examples
#' \dontrun{
#' head(classificationList("CELLAR"))
#' res <- classificationList("FAO", showQuery = TRUE)
#' names(res)
#' }
#'
#' @import httr
#' @importFrom utils read.csv
#' @export
classificationList <- function(endpoint = "ALL", showQuery = FALSE) {
  # Validate and normalize endpoint using the internal helper.
  # .expand_endpoints() returns a vector of endpoint names.
  # Example: "ALL" -> c("CELLAR", "FAO")
  endpoints <- .expand_endpoints(endpoint)
  
  # If multiple endpoints were requested, return a named list.
  if (length(endpoints) > 1L) {
    out <- setNames(vector("list", length(endpoints)), endpoints)
    for (ep in endpoints) {
      out[[ep]] <- .ct_classification_list_single(ep, showQuery = showQuery)
    }
    return(out)
  }
  
  # Single endpoint case.
  .ct_classification_list_single(endpoints, showQuery = showQuery)
}

# -------------------------------------------------------------------
# Internal: run the SPARQL query for a single endpoint
# -------------------------------------------------------------------
#' @keywords internal
#' @noRd
.ct_classification_list_single <- function(endpoint, showQuery = FALSE) {
  
  # Resolve the endpoint URL via internal single source of truth
  endpoint_url <- .sparql_endpoint(endpoint)
  
  # Choose the SPARQL query by endpoint
  if (identical(endpoint, "CELLAR")) {
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

  FILTER (
    STRSTARTS(STR(?scheme), 'http://data.europa.eu/') ||
    STRSTARTS(STR(?scheme), 'https://data.europa.eu/')
  )

  ?scheme ?p <http://publications.europa.eu/resource/authority/corporate-body/ESTAT> .

  OPTIONAL { ?scheme skos:notation ?notation }

  ?concept a skos:Concept ;
           skos:inScheme ?scheme ;
           skos:prefLabel ?clab .
  FILTER (LANG(?clab) != '')
}
GROUP BY ?scheme ?notation
ORDER BY ?Prefix
"
  } else if (identical(endpoint, "FAO")) {
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
  } else {
    stop("Unsupported endpoint in .ct_classification_list_single().")
  }

# Optionally print the query for debugging
if (isTRUE(showQuery)) {
  message("Endpoint: ", endpoint_url)
  message(SPARQL.query)
}

# Execute the request and parse CSV response
out <- tryCatch({
  resp <- httr::POST(
    url    = endpoint_url,
    httr::accept("text/csv"),
    body   = list(query = SPARQL.query),
    encode = "form"
  )
  txt <- httr::content(resp, "text", encoding = "UTF-8")
  df  <- utils::read.csv(text = txt, sep = ",", stringsAsFactors = FALSE, check.names = FALSE)
  
  # Basic post-conditions: expected columns should exist
  needed <- c("URI", "Prefix", "Title", "Languages")
  if (!all(needed %in% names(df))) {
    stop("Unexpected columns returned by SPARQL endpoint. Got: ",
         paste(names(df), collapse = ", "))
  }
  
  # Prepare result structure; normalize prefix, extract basename of URI
  uri    <- df[["URI"]]
  prefix <- if (identical(endpoint, "CELLAR")) gsub("\\.", "", df[["Prefix"]]) else df[["Prefix"]]
  prefix <- .ct_clean_prefix(prefix)
  title  <- df[["Title"]]
  langs  <- df[["Languages"]]
  
  result <- data.frame(
    Prefix        = prefix,
    ConceptScheme = .ct_basename_uri(uri),
    URI           = uri,
    Title         = title,
    Languages     = langs,
    stringsAsFactors = FALSE
  )
  rownames(result) <- seq_len(nrow(result))
  result
}, error = function(e) {
  # Provide helpful diagnostics while keeping a clean error for callers
  message("The following SPARQL code was used in the call:\n", SPARQL.query)
  message("The above SPARQL call to ", endpoint_url,
          " generated the following error message:\n", conditionMessage(e))
  stop(simpleError(
    paste0("Error in classificationList(", endpoint, "): endpoint unavailable or returned unexpected data")
  ))
})

# Return either the data.frame alone or a list with the query
if (isTRUE(showQuery)) {
  return(list("SPARQL.query" = SPARQL.query, "ClassificationList" = out))
} else {
  return(out)
}
}

# -------------------------------------------------------------------
# Internal utilities
# -------------------------------------------------------------------

#' @keywords internal
#' @noRd
.ct_basename_uri <- function(x) {
  # Keep only the last path/token after '/' or '#', then sanitize
  out <- sub("^.*[/#]", "", x)
  gsub("[^A-Za-z0-9_\\-\\.]", "", out)
}

#' @keywords internal
#' @noRd
.ct_clean_prefix <- function(prefix) {
  # Normalize prefixes: lowercase, drop common version suffixes,
  # remove dots/spaces and keep a safe alphanumeric/underscore set
  prefix <- tolower(prefix)
  prefix <- sub("(_v[0-9.]+)$", "", prefix)
  prefix <- sub("(-v[0-9.]+)$", "", prefix)
  prefix <- gsub("\\.", "", prefix)
  prefix <- gsub(" ", "", prefix)
  prefix <- gsub("[^a-z0-9_]", "", prefix)
  trimws(prefix)
}

