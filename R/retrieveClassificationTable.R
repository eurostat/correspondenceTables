
#' @title Retrieve a full classification table from CELLAR or FAO
#'
#' @description
#' Retrieves the complete structure of a statistical classification published in  
#' either CELLAR or FAO, using SKOS/XKOS semantics.  
#'
#' The function:
#' \itemize{
#'   \item resolves the full ConceptScheme URI associated with the classification,  
#'         through endpoint‑aware matching, ASK‑based validation, and SPARQL discovery;
#'   \item anchors the extraction on the validated scheme URI;
#'   \item retrieves all concepts belonging to the scheme and their metadata;
#'   \item aggregates labels, hierarchical relations, inclusion/exclusion notes,  
#'         and optional depth information;
#'   \item returns one row per concept.
#' }
#'
#' The resulting table is suitable for classification browsing, integrity checks,  
#' hierarchical analysis, documentation, and downstream correspondence mapping.
#'
#' @param endpoint Character. Repository to query. Must be either  
#'   \code{"CELLAR"} or \code{"FAO"}.
#'
#' @param prefix Character. Classification prefix used for matching and URI resolution  
#'   (e.g. \code{"cn2022"}, \code{"cpc21"}, \code{"isic4"}).
#'
#' @param conceptScheme Character. Local identifier of the scheme  
#'   (often identical to \code{prefix}). The function automatically resolves this  
#'   to the canonical ConceptScheme URI published in the endpoint.
#'
#' @param language Character. Desired language for labels, scope notes and exclusion notes.  
#'   Default: \code{"en"}.
#'
#' @param level Character.  
#'   \itemize{
#'     \item \code{"ALL"} (default): return all levels in the hierarchy;
#'     \item a specific depth value (e.g. \code{"2"}) to filter concepts at that depth only.
#'   }
#'
#' @param CSVout Logical or character.  
#'   \itemize{
#'     \item \code{NULL}: do not export;  
#'     \item \code{TRUE}: export automatically to  
#'       \verb{<prefix>_<scheme>_<language>_classification.csv};  
#'     \item character: export to the provided filepath.
#'   }
#'
#' @param showQuery Logical.  
#'   \itemize{
#'     \item \code{FALSE} (default): return only the classification table;  
#'     \item \code{TRUE}: return a list containing the SPARQL query,  
#'           the resolved scheme URI, and the table itself.
#'   }
#'
#' @param knownSchemes Optional. A data frame supplying authoritative mappings  
#'   of the form \code{Prefix, ConceptScheme, URI[, Endpoint]}.  
#'   When provided, this overrides automatic discovery. To be obtained using \code{classificationList(endpoint)}.
#'
#' @param preferMappingOnly Logical.  
#'   If \code{TRUE}, the function *never* attempts SPARQL discovery and  
#'   uses only information in \code{knownSchemes} or \code{classificationList(endpoint)}.  
#'   Default: \code{FALSE}.
#'
#'
#' @return
#' If \code{showQuery = FALSE}, returns a \code{data.frame} with one row per concept  
#' and the following columns:
#'
#' \itemize{
#'
#'   \item \strong{Concept}  
#'         Full URI of the concept node.
#'
#'   \item \strong{Code}  
#'         Notation/code of the concept (\code{skos:notation}), coerced to string.
#'
#'   \item \strong{Label}  
#'         Human‑readable label of the concept in the requested language  
#'         (via \code{skos:prefLabel} or \code{skos:altLabel}, combined with COALESCE).
#'
#'   \item \strong{Depth}  
#'         Depth level in the classification hierarchy (\code{xkos:depth}),  
#'         when published.
#'
#'   \item \strong{BroaderList}  
#'         Concatenated list of broader concepts (URIs), separated by \code{" | "}.
#'
#'   \item \strong{BroaderCodeList}  
#'         Concatenated list of broader concept notations, aligned with \code{BroaderList}.
#'
#'   \item \strong{IncludeNotes}  
#'         Concatenated \code{skos:scopeNote} values describing what is *included*  
#'         in the meaning of the concept.
#'
#'   \item \strong{ExcludeNotes}  
#?         Concatenated \code{xkos:exclusionNote} values describing what is  
#'         *excluded* from the meaning of the concept.
#'
#' }
#'
#' If \code{showQuery = TRUE}, returns a list:
#' \itemize{
#'   \item \code{SPARQL.query} – the executed SPARQL string;
#'   \item \code{scheme_uri} – the resolved ConceptScheme URI;
#'   \item \code{ClassificationTable} – the data.frame described above.
#' }
#'
#'
#'
#' @examples
#' \dontrun{
#' # Retrieve full CN2022 classification (English)
#' cn <- retrieveClassificationTable(
#'   endpoint      = "CELLAR",
#'   prefix        = "cn2022",
#'   conceptScheme = "cn2022",
#'   language      = "en"
#' )
#'
#' # Retrieve CPC v2.1 from FAO and inspect query
#' out <- retrieveClassificationTable(
#'   endpoint      = "FAO",
#'   prefix        = "cpc21",
#'   conceptScheme = "cpc21",
#'   showQuery     = TRUE
#' )
#' cat(out$SPARQL.query)
#' }
#'
#' @import httr
#' @export


#  Main function ---------------------------

retrieveClassificationTable <- function(endpoint,
                                        prefix,
                                        conceptScheme,
                                        language  = "en",
                                        level     = "ALL",
                                        CSVout    = NULL,
                                        showQuery = FALSE,
                                        knownSchemes = NULL,
                                        preferMappingOnly = FALSE) {
  endpoint <- toupper(trimws(endpoint))
  if (!endpoint %in% c("CELLAR", "FAO")) stop("`endpoint` must be 'CELLAR' or 'FAO'.")
  prefix        <- trimws(prefix)
  conceptScheme <- trimws(conceptScheme)
  level         <- trimws(level)
  
  # Local data branch (optional, unchanged)
  if (isTRUE(getOption("useLocalDataForVignettes", FALSE))) {
    localDataPath <- system.file("extdata", paste0(prefix, "_", language, ".csv"), package = "correspondenceTables")
    if (nzchar(localDataPath) && file.exists(localDataPath)) {
      data <- utils::read.csv(localDataPath, stringsAsFactors = FALSE)
      if (!identical(toupper(level), "ALL") && "Depth" %in% names(data)) {
        data <- subset(data, as.character(Depth) == level)
      }
      return(if (isTRUE(showQuery))
        list(SPARQL.query = NA_character_, ClassificationTable = data)
        else data)
    }
  }
  
  # SPARQL endpoint
  endpoint_url <- if (endpoint == "CELLAR") {
    "http://publications.europa.eu/webapi/rdf/sparql"
  } else {
    "https://caliper.integratedmodelling.org/caliper/sparql"
  }
  
  # Scheme URI resolution (uses endpoint + optional external mapping)
  scheme_uri <- .resolve_scheme_uri(
    endpoint_url      = endpoint_url,
    endpoint          = endpoint,
    prefix            = prefix,
    conceptScheme     = conceptScheme,
    language          = language,
    knownSchemes      = knownSchemes,      # optional data.frame (Prefix, ConceptScheme, URI[, Endpoint])
    preferMappingOnly = preferMappingOnly  # TRUE = no discovery/fallback
  )
  if (!nzchar(scheme_uri)) {
    stop(sprintf("Could not resolve ConceptScheme for prefix='%s' conceptScheme='%s' in %s.",
                 prefix, conceptScheme, endpoint))
  }
  
  # Presence check (extra robustness)
  if (!isTRUE(.ask_scheme_exists(endpoint_url, scheme_uri))) {
    stop(paste0("ConceptScheme not present in this endpoint: ", scheme_uri))
  }
  
  # SPARQL prefix block
  prefix_block <- paste(
    "PREFIX skos: <http://www.w3.org/2004/02/skos/core#>",
    "PREFIX xkos: <http://rdf-vocabulary.ddialliance.org/xkos#>",
    "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>",
    sep = "\n"
  )
  
  # Optional depth filter (on base variable ?Depth0)
  filter_depth_inner <- ""
  if (!identical(toupper(level), "ALL")) {
    filter_depth_inner <- paste0("FILTER (STR(?Depth0) = '", level, "')")
  }
  
  # Compatible query (no subselect, no alias conflict, clean GROUP_CONCAT)
  SPARQL.query <- paste0(
    prefix_block, "\n",
    "SELECT ?Concept\n",
    "       (MIN(STR(?Notation)) AS ?Code)\n",
    "       (SAMPLE(?Label0)     AS ?Label)\n",
    "       (SAMPLE(?Depth0)     AS ?Depth)\n",
    "       (GROUP_CONCAT(DISTINCT STR(?Broader);         separator=\" | \")  AS ?BroaderList)\n",
    "       (GROUP_CONCAT(DISTINCT STR(?BroaderNotation); separator=\" | \")  AS ?BroaderCodeList)\n",
    "       (GROUP_CONCAT(DISTINCT ?Include;              separator=\" || \") AS ?IncludeNotes)\n",
    "       (GROUP_CONCAT(DISTINCT ?Exclude;              separator=\" || \") AS ?ExcludeNotes)\n",
    "WHERE {\n",
    "  ?Concept a skos:Concept ; skos:inScheme <", scheme_uri, "> ; skos:notation ?Notation .\n",
    "  # ---- Depth sul concetto (fallback)\n",
    "  OPTIONAL { ?Concept xkos:depth ?DepthFromConcept }\n",
    "  # ---- Depth via livello di classificazione (caso comune)\n",
    "  OPTIONAL {\n",
    "    ?Concept ^skos:member ?Level .\n",
    "    ?Level a xkos:ClassificationLevel .\n",
    "    ?Level xkos:depth ?DepthFromLevel .\n",
    "  }\n",
    "  BIND (COALESCE(?DepthFromLevel, ?DepthFromConcept) AS ?Depth0)\n",
    "  OPTIONAL { ?Concept skos:broader ?Broader . ?Broader skos:notation ?BroaderNotation }\n",
    "  OPTIONAL { ?Concept skos:prefLabel ?Pref FILTER (LANG(?Pref) = '", language, "') }\n",
    "  OPTIONAL { ?Concept skos:altLabel  ?Alt  FILTER (LANG(?Alt)  = '", language, "') }\n",
    "  BIND (COALESCE(?Pref, ?Alt) AS ?Label0)\n",
    "  OPTIONAL { ?Concept skos:scopeNote     ?Include FILTER (LANG(?Include) = '", language, "') }\n",
    "  OPTIONAL { ?Concept xkos:exclusionNote ?Exclude FILTER (LANG(?Exclude) = '", language, "') }\n",
    if (nzchar(filter_depth_inner)) paste0("  ", filter_depth_inner, "\n") else "",
    "}\n",
    "GROUP BY ?Concept\n",
    "ORDER BY ?Code\n"
  )
  
  # Execution: try POST forcing CSV, fallback to GET if needed
  resp <- httr::POST(
    url    = endpoint_url,
    body   = list(query = SPARQL.query, format = "text/csv"),
    encode = "form",
    httr::accept("text/csv"),
    httr::user_agent("correspondenceTables (R)")
  )
  
  if (httr::status_code(resp) >= 400L) {
    resp_get <- httr::GET(
      url   = endpoint_url,
      query = list(query = SPARQL.query, format = "text/csv"),
      httr::accept("text/csv"),
      httr::user_agent("correspondenceTables (R)")
    )
    httr::stop_for_status(resp_get)
    csv_text <- httr::content(resp_get, as = "text", encoding = "UTF-8")
  } else {
    csv_text <- httr::content(resp, as = "text", encoding = "UTF-8")
    # Rare: server responds XML on POST -> retry with GET
    if (grepl("^\\s*<\\?xml|<sparql", csv_text, ignore.case = TRUE)) {
      resp_get <- httr::GET(
        url   = endpoint_url,
        query = list(query = SPARQL.query, format = "text/csv"),
        httr::accept("text/csv"),
        httr::user_agent("correspondenceTables (R)")
      )
      httr::stop_for_status(resp_get)
      csv_text <- httr::content(resp_get, as = "text", encoding = "UTF-8")
    }
  }
  
  data <- .read_sparql_csv(csv_text)
  
  # Cleanup of embedded line-breaks in character fields
  data[] <- lapply(data, function(x) if (is.character(x)) gsub("\n", " ", x) else x)
  
  # CSV output (optional)
  if (isTRUE(CSVout)) {
    file_out <- paste0(prefix, "_", conceptScheme, "_", language, "_classification.csv")
    utils::write.csv(data, file_out, row.names = FALSE)
    message("Saved: ", file.path(getwd(), file_out))
  } else if (is.character(CSVout)) {
    utils::write.csv(data, CSVout, row.names = FALSE)
    message("Saved: ", CSVout)
  }
  
  if (isTRUE(showQuery)) {
    return(list(SPARQL.query = SPARQL.query, scheme_uri = scheme_uri, ClassificationTable = data))
  } else {
    return(data)
  }
}



# helpers (internal) ----------------------------------------------

#' @keywords internal
#' @noRd
# --- Read CSV with 'row.names' sanitization (prevents duplicate errors) ---
.read_sparql_csv <- function(csv_text) {
  if (!nzchar(csv_text)) return(data.frame())
  nl <- regexpr("\n", csv_text, fixed = TRUE)
  if (nl > 0) {
    header <- substr(csv_text, 1, nl - 1)
    body   <- substr(csv_text, nl + 1, nchar(csv_text))
    header <- sub("^row.names(,|$)", "row_names\\1", header)
    header <- gsub(",row.names,", ",row_names,", header, fixed = TRUE)
    header <- sub(",row.names$", ",row_names", header)
    csv_text <- paste0(header, "\n", body)
  }
  utils::read.csv(
    textConnection(csv_text),
    stringsAsFactors = FALSE,
    row.names = NULL,   # keep as a regular column
    check.names = FALSE,
    comment.char = ""   # do not treat '#' as comments
  )
}

#' @keywords internal
#' @noRd
# --- ASK: verify that the ConceptScheme exists on the current endpoint ---
.ask_scheme_exists <- function(endpoint_url, scheme_uri) {
  ask_q <- sprintf("
PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
ASK { ?c skos:inScheme <%s> }", scheme_uri)
  resp <- httr::GET(
    url   = endpoint_url,
    query = list(query = ask_q),
    httr::add_headers(Accept = "text/boolean, text/plain, application/sparql-results+json"),
    httr::user_agent("correspondenceTables (R) / ASK")
  )
  httr::stop_for_status(resp)
  txt <- httr::content(resp, as = "text", encoding = "UTF-8")
  grepl("\\btrue\\b", tolower(txt))
}

#' @keywords internal
#' @noRd
# --- Lightweight ConceptScheme discovery (SPARQL) ---
.discover_scheme_uri <- function(endpoint_url, prefix, conceptScheme, language = "en") {
  probe <- sprintf("
PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
SELECT ?scheme ?label WHERE {
  ?scheme a skos:ConceptScheme ;
          skos:prefLabel ?label .
  FILTER (LANG(?label) = '%s')
  FILTER (
    CONTAINS(LCASE(STR(?scheme)), LCASE('%s')) ||
    CONTAINS(LCASE(STR(?scheme)), LCASE('%s')) ||
    CONTAINS(LCASE(STR(?label)),  LCASE('%s')) ||
    CONTAINS(LCASE(STR(?label)),  LCASE('%s'))
  )
}
ORDER BY ?scheme", language, prefix, conceptScheme, prefix, conceptScheme)
  
  resp <- httr::GET(
    url   = endpoint_url,
    query = list(query = probe),
    httr::add_headers(Accept = "application/sparql-results+csv"),
    httr::user_agent("correspondenceTables (R) / SPARQL")
  )
  httr::stop_for_status(resp)
  csv <- httr::content(resp, as = "text", encoding = "UTF-8")
  df <- .read_sparql_csv(csv)
  if (nrow(df)) {
    score <- function(u, lbl) {
      s <- 0L
      if (grepl("/scheme/?$", u)) s <- s + 3L
      if (grepl(prefix, u, ignore.case = TRUE)) s <- s + 2L
      if (grepl(conceptScheme, u, ignore.case = TRUE)) s <- s + 2L
      if (grepl(prefix, lbl, ignore.case = TRUE)) s <- s + 1L
      if (grepl(conceptScheme, lbl, ignore.case = TRUE)) s <- s + 1L
      s
    }
    df$._score <- mapply(score, df$scheme, df$label, USE.NAMES = FALSE)
    df <- df[order(-df$._score, df$scheme), ]
    return(df$scheme[1])
  }
  
  # Second broader probe (for the prefix only)
  probe2 <- sprintf("
PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
SELECT ?scheme ?label WHERE {
  ?scheme a skos:ConceptScheme ;
          skos:prefLabel ?label .
  FILTER (LANG(?label) = '%s')
  FILTER (
    CONTAINS(LCASE(STR(?scheme)), LCASE('%s')) ||
    CONTAINS(LCASE(STR(?label)),  LCASE('%s'))
  )
}
ORDER BY ?scheme
LIMIT 50", language, prefix, prefix)
  
  resp2 <- httr::GET(
    url   = endpoint_url,
    query = list(query = probe2),
    httr::add_headers(Accept = "application/sparql-results+csv"),
    httr::user_agent("correspondenceTables (R) / SPARQL")
  )
  httr::stop_for_status(resp2)
  csv2 <- httr::content(resp2, as = "text", encoding = "UTF-8")
  df2 <- .read_sparql_csv(csv2)
  if (nrow(df2)) {
    df2$._ends_scheme <- grepl("/scheme/?$", df2$scheme)
    df2 <- df2[order(-as.integer(df2$._ends_scheme), df2$scheme), ]
    return(df2$scheme[1])
  }
  
  NA_character_
}

#' @keywords internal
#' @noRd
# --- Normalize an external mapping table (columns and trimming) ---
.normalize_known_schemes <- function(df) {
  if (is.null(df) || !is.data.frame(df) || !nrow(df)) return(NULL)
  nms <- names(df)
  nms <- sub("^concept\\s*scheme$", "ConceptScheme", nms, ignore.case = TRUE)
  nms <- sub("^conceptscheme$",     "ConceptScheme", nms, ignore.case = TRUE)
  nms <- sub("^concept_scheme$",    "ConceptScheme", nms, ignore.case = TRUE)
  nms <- sub("^prefix$",            "Prefix",        nms, ignore.case = TRUE)
  nms <- sub("^uri$",               "URI",           nms, ignore.case = TRUE)
  nms <- sub("^source$",            "Endpoint",      nms, ignore.case = TRUE)  # optional
  names(df) <- nms
  
  req_cols <- c("Prefix", "ConceptScheme", "URI")
  if (!all(req_cols %in% names(df))) return(NULL)
  
  keep <- intersect(c(req_cols, "Endpoint"), names(df))
  out  <- df[, keep, drop = FALSE]
  
  for (col in intersect(c("Prefix", "ConceptScheme", "URI", "Endpoint"), names(out))) {
    if (is.character(out[[col]])) out[[col]] <- trimws(out[[col]])
  }
  out
}

#' @keywords internal
#' @noRd
# --- Get the mapping: first external override, otherwise classificationList(endpoint) ---
.get_known_schemes_from_classificationList <- function(endpoint,
                                                       knownSchemes_override = NULL) {
  endpoint <- toupper(trimws(endpoint))
  if (!endpoint %in% c("FAO","CELLAR")) {
    stop("`endpoint` must be 'FAO' or 'CELLAR'.")
  }
  
  # 1) Use mapping passed from outside, if present
  ks <- .normalize_known_schemes(knownSchemes_override)
  if (!is.null(ks)) {
    if ("Endpoint" %in% names(ks)) {
      ks_src <- ks[toupper(ks$Endpoint) == endpoint, setdiff(names(ks), "Endpoint"), drop = FALSE]
      if (nrow(ks_src)) return(ks_src)
      # if the table is single-source or has no marked rows, use everything
    }
    return(ks)
  }
  
  # 2) Fallback: invoke classificationList(endpoint)
  if (!exists("classificationList", mode = "function")) return(NULL)
  df <- try(classificationList(endpoint), silent = TRUE)
  if (inherits(df, "try-error") || is.null(df)) return(NULL)
  ks2 <- .normalize_known_schemes(df)
  if (is.null(ks2)) return(NULL)
  ks2[, c("Prefix","ConceptScheme","URI"), drop = FALSE]
}

#' @keywords internal
#' @noRd
# --- Resolver: mapping -> discovery -> (pattern-based fallback only for CELLAR) ---
.resolve_scheme_uri <- function(endpoint_url,
                                endpoint,
                                prefix,
                                conceptScheme,
                                language = "en",
                                knownSchemes = NULL,
                                preferMappingOnly = FALSE) {
  endpoint <- toupper(trimws(endpoint))
  if (!endpoint %in% c("FAO","CELLAR")) {
    stop("`endpoint` must be 'FAO' or 'CELLAR'.")
  }
  
  # 1) Mapping (external override -> classificationList(endpoint))
  known_schemes <- .get_known_schemes_from_classificationList(
    endpoint = endpoint,
    knownSchemes_override = knownSchemes
  )
  
  if (!is.null(known_schemes) && nrow(known_schemes)) {
    ks_pref   <- tolower(trimws(known_schemes[["Prefix"]]))
    ks_cs     <- tolower(trimws(known_schemes[["ConceptScheme"]]))
    want_pref <- tolower(trimws(prefix))
    want_cs   <- tolower(trimws(conceptScheme))
    idx <- which(ks_pref == want_pref & ks_cs == want_cs)
    if (length(idx)) {
      hit <- known_schemes[idx, , drop = FALSE]
      # heuristic: prefer URIs that end with /scheme
      ends_scheme <- grepl("/scheme/?$", hit[["URI"]])
      ord <- order(!ends_scheme, hit[["URI"]], decreasing = TRUE)
      hit <- hit[ord, , drop = FALSE]
      uri <- hit[["URI"]][1]
      if (isTRUE(.ask_scheme_exists(endpoint_url, uri))) return(uri)
      if (isTRUE(preferMappingOnly)) return(NA_character_)  # requested: no discovery/fallback
    } else if (isTRUE(preferMappingOnly)) {
      return(NA_character_)
    }
  } else if (isTRUE(preferMappingOnly)) {
    return(NA_character_)
  }
  
  # 2) SPARQL discovery (if not blocked)
  disc <- try(.discover_scheme_uri(endpoint_url, prefix, conceptScheme, language), silent = TRUE)
  if (!inherits(disc, "try-error") && is.character(disc) && nzchar(disc)) {
    if (.ask_scheme_exists(endpoint_url, disc)) return(disc)
  }
  
  # 3) Legacy fallback only for CELLAR (validated with ASK)
  if (identical(endpoint, "CELLAR")) {
    pat <- paste0("http://data.europa.eu/xsp/", tolower(prefix), "/", conceptScheme)
    if (.ask_scheme_exists(endpoint_url, pat)) return(pat)
  }
  
  NA_character_
}