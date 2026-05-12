#' @title Retrieve a full classification table from CELLAR or FAO
#'
#' @description
#' Retrieves the complete structure of a statistical classification published in
#' either CELLAR or FAO repositories based on a Prefix–ConceptScheme pair obtained
#' using \code{classificationList()}.
#'
#' The resulting table is suitable for classification browsing, integrity checks,
#' hierarchical analysis, documentation, and downstream correspondence mapping.
#'
#' @param endpoint Character. Repository to query. Must be either "CELLAR" or "FAO".
#' @param prefix Character. Classification prefix used for matching and URI resolution
#'   (e.g. "cn2022", "cpc21", "isic4").
#' @param conceptScheme Character. Local identifier of the scheme (often identical to
#'   \code{prefix}). The function automatically resolves this to the canonical
#'   ConceptScheme URI published in the endpoint.
#' @param language Character. Desired language for labels, scope notes and exclusion notes.
#'   Default: "en".
#' @param level Character. One of:
#'   \itemize{
#'     \item "ALL" (default): return all levels in the hierarchy;
#'     \item a specific depth value (e.g. "2") to filter concepts at that depth only.
#'   }
#' @param showQuery Logical.
#'   \itemize{
#'     \item FALSE (default): returns only the classification table;
#'     \item TRUE: returns a list containing the SPARQL query, the resolved scheme URI, and the table itself.
#'   }
#' @param knownSchemes Optional. A data.frame supplying authoritative mappings of the
#'   form \code{Prefix, ConceptScheme, URI[, Endpoint]}. When provided, this overrides
#'   automatic discovery. To be obtained using \code{classificationList(endpoint)}.
#' @param preferMappingOnly Logical. If TRUE, the function never attempts SPARQL
#'   discovery and uses only information in \code{knownSchemes} or \code{classificationList(endpoint)}.
#'   Default: FALSE.
#'
#' @return
#' If \code{showQuery = FALSE}, a \code{data.frame} with one row per concept and columns:
#' \itemize{
#'   \item \strong{Concept}: full concept URI;
#'   \item \strong{Code}: \code{skos:notation} coerced to string;
#'   \item \strong{Label}: preferred or alternative label in the requested language;
#'   \item \strong{Depth}: \code{xkos:depth} if available;
#'   \item \strong{BroaderList}: broader concept URIs (pipe-separated);
#'   \item \strong{BroaderCodeList}: broader concept notations (pipe-separated);
#'   \item \strong{IncludeNotes}: \code{skos:scopeNote} values (double-pipe-separated);
#'   \item \strong{ExcludeNotes}: \code{xkos:exclusionNote} values (double-pipe-separated).
#' }
#'
#' If \code{showQuery = TRUE}, returns a list with:
#' \itemize{
#'   \item \code{SPARQL.query} – the executed SPARQL string;
#'   \item \code{scheme_uri} – the resolved ConceptScheme URI;
#'   \item \code{ClassificationTable} – the data.frame described above.
#' }
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

retrieveClassificationTable <- function(endpoint,
                                        prefix,
                                        conceptScheme,
                                        language  = "en",
                                        level     = "ALL",
                                        showQuery = FALSE,
                                        knownSchemes = NULL,
                                        preferMappingOnly = FALSE) {
  # --- Endpoint validation & URL resolution (using internal helpers) ----------
  endpoint <- .validate_endpoints(endpoint)   # normalizes and validates
  if (identical(endpoint, "ALL")) {
    stop("'ALL' is not allowed in retrieveClassificationTable(); provide a single endpoint.",
         call. = FALSE)
  }
  endpoint_url <- .sparql_endpoint(endpoint)
  
  # --- Sanitize basic inputs --------------------------------------------------
  prefix        <- trimws(prefix)
  conceptScheme <- trimws(conceptScheme)
  level         <- trimws(level)
  
  # --- Resolve ConceptScheme URI (mapping -> discovery -> CELLAR fallback) ----
  scheme_uri <- .resolve_scheme_uri(
    endpoint_url      = endpoint_url,
    endpoint          = endpoint,
    prefix            = prefix,
    conceptScheme     = conceptScheme,
    language          = language,
    knownSchemes      = knownSchemes,      
    preferMappingOnly = preferMappingOnly  # TRUE = mapping only, no discovery/fallback
  )
  
  if (length(scheme_uri) != 1L || is.na(scheme_uri) || !nzchar(scheme_uri)) {
    stop("Prefix or ConceptScheme not present in this endpoint.", call. = FALSE)
  }
  
  
  # --- Existence check (extra robustness) ------------------------------------
  
  ask_ok <- try(.ask_scheme_exists(endpoint_url, scheme_uri), silent = TRUE)
  
  if (inherits(ask_ok, "try-error")) {
    stop(sprintf("Scheme URI '%s' could not be checked against endpoint '%s'.",
                 scheme_uri, endpoint),
         call. = FALSE)
  }
  
  if (!isTRUE(ask_ok)) {
    
    # If mapping was provided and preferMappingOnly=TRUE -> the mapping entry is invalid
    if (!is.null(knownSchemes) && isTRUE(preferMappingOnly)) {
      stop(sprintf("The URI '%s' from knownSchemes is not valid for endpoint '%s'.",
                   scheme_uri, endpoint),
           call. = FALSE)
    }
    
    stop(paste0(
      "Prefix or ConceptScheme not present in this endpoint: ", endpoint,
      " (resolved URI was: ", scheme_uri, ")"
    ), call. = FALSE)
  }
  
  # --- SPARQL prefixes-------
  prefix_block <- paste(
    "PREFIX skos: <http://www.w3.org/2004/02/skos/core#>",
    "PREFIX xkos: <http://rdf-vocabulary.ddialliance.org/xkos#>",
    "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>",
    sep = "\n"
  )
  
  # --- Optional depth filter -------
  filter_depth_inner <- ""
  if (!identical(toupper(level), "ALL")) {
    filter_depth_inner <- paste0("FILTER (STR(?Depth0) = '", level, "')")
  }
  
  # --- Query (flat select + normalized aggregations)-----
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
    "  # Depth from concept (fallback)\n",
    "  OPTIONAL { ?Concept xkos:depth ?DepthFromConcept }\n",
    "  # Depth via classification level (common case)\n",
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
  
  # --- Execute: POST (CSV) with GET fallback ----
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
  
  # --- Cleanup--
  data[] <- lapply(data, function(x) if (is.character(x)) gsub("\n", " ", x) else x)
  
  # --- Return --
  if (isTRUE(showQuery)) {
    return(list(SPARQL.query = SPARQL.query, scheme_uri = scheme_uri, ClassificationTable = data))
  } else {
    return(data)
  }
}

# Internal helpers -------------------

# Read CSV with row-name sanitization (prevents duplicate colname issues)
#' @keywords internal
#' @noRd
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
    row.names = NULL,
    check.names = FALSE,
    comment.char = ""
  )
}

# ASK if scheme exists on the endpoint
#' @keywords internal
#' @noRd
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

# Lightweight ConceptScheme discovery (SPARQL)
#' @keywords internal
#' @noRd
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
  df  <- .read_sparql_csv(csv)
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
  
  # broader probe on prefix only
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
  df2  <- .read_sparql_csv(csv2)
  if (nrow(df2)) {
    df2$._ends_scheme <- grepl("/scheme/?$", df2$scheme)
    df2 <- df2[order(-as.integer(df2$._ends_scheme), df2$scheme), ]
    return(df2$scheme[1])
  }
  
  NA_character_
}

# Normalize an external mapping table (columns and trimming)
#' @keywords internal
#' @noRd
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

# Get mapping from override or classificationList(); uses internal endpoint helpers
#' @keywords internal
#' @noRd
.get_known_schemes_from_classificationList <- function(endpoint,
                                                       knownSchemes_override = NULL) {
  endpoint <- .validate_endpoints(endpoint)  
  
  # 1) external override (if provided)
  ks <- .normalize_known_schemes(knownSchemes_override) #if NULL, it stays NULL
  if (!is.null(ks)) {
    if ("Endpoint" %in% names(ks)) {
      ks_src <- ks[toupper(ks$Endpoint) == endpoint, setdiff(names(ks), "Endpoint"), drop = FALSE]
      if (nrow(ks_src)) return(ks_src)
      # otherwise: use the whole table
    }
    return(ks)
  }
  
  # 2) fallback: call classificationList(endpoint) (public function)
  if (!exists("classificationList", mode = "function")) return(NULL)
  df <- try(classificationList(endpoint), silent = TRUE)
  if (inherits(df, "try-error") || is.null(df)) return(NULL)
  ks2 <- .normalize_known_schemes(df)
  if (is.null(ks2)) return(NULL)
  ks2[, c("Prefix","ConceptScheme","URI"), drop = FALSE]
}

# Resolver: mapping -> discovery -> (CELLAR pattern fallback), all via internal helpers
#' @keywords internal
#' @noRd
.resolve_scheme_uri <- function(endpoint_url,
                                endpoint,
                                prefix,
                                conceptScheme,
                                language = "en",
                                knownSchemes = NULL,
                                preferMappingOnly = FALSE) {
  endpoint <- .validate_endpoints(endpoint)
  
  # 1) authoritative mapping (override -> classificationList())
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
      
      # NEW: if mapping-only, return the mapped URI AS-IS (ASK is done by caller)
      if (isTRUE(preferMappingOnly)) return(uri)
      # Otherwise (not mapping-only), do a sanity ASK here to avoid extra hop later
      if (isTRUE(.ask_scheme_exists(endpoint_url, uri))) return(uri)
      # If ASK fails and not mapping-only, continue to discovery/fallback below
    } else if (isTRUE(preferMappingOnly)) {
      # Mapping-only and no pair in mapping -> hard NA
      return(NA_character_)
    }
  } else if (isTRUE(preferMappingOnly)) {
    # Mapping-only and no mapping table available at all -> hard NA
    return(NA_character_)
  }
  
  # 2) SPARQL discovery (ONLY if not mapping-only)
  if (!isTRUE(preferMappingOnly)) {
    disc <- try(.discover_scheme_uri(endpoint_url, prefix, conceptScheme, language), silent = TRUE)
    if (!inherits(disc, "try-error") && is.character(disc) && nzchar(disc)) {
      if (.ask_scheme_exists(endpoint_url, disc)) return(disc)
    }
    
    # 3) CELLAR-only fallback pattern (validated by ASK)
    if (identical(endpoint, "CELLAR")) {
      pat <- paste0("http://data.europa.eu/xsp/", tolower(prefix), "/", conceptScheme)
      if (.ask_scheme_exists(endpoint_url, pat)) return(pat)
    }
  }
  
  NA_character_
}
