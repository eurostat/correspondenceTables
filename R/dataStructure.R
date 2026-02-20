#' Retrieve a classification data structure from CELLAR/FAO via SPARQL
#'
#' @title Data structure (levels and depth) for a SKOS/XKOS classification
#'
#' @description
#' Queries a SPARQL endpoint (CELLAR or FAO) to retrieve the structure of a
#' SKOS/XKOS classification scheme (levels, depth, and concepts), with
#' language-aware label fallback. You can return a compact **summary** table,
#' a detailed **concept-level** table, or **both**.
#'
#' The function first resolves the input classification using a local registry
#' returned by [classificationList()], then builds SPARQL queries and fetches
#' the results as CSV. An **offline** mode is supported for vignette/demo
#' use via `options(useLocalDataForVignettes = TRUE)`.
#'
#' @param endpoint Character scalar. Accepted values are `"CELLAR"` or `"FAO"`.
#'   `"ALL"` is not queryable and will error. Matching is case-insensitive.
#' @param prefix Character. A classification prefix used in the registry
#'   (e.g. `"NACE"`, `"CPA"`, `"HS"`, `"AGROVOC"`). Used to resolve the scheme.
#' @param conceptScheme Character or `NULL`. Optional alternative identifier
#'   for the concept scheme. The resolver tries both `prefix` and `conceptScheme`
#'   with multiple normalization steps (case-insensitive, dot removal, etc.).
#' @param language Character scalar. ISO 639-1 language code used to select
#'   labels (default: `"en"`). Fallback chain: requested language → English (`en`)
#'   → no-language literal → local name.
#' @param showQuery Logical. If `TRUE`, returns a list with the SPARQL query
#'   (or queries) and the resolved metadata instead of just the data frame(s).
#'   The query text is also printed to the console.
#' @param return One of `c("summary", "details", "both")`. Controls which table(s)
#'   are fetched and returned.
#'
#' @details
#' **Resolution of the scheme**
#'
#' - The function calls an in-package registry via `classificationList(endpoint)`
#'   and tries to match either `prefix` or `conceptScheme` using a set of
#'   normalized keys. On success it extracts the namespace `ns_uri` and the
#'   concept scheme ID.
#'
#' **Offline mode**
#'
#' - If `options(useLocalDataForVignettes) == TRUE`, the function looks for a
#'   CSV under `inst/extdata/` named `dataStructure_<scheme>.csv` and returns
#'   it (summary only). This is useful for vignettes or examples without network.
#'
#' **Online mode**
#'
#' - The function downloads a small JSON configuration from the project’s
#'   repository to select the correct SPARQL endpoint URL for the given
#'   `endpoint`, then executes one or two SPARQL queries via `httr::POST`
#'   requesting CSV output.
#'
#' **Label and level fallback**
#'
#' - Labels for both the classification levels and concepts follow a multi-step
#'   fallback: requested `language` → `"en"` → no-language literal → local name
#'   extracted from the resource IRI.
#'
#' **Columns / Shapes**
#'
#' - When `return = "summary"`, the result is a `data.frame` with:
#'   - `Concept_Scheme` (character): scheme local name
#'   - `Depth` (integer or character): depth value from XKOS
#'   - `Level` (character): resolved level label
#'   - `Count` (integer): count of concepts at that level/depth
#'
#' - When `return = "details"`, the result is a `data.frame` with:
#'   - `Concept` (IRI as character)
#'   - `Code` (notation as character)
#'   - `Label` (resolved pref/alt label)
#'   - `Depth` (integer or character)
#'   - `Level` (resolved level label)
#'   - `BroaderList` (pipe-separated IRIs of broader concepts)
#'   - `BroaderCodeList` (pipe-separated notations of broader concepts)
#'
#' - When `return = "both"`, a named list with `summary` and `details`.
#'
#' - If `showQuery = TRUE`, the function returns a list containing:
#'   - `resolved`: metadata (`endpoint`, inputs, `scheme_id`, `ns_prefix`,
#'     `ns_uri`, and `title` if available)
#'   - the SPARQL query (or queries)
#'   - the retrieved table(s)
#'
#' @return
#' A `data.frame` (for `"summary"` or `"details"`), or a `list(summary, details)`
#' when `return = "both"`. If `showQuery = TRUE`, returns a list containing the
#' query text, resolved metadata, and the fetched table(s).
#'
#' @section Errors and Warnings:
#' - Will error if `endpoint` is not one of `"CELLAR"` or `"FAO"`, or if the
#'   scheme cannot be resolved from the registry.
#' - Will warn and return only a summary table in offline mode.
#' - Will propagate HTTP errors from the SPARQL endpoint.
#'
#' @seealso [classificationList()]
#'
#' @examples
#' \dontrun{
#' # -------------------------------------------------------------------
#' # Online examples (require network and a valid registry entry)
#' # Classification: CN 2022 (CELLAR), English labels
#' # -------------------------------------------------------------------
#'
#' # 1) CN (English) — Summary only
#' ds_cn <- dataStructure(
#'   endpoint      = "CELLAR",
#'   prefix        = "cn2022",
#'   conceptScheme = "cn2022",
#'   language      = "en",
#'   return        = "summary"
#' )
#' print(ds_cn)
#'
#' # 2) CN (English) — Details only (one row per concept)
#' det_cn <- dataStructure(
#'   endpoint      = "CELLAR",
#'   prefix        = "cn2022",
#'   conceptScheme = "cn2022",
#'   language      = "en",
#'   return        = "details"
#' )
#' head(det_cn)
#'
#' # 3) CN (English) — Summary and Details together
#' both_cn <- dataStructure(
#'   endpoint      = "CELLAR",
#'   prefix        = "cn2022",
#'   conceptScheme = "cn2022",
#'   language      = "en",
#'   return        = "both"
#' )
#' names(both_cn)                 # Expect: "summary" "details"
#' print(both_cn$summary)
#' head(both_cn$details)
#'
#' # 4) CN (English) — Split Details by Depth (one data.frame per level)
#' det_cn_levels <- split(det_cn, as.character(det_cn$Depth))
#' names(det_cn_levels)           # e.g. "1", "2", "3", "4", "5"
#' print(det_cn_levels)
#' }
#'
#' # -------------------------------------------------------------------
#' # Offline demo (if the package ships the CSV files):
#' # -------------------------------------------------------------------
#' \dontshow{
#' old <- getOption("useLocalDataForVignettes"); on.exit(options(useLocalDataForVignettes = old))
#' }
#' options(useLocalDataForVignettes = TRUE)
#' # Example: #' CSV under `inst/extdata/` named `dataStructure_<scheme>.csv`
#' #' # Same API, but data are read from inst/extdata/ instead of SPARQL
#' # dataStructure("CELLAR", "NACE", "NACERev2", return = "summary")#' 
#' options(useLocalDataForVignettes = FALSE)
#'
#' @importFrom httr POST stop_for_status content accept
#' @importFrom jsonlite fromJSON
#' @importFrom utils read.csv head
#' @export

dataStructure <- function(endpoint, prefix, conceptScheme,
                          language = "en",
                          showQuery = FALSE,
                          return = c("summary", "details", "both")) {
  
  return <- match.arg(return)
  
  # --- helpers (local to this function) ------------------------------------
  normalize_key <- function(x) tolower(gsub("[^A-Za-z0-9._-]", "", x))
  
  get_registry_table <- function(endpoint) {
    ep  <- toupper(endpoint)
    lst <- classificationList(ep)
    if (is.data.frame(lst)) return(lst)
    if (is.list(lst) && !is.null(lst[[ep]]) && is.data.frame(lst[[ep]])) return(lst[[ep]])
    lst_all <- classificationList("ALL")
    if (is.list(lst_all) && !is.null(lst_all[[ep]]) && is.data.frame(lst_all[[ep]])) return(lst_all[[ep]])
    stop(simpleError(sprintf("No classification registry available for endpoint '%s'.", endpoint)))
  }
  
  resolve_scheme <- function(endpoint, key1, key2 = NULL) {
    tbl <- get_registry_table(endpoint)
    if (!is.data.frame(tbl) || nrow(tbl) == 0) {
      stop(simpleError(sprintf("No classification registry available for endpoint '%s'.", endpoint)))
    }
    tbl$Prefix_clean              <- normalize_key(tbl$Prefix)
    tbl$ConceptScheme_clean       <- normalize_key(tbl$ConceptScheme)
    tbl$Prefix_clean_nodot        <- gsub("\\.", "", tbl$Prefix_clean)
    tbl$ConceptScheme_clean_nodot <- gsub("\\.", "", tbl$ConceptScheme_clean)
    
    keys_raw         <- unique(na.omit(c(key1, key2)))
    keys_clean       <- normalize_key(keys_raw)
    keys_clean_nodot <- gsub("\\.", "", keys_clean)
    
    pick <- function(ix) {
      row <- tbl[ix, , drop = FALSE]
      uri <- row$URI
      ns_uri    <- sub("/[^/]+$", "/", uri)
      scheme_id <- row$ConceptScheme
      list(scheme_id = scheme_id, ns_uri = ns_uri, row = row)
    }
    
    hit <- which(tbl$ConceptScheme %in% keys_raw);              if (length(hit)) return(pick(hit[1]))
    hit <- which(tbl$ConceptScheme_clean %in% keys_clean);      if (length(hit)) return(pick(hit[1]))
    hit <- which(tbl$Prefix_clean %in% keys_clean);             if (length(hit)) return(pick(hit[1]))
    hit <- which(tbl$ConceptScheme_clean_nodot %in% keys_clean_nodot); if (length(hit)) return(pick(hit[1]))
    hit <- which(tbl$Prefix_clean_nodot %in% keys_clean_nodot); if (length(hit)) return(pick(hit[1]))
    
    suggestions <- utils::head(paste0(tbl$Prefix, " (", tbl$ConceptScheme, ")"), 10)
    stop(simpleError(
      paste0(
        "Could not resolve input '", paste(keys_raw, collapse = "', '"),
        "' for endpoint '", endpoint, "'.\nTry one of:\n  - ",
        paste(suggestions, collapse = "\n  - ")
      )
    ))
  }
  
  # HTML unescape helper (robust; always yields real < and >)
  unescape_html <- function(x) {
    x <- gsub("&amp;amp;amp;amp;amp;lt;",  "&amp;amp;amp;amp;lt;", x, fixed = TRUE)
    x <- gsub("&amp;amp;amp;amp;amp;gt;",  "&amp;amp;amp;amp;gt;", x, fixed = TRUE)
    x <- gsub("&amp;amp;amp;amp;amp;amp;lt;",  "&amp;amp;amp;amp;lt;", x, fixed = TRUE)
    x <- gsub("&amp;amp;amp;amp;amp;amp;gt;",  "&amp;amp;amp;amp;gt;", x, fixed = TRUE)
    x <- gsub("&amp;amp;amp;amp;amp;amp;amp;lt;",  "&amp;amp;amp;amp;lt;", x, fixed = TRUE)
    x <- gsub("&amp;amp;amp;amp;amp;amp;amp;gt;",  "&amp;amp;amp;amp;gt;", x, fixed = TRUE)
    x <- gsub("&amp;amp;amp;amp;amp;amp;amp;amp;lt;",  "&amp;amp;amp;amp;lt;", x, fixed = TRUE)
    x <- gsub("&amp;amp;amp;amp;amp;amp;amp;amp;gt;",  "&amp;amp;amp;amp;gt;", x, fixed = TRUE)
    x <- gsub("&amp;amp;amp;amp;lt;", "&amp;amp;lt;", x, fixed = TRUE)
    x <- gsub("&amp;amp;amp;amp;gt;", "&amp;amp;gt;", x, fixed = TRUE)
    x
  }
  

  emit_no_level_warning <- function(df) {
    if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
      warning(
        "This classification has no level. Please use level = 'ALL' when retrieving it using the retrieveClassificationTable",
        call. = FALSE,
        immediate. = TRUE
      )
      return(TRUE)
    }
    FALSE
  }
  # --- end helpers ----------------------------------------------------------
  
  endpoint <- toupper(endpoint)
  if (!(endpoint %in% c("ALL", "FAO", "CELLAR"))) {
    stop(simpleError(paste("The endpoint value:", endpoint, "is not accepted")))
  }
  if (endpoint == "ALL") {
    stop(simpleError("Endpoint 'ALL' cannot be queried. Use 'CELLAR' or 'FAO'."))
  }
  
  # --- Local/offline mode ---------------------------------------------------
  if (getOption("useLocalDataForVignettes", FALSE)) {
    scheme_id <- tryCatch(
      resolve_scheme(endpoint, prefix, conceptScheme)$scheme_id,
      error = function(e) NA_character_
    )
    candidates <- unique(na.omit(c(scheme_id, prefix)))
    for (k in candidates) {
      localDataPath <- system.file("extdata", paste0("dataStructure_", k, ".csv"),
                                   package = "correspondenceTables")
      if (file.exists(localDataPath)) {
        data <- utils::read.csv(localDataPath, check.names = FALSE)
        if (showQuery) message("Data loaded from local file: ", basename(localDataPath))
        if (return == "summary") return(data)
        warning("Offline mode: only summary table available. Returning summary.")
        return(data)
      }
    }
    stop(simpleError("Local data not found for the requested classification in offline mode."))
  }
  
  # --- Online mode ----------------------------------------------------------
  config_url <- "https://raw.githubusercontent.com/eurostat/correspondenceTables/main/inst/extdata/endpoint_source_config.json"
  config <- jsonlite::fromJSON(config_url)
  source <- switch(endpoint,
                   "CELLAR" = config$CELLAR,
                   "FAO"    = config$FAO)
  
  resolved  <- resolve_scheme(endpoint, prefix, conceptScheme)
  scheme_id <- resolved$scheme_id
  ns_prefix <- scheme_id
  ns_uri    <- resolved$ns_uri
  
  # Build SPARQL PREFIX block (real <...> IRIs)
  core_prefixes <- paste(
    sprintf("PREFIX skos: <%s>", "http://www.w3.org/2004/02/skos/core#"),
    sprintf("PREFIX xkos: <%s>", "http://rdf-vocabulary.ddialliance.org/xkos#"),
    sprintf("PREFIX rdf:  <%s>", "http://www.w3.org/1999/02/22-rdf-syntax-ns#"),
    sep = "\n"
  )
  class_prefix <- sprintf("PREFIX %s: <%s>", ns_prefix, ns_uri)
  prefix_block <- paste(core_prefixes, class_prefix, sep = "\n")
  
  # -------------------------------
  # 1) SUMMARY query
  # -------------------------------
  q_summary <- paste0(
    prefix_block, "
SELECT DISTINCT ?Concept_Scheme ?Depth ?Level (COUNT (DISTINCT ?s) AS ?Count)
WHERE {
  ?s skos:prefLabel ?Label ;
     skos:inScheme ", ns_prefix, ":", scheme_id, " ;
     skos:inScheme ?Scheme ;
     ^skos:member ?Member ;
     skos:notation ?notation .

  ?Member a xkos:ClassificationLevel .
  OPTIONAL { ?Member xkos:levels ?levels_temp . }
  OPTIONAL { ?Member xkos:depth  ?Depth . }

  OPTIONAL {
    ?Member skos:prefLabel ?LevelLabel_req .
    FILTER (lang(?LevelLabel_req) = '", language, "')
  }
  OPTIONAL {
    ?Member skos:prefLabel ?LevelLabel_en .
    FILTER (lang(?LevelLabel_en) = 'en')
  }
  OPTIONAL {
    ?Member skos:prefLabel ?LevelLabel_nolang .
    FILTER (lang(?LevelLabel_nolang) = '')
  }

  FILTER (?Scheme = ", ns_prefix, ":", scheme_id, ")
  FILTER (lang(?Label) = '", language, "')

  BIND('", ns_uri, "' AS ?CLASS_URL)

  BIND (STRAFTER(STR(?Scheme), ?CLASS_URL) AS ?Concept_Scheme)
  BIND (STRAFTER(STR(?Member), ?CLASS_URL) AS ?LevelLocal)

  BIND (COALESCE(?LevelLabel_req, ?LevelLabel_en, ?LevelLabel_nolang, ?LevelLocal) AS ?LevelRaw)
  BIND (STR(?LevelRaw) AS ?Level)

  FILTER (STRLEN(?Concept_Scheme) != 0)
  FILTER (STRLEN(?Level) != 0)
}
GROUP BY ?Concept_Scheme ?Depth ?Level
ORDER BY ?Concept_Scheme ?Depth ?Level
")
  
  # -------------------------------
  # 2) DETAILS query
  # -------------------------------
  q_details <- paste0(
    prefix_block, "
SELECT ?Concept
       (MIN(STR(?Notation)) AS ?Code)
       (SAMPLE(?Label0)     AS ?Label)
       (SAMPLE(?Depth0)     AS ?Depth)
       (SAMPLE(?LevelTxt)   AS ?Level)
       (GROUP_CONCAT(DISTINCT STR(?Broader);         separator=' | ')  AS ?BroaderList)
       (GROUP_CONCAT(DISTINCT STR(?BroaderNotation); separator=' | ')  AS ?BroaderCodeList)
WHERE {
  ?Concept a skos:Concept ;
           skos:inScheme ", ns_prefix, ":", scheme_id, " ;
           skos:notation ?Notation .

  OPTIONAL { ?Concept xkos:depth ?DepthFromConcept }
  OPTIONAL {
    ?Concept ^skos:member ?Member .
    ?Member a xkos:ClassificationLevel .
    OPTIONAL { ?Member xkos:depth ?DepthFromLevel }

    OPTIONAL { ?Member skos:prefLabel ?Lvl_req FILTER (LANG(?Lvl_req) = '", language, "') }
    OPTIONAL { ?Member skos:prefLabel ?Lvl_en  FILTER (LANG(?Lvl_en)  = 'en') }
    OPTIONAL { ?Member skos:prefLabel ?Lvl_no  FILTER (LANG(?Lvl_no)  = '') }
    BIND (COALESCE(?Lvl_req, ?Lvl_en, ?Lvl_no, STRAFTER(STR(?Member), '", ns_uri, "')) AS ?LevelTxt)
  }
  BIND (COALESCE(?DepthFromLevel, ?DepthFromConcept) AS ?Depth0)

  OPTIONAL { ?Concept skos:prefLabel ?Pref FILTER (LANG(?Pref) = '", language, "') }
  OPTIONAL { ?Concept skos:altLabel  ?Alt  FILTER (LANG(?Alt)  = '", language, "') }
  BIND (COALESCE(?Pref, ?Alt) AS ?Label0)

  OPTIONAL { ?Concept skos:broader ?Broader . ?Broader skos:notation ?BroaderNotation }
}
GROUP BY ?Concept
ORDER BY ?Code
")
  
  # --- Function to run a query and return a data.frame 
  run_q <- function(query) {
    if (grepl("&amp;amp;amp;amp;lt;|&amp;amp;amp;amp;gt;", query, perl = TRUE)) {
      query <- unescape_html(query)
    }
    query <- gsub("&amp;amp;amp;amp;lt;", "&amp;amp;lt;", query, fixed = TRUE)
    query <- gsub("&amp;amp;amp;amp;gt;", "&amp;amp;gt;", query, fixed = TRUE)
    
    resp <- httr::POST(
      url    = source,
      body   = list(query = query, format = "text/csv"),
      encode = "form",
      httr::accept("text/csv")
    )
    httr::stop_for_status(resp)
    
    txt <- httr::content(resp, "text")
    if (!nzchar(txt)) {
      return(data.frame())
    }
    utils::read.csv(text = txt, sep = ",", check.names = FALSE, stringsAsFactors = FALSE)
  }
  
 
  if (return == "summary") {
    tab <- run_q(q_summary)
    
    
    if (emit_no_level_warning(tab)) {
      if (showQuery) {
        out <- list(
          resolved = list(
            endpoint             = endpoint,
            input_prefix         = prefix,
            input_conceptScheme  = conceptScheme,
            scheme_id            = scheme_id,
            ns_prefix            = ns_prefix,
            ns_uri               = ns_uri,
            title                = if (!is.null(resolved$row$Title)) resolved$row$Title else NA_character_
          ),
          SPARQL.query  = q_summary,
          dataStructure = tab
        )
        cat(out$SPARQL.query, sep = "\n")
        return(out)
      }
      return(tab)  # restituisci comunque data.frame (potenzialmente vuoto)
    }
    
    if (nrow(tab)) tab <- tab[order(tab[, 3], decreasing = FALSE), ]
    if (showQuery) {
      out <- list(
        resolved = list(
          endpoint             = endpoint,
          input_prefix         = prefix,
          input_conceptScheme  = conceptScheme,
          scheme_id            = scheme_id,
          ns_prefix            = ns_prefix,
          ns_uri               = ns_uri,
          title                = if (!is.null(resolved$row$Title)) resolved$row$Title else NA_character_
        ),
        SPARQL.query  = q_summary,
        dataStructure = tab
      )
      cat(out$SPARQL.query, sep = "\n")
      return(out)
    }
    return(tab)
  }
  
  
  if (return == "details") {
    det <- run_q(q_details)
    
    
    if (is.null(det) || !is.data.frame(det) || nrow(det) == 0 ||
        (!"Level" %in% names(det)) || all(is.na(det$Level))) {
      warning(
        "This classification has no level. Please use level = 'ALL' when retrieving it using the retrieveClassificationTable",
        call. = FALSE,
        immediate. = TRUE
      )
    }
    
    if (showQuery) {
      out <- list(
        resolved = list(
          endpoint             = endpoint,
          input_prefix         = prefix,
          input_conceptScheme  = conceptScheme,
          scheme_id            = scheme_id,
          ns_prefix            = ns_prefix,
          ns_uri               = ns_uri,
          title                = if (!is.null(resolved$row$Title)) resolved$row$Title else NA_character_
        ),
        SPARQL.query  = q_details,
        details       = det
      )
      cat(out$SPARQL.query, sep = "\n")
      return(out)
    }
    return(det)
  }
  

  tab <- run_q(q_summary)
  

  emit_no_level_warning(tab)
  
  if (nrow(tab)) tab <- tab[order(tab[, 3], decreasing = FALSE), ]
  det <- run_q(q_details)
  
  if (showQuery) {
    out <- list(
      resolved = list(
        endpoint             = endpoint,
        input_prefix         = prefix,
        input_conceptScheme  = conceptScheme,
        scheme_id            = scheme_id,
        ns_prefix            = ns_prefix,
        ns_uri               = ns_uri,
        title                = if (!is.null(resolved$row$Title)) resolved$row$Title else NA_character_
      ),
      SPARQL.query.summary = q_summary,
      SPARQL.query.details = q_details,
      summary              = tab,
      details              = det
    )
    cat(out$SPARQL.query.summary, sep = "\n")
    cat("\n---\n")
    cat(out$SPARQL.query.details, sep = "\n")
    return(out)
  }
  list(summary = tab, details = det)
}