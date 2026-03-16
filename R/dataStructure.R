#' @title Retrieve a classification structure (levels and concepts) from a supported service
#'
#' @description
#' Queries the selected service (e.g., CELLAR or FAO) to retrieve the structure of a
#' classification scheme (levels, depth, and concept rows), with language-aware
#' label fallback. You can return a compact summary table, a detailed
#' concept-level table, or both.
#'
#' The function first resolves the input classification using the in-package registry
#' returned by \code{classificationList()}, then retrieves the structure and returns
#' it as data frames. Network errors are handled with retry logic.
#'
#' @param endpoint Character scalar. Accepted values are \code{"CELLAR"} or \code{"FAO"}.
#' @param prefix Character. A classification prefix used in the registry
#'   (e.g. \code{"NACE"}, \code{"CPA"}, \code{"CN2022"}). Used to resolve the scheme.
#' @param conceptScheme Character or \code{NULL}. Optional alternative identifier
#'   for the concept scheme. The resolver tries both \code{prefix} and \code{conceptScheme}
#'   using basic normalization (case-insensitive; dot removal).
#' @param language Character scalar. Two-letter language code used to select
#'   labels (default: \code{"en"}). Fallback chain: requested language -> English (\code{"en"})
#'   -> no-language literal -> the local resource name.
#' @param showQuery Logical. If \code{TRUE}, the generated queries are emitted via
#'   \code{message()} and also included in the returned list.
#' @param return One of \code{c("summary", "details", "both")}. Controls which table(s)
#'   are fetched and returned.
#' @param timeout_sec Numeric. HTTP timeout in seconds. Default: \code{60}.
#' @param retries Integer. Number of automatic retries on transient HTTP errors.
#'   Default: \code{3}.
#'
#' @details
#' \strong{Resolution of the scheme}
#'
#' The function calls \code{classificationList(endpoint)} and tries to match either
#' \code{prefix} or \code{conceptScheme} using normalized keys. On success it extracts
#' the classification namespace and scheme identifier needed to compose requests.
#'
#' \strong{Language fallback}
#'
#' Labels for both the classification levels and concepts follow a multi-step
#' fallback: requested \code{language} -> \code{"en"} → no-language literal → local name.
#'
#' @return
#' A \code{data.frame} (for \code{"summary"} or \code{"details"}), or a
#' \code{list(summary, details)} when \code{return = "both"}.
#'
#' \strong{Columns:}
#' \itemize{
#'   \item All returned tables include a leading \code{Prefix} column (lower-case token).
#'   \item \code{summary}: \code{Concept_Scheme}, \code{Depth}, \code{Level}, \code{Count}.
#'   \item \code{details}: \code{Concept}, \code{Code}, \code{Label}, \code{Depth}, \code{Level},
#'         \code{BroaderList}, \code{BroaderCodeList}.
#' }
#'
#' If \code{showQuery = TRUE}, the return value is a list containing:
#' \itemize{
#'   \item \code{resolved}: metadata (\code{endpoint}, inputs, \code{scheme_id}, \code{ns_uri}, and optionally \code{title})
#'   \item the query text (one or two elements depending on \code{return})
#'   \item the retrieved table(s)
#' }
#'
#' @section Errors and Warnings:
#' \itemize{
#'   \item Errors if \code{endpoint} is not \code{"CELLAR"} or \code{"FAO"}, or if the
#'         scheme cannot be resolved from the registry.
#'   \item Warns (and still returns rows) if no explicit levels are declared.
#'   \item Propagates HTTP errors from the service after retry policy.
#' }
#'
#' @seealso \code{\link{classificationList}}
#'
#' @examples
#' \dontrun{
#' ds_cn <- dataStructure(
#'   endpoint      = "CELLAR",
#'   prefix        = "cn2022",
#'   conceptScheme = "cn2022",
#'   language      = "en",
#'   return        = "summary"
#' )
#' head(ds_cn)
#' }
#'
#' @importFrom httr RETRY stop_for_status content accept add_headers user_agent timeout
#' @importFrom utils read.csv head
#' @export
dataStructure <- function(endpoint, prefix, conceptScheme = NULL,
                                language = "en",
                                showQuery = FALSE,
                                return = c("summary", "details", "both"),
                                timeout_sec = 60, retries = 3) {
  
  return <- match.arg(return)
  
  # ---- Input validation -----------------------------------------------------
  ep <- .validate_endpoints(endpoint)
  if (identical(ep, "ALL") || length(ep) != 1L) {
    stop("'endpoint' must be a single value ('CELLAR' or 'FAO'), not 'ALL' or a vector.", call. = FALSE)
  }
  endpoint <- ep
  
  if (!is.character(language) || length(language) != 1L || !grepl("^[a-z]{2}$", language)) {
    stop("'language' must be a two-letter ISO 639-1 code (e.g., 'en', 'fr', 'de').", call. = FALSE)
  }
  
  # ---- Helpers (local) ------------------------------------------------------
  normalize_key <- function(x) tolower(gsub("[^A-Za-z0-9._-]", "", x))
  
  get_registry_table <- function(endpoint) {
    ep  <- toupper(endpoint)
    lst <- classificationList(ep)
    if (is.data.frame(lst)) return(lst)
    if (is.list(lst) && !is.null(lst[[ep]]) && is.data.frame(lst[[ep]])) return(lst[[ep]])
    stop(sprintf("No classification registry available for endpoint '%s'.", endpoint), call. = FALSE)
  }
  
  resolve_scheme <- function(endpoint, key1, key2 = NULL) {
    tbl <- get_registry_table(endpoint)
    if (!is.data.frame(tbl) || !nrow(tbl)) {
      stop(sprintf("No classification registry available for endpoint '%s'.", endpoint), call. = FALSE)
    }
    # Expected columns: Prefix, ConceptScheme, URI, (optional) Title
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
    
    hit <- which(tbl$ConceptScheme %in% keys_raw);                      if (length(hit)) return(pick(hit[1]))
    hit <- which(tbl$ConceptScheme_clean %in% keys_clean);              if (length(hit)) return(pick(hit[1]))
    hit <- which(tbl$Prefix_clean %in% keys_clean);                     if (length(hit)) return(pick(hit[1]))
    hit <- which(tbl$ConceptScheme_clean_nodot %in% keys_clean_nodot);  if (length(hit)) return(pick(hit[1]))
    hit <- which(tbl$Prefix_clean_nodot %in% keys_clean_nodot);         if (length(hit)) return(pick(hit[1]))
    
    suggestions <- utils::head(paste0(tbl$Prefix, " (", tbl$ConceptScheme, ")"), 10)
    stop(
      paste0(
        "Could not resolve the requested classification '", paste(keys_raw, collapse = "', '"),
        "' for endpoint '", endpoint, "'.\nTry one of:\n  - ",
        paste(suggestions, collapse = "\n  - ")
      ),
      call. = FALSE
    )
  }
  
  warn_if_no_levels <- function(df) {
    if (is.null(df) || !is.data.frame(df) || nrow(df) == 0L) {
      warning(
        "The classification appears to have no declared levels; returning available rows only. ",
        "If you rely on explicit levels, consider using `level = 'ALL'` in retrieveClassificationTable().",
        call. = FALSE,
        immediate. = TRUE
      )
      return(TRUE)
    }
    FALSE
  }
  
  coerce_types_summary <- function(df) {
    if (!nrow(df)) return(df)
    if ("Depth" %in% names(df)) df$Depth <- suppressWarnings(as.integer(df$Depth))
    if ("Count" %in% names(df)) df$Count <- suppressWarnings(as.integer(df$Count))
    df
  }
  
  coerce_types_details <- function(df) {
    if (!nrow(df)) return(df)
    if ("Depth" %in% names(df)) df$Depth <- suppressWarnings(as.integer(df$Depth))
    df
  }
  
  # NEW: add Prefix as first column (length-safe; preserves empty frames)
  add_prefix_col <- function(df, token) {
    if (!is.data.frame(df)) return(df)
    if (!nrow(df)) {
      df$Prefix <- character(0)
      df <- df[, c("Prefix", setdiff(names(df), "Prefix")), drop = FALSE]
      return(df)
    }
    cbind(Prefix = rep(token, nrow(df)), df)
  }
  
  # ---- Resolve endpoint URL and scheme -------------------------------------
  source_url <- .sparql_endpoint(endpoint)
  
  resolved   <- resolve_scheme(endpoint, prefix, conceptScheme)
  scheme_id  <- resolved$scheme_id
  ns_uri     <- resolved$ns_uri
  title_val  <- if (!is.null(resolved$row$Title)) resolved$row$Title else NA_character_
  
  # NEW: printable prefix token (from registry if available; fallback to input)
  prefix_token <- if (!is.null(resolved$row$Prefix)) as.character(resolved$row$Prefix) else prefix
  prefix_token <- tolower(trimws(prefix_token))
  
  # Build PREFIX block with fixed alias 'cs:' for the classification namespace
  prefix_block <- paste(
    "PREFIX skos: <http://www.w3.org/2004/02/skos/core#>",
    "PREFIX xkos: <http://rdf-vocabulary.ddialliance.org/xkos#>",
    "PREFIX rdf:  <http://www.w3.org/1999/02/22-rdf-syntax-ns#>",
    sprintf("PREFIX cs:   <%s>", ns_uri),
    sep = "\n"
  )
  
  # -------------------------------
  # 1) SUMMARY query (uses cs:<scheme_id>)
  # -------------------------------
  q_summary <- paste0(
    prefix_block, "
SELECT DISTINCT ?Concept_Scheme ?Depth ?Level (COUNT (DISTINCT ?s) AS ?Count)
WHERE {
  ?s skos:prefLabel ?Label ;
     skos:inScheme cs:", scheme_id, " ;
     skos:inScheme ?Scheme ;
     ^skos:member ?Member ;
     skos:notation ?notation .

  ?Member a xkos:ClassificationLevel .
  OPTIONAL { ?Member xkos:levels ?levels_temp . }
  OPTIONAL { ?Member xkos:depth  ?Depth . }

  OPTIONAL { ?Member skos:prefLabel ?LevelLabel_req . FILTER (lang(?LevelLabel_req) = '", language, "') }
  OPTIONAL { ?Member skos:prefLabel ?LevelLabel_en  . FILTER (lang(?LevelLabel_en)  = 'en') }
  OPTIONAL { ?Member skos:prefLabel ?LevelLabel_nolang . FILTER (lang(?LevelLabel_nolang) = '') }

  FILTER (?Scheme = cs:", scheme_id, ")
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
  # 2) DETAILS query (uses cs:<scheme_id>)
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
           skos:inScheme cs:", scheme_id, " ;
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
  
  # ---- HTTP runner (wrap ONLY the network call in tryCatch) -----------------
  run_q <- function(query) {
    if (isTRUE(showQuery)) message(query)
    resp <- tryCatch(
      httr::RETRY(
        verb   = "POST",
        url    = source_url,
        body   = list(query = query, format = "text/csv"),
        encode = "form",
        httr::accept("text/csv"),
        httr::add_headers(`Accept-Encoding` = "gzip"),
        httr::user_agent("correspondenceTables (R)"),
        httr::timeout(timeout_sec),
        times  = retries,
        pause_base = 1, pause_cap = 8,
        terminate_on = c(400, 401, 403, 404)
      ),
      error = function(e) {
        stop(sprintf("Network error while contacting the service: %s", conditionMessage(e)), call. = FALSE)
      }
    )
    httr::stop_for_status(resp)
    txt <- httr::content(resp, as = "text", encoding = "UTF-8")
    
    if (!nzchar(txt)) {
      # Service returned an empty payload instead of a CSV table
      return(data.frame())
    }
    
    df <- try(utils::read.csv(text = txt, check.names = FALSE, stringsAsFactors = FALSE), silent = TRUE)
    if (inherits(df, "try-error")) {
      stop("The service returned a non-parsable response instead of a CSV table.", call. = FALSE)
    }
    df
  }
  
  # ---- Fetch according to 'return' -----------------------------------------
  if (return == "summary") {
    tab <- coerce_types_summary(run_q(q_summary))
    warn_if_no_levels(tab)
    
    if (nrow(tab) && "Depth" %in% names(tab)) {
      ord <- do.call(order, c(list(tab$Depth), if ("Level" %in% names(tab)) list(tab$Level) else list(), list(na.last = TRUE)))
      tab <- tab[ord, , drop = FALSE]
    }
    
    # NEW: add Prefix column
    tab <- add_prefix_col(tab, prefix_token)
    
    if (isTRUE(showQuery)) {
      return(list(
        resolved      = list(endpoint = endpoint, input_prefix = prefix, input_conceptScheme = conceptScheme,
                             scheme_id = scheme_id, ns_uri = ns_uri, title = title_val),
        query.summary = q_summary,
        dataStructure = tab
      ))
    }
    return(tab)
  }
  
  if (return == "details") {
    det <- coerce_types_details(run_q(q_details))
    
    if (is.null(det) || !is.data.frame(det) || nrow(det) == 0L ||
        (!"Level" %in% names(det)) || all(is.na(det$Level))) {
      warning(
        "The classification appears to have no declared levels; returning available rows only. ",
        "If you rely on explicit levels, consider using `level = 'ALL'` in retrieveClassificationTable().",
        call. = FALSE, immediate. = TRUE
      )
    }
    
    # NEW: add Prefix column
    det <- add_prefix_col(det, prefix_token)
    
    if (isTRUE(showQuery)) {
      return(list(
        resolved     = list(endpoint = endpoint, input_prefix = prefix, input_conceptScheme = conceptScheme,
                            scheme_id = scheme_id, ns_uri = ns_uri, title = title_val),
        query.details = q_details,
        details       = det
      ))
    }
    return(det)
  }
  
  # return == "both"
  tab <- coerce_types_summary(run_q(q_summary))
  warn_if_no_levels(tab)
  if (nrow(tab) && "Depth" %in% names(tab)) {
    ord <- do.call(order, c(list(tab$Depth), if ("Level" %in% names(tab)) list(tab$Level) else list(), list(na.last = TRUE)))
    tab <- tab[ord, , drop = FALSE]
  }
  det <- coerce_types_details(run_q(q_details))
  
  # NEW: add Prefix column to both
  tab <- add_prefix_col(tab, prefix_token)
  det <- add_prefix_col(det, prefix_token)
  
  if (isTRUE(showQuery)) {
    return(list(
      resolved          = list(endpoint = endpoint, input_prefix = prefix, input_conceptScheme = conceptScheme,
                               scheme_id = scheme_id, ns_uri = ns_uri, title = title_val),
      query.summary     = q_summary,
      query.details     = q_details,
      summary           = tab,
      details           = det
    ))
  }
  list(summary = tab, details = det)
}



