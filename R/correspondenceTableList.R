#' @title List available correspondence tables from online services
#'
#' @description
#' Returns a data frame listing available correspondence tables (mappings between
#' statistical classifications) published by one or more supported online services.
#'
#' Use this function to discover valid `prefix` and `ID_table` values before
#' calling `retrieveCorrespondenceTable()`.
#'
#' @param endpoint Character. Which service(s) to query.
#'   One or more of the supported endpoints (e.g. "CELLAR", "FAO"),
#'   or "ALL" to query all available services. Case-insensitive.
#'   If multiple endpoints are supplied (including "ALL"), a **named list**
#'   is returned; otherwise a single `data.frame`.
#' @param showQuery Logical. If `TRUE`, the SPARQL queries used are emitted via
#'   `message()` (suppress with `suppressMessages()`).
#'
#' @details
#' Read-only queries; no side effects (no file writes or caching).
#'
#' @return
#' If a single endpoint is requested, a `data.frame` (one row per correspondence).
#' If multiple endpoints are requested (including "ALL"), a **named list**
#' of data frames, one per endpoint.
#'
#' @seealso retrieveCorrespondenceTable()
#' 
#' @examples
#' \dontrun{
#' # List correspondence tables available in CELLAR
#' ct <- correspondenceTableList(endpoint = "CELLAR")
#' head(ct)
#'
#' # List correspondence tables available in FAO
#' ct_fao <- correspondenceTableList(endpoint = "FAO")
#' head(ct_fao)
#'
#' # Get correspondence tables from both repositories
#' ct_all <- correspondenceTableList(endpoint = "ALL")
#' names(ct_all)
#' }
#' 
#' 
#' @export
correspondenceTableList <- function(endpoint = "ALL", showQuery = FALSE) {
  
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("Package 'httr' is required. Please install.packages('httr').", call. = FALSE)
  }
  
  # ---- Centralized endpoint handling ----
  eps <- .expand_endpoints(endpoint)  # validates + expands "ALL" + preserves vectors
  
  # ---- Worker for a single endpoint (kept local; you can externalize later) ----
  run_one <- function(ep) {
    
    endpoint_url <- .sparql_endpoint(ep)
    
    # 1) Discover classification schemes (best effort)
    schemes <- tryCatch(
      classificationList(endpoint = ep),
      error = function(e) {
        message("classificationList() failed for endpoint '", ep, "': ",
                conditionMessage(e), ". Returning empty result.")
        return(NULL)
      }
    )
    
    # Extract scheme URIs if available (used for non-FAO endpoints to filter noise)
    scheme_uri <- character(0)
    if (is.data.frame(schemes) && nrow(schemes)) {
      candidate_uri_cols <- c("SchemeURI", "ConceptSchemeURI", "URI", "SchemeUri", "ConceptSchemeUri")
      uri_cols_idx <- which(tolower(names(schemes)) %in% tolower(candidate_uri_cols))
      if (length(uri_cols_idx)) {
        scheme_uri <- unique(unlist(schemes[uri_cols_idx], use.names = FALSE))
        scheme_uri <- as.character(scheme_uri)
        scheme_uri <- scheme_uri[!is.na(scheme_uri) & grepl("^https?://", scheme_uri)]
      }
    }
    
    # 2) Build SPARQL query
    prefix_header <- paste(
      "PREFIX skos: <http://www.w3.org/2004/02/skos/core#>",
      "PREFIX xkos: <http://rdf-vocabulary.ddialliance.org/xkos#>",
      sep = "\n"
    )
    
    values_block <- if (length(scheme_uri)) {
      paste0(
        "VALUES ?KnownScheme { ",
        paste(sprintf("<%s>", scheme_uri), collapse = " "),
        " }"
      )
    } else {
      ""
    }
    
    SPARQL.query <- if (toupper(ep) == "FAO") {
      # FAO: list correspondences directly (src/tgt schemes often not exposed here)
      paste0(
        prefix_header, "\n",
        "SELECT DISTINCT ?corr ?Label\n",
        "WHERE {\n",
        "  ?corr a xkos:Correspondence .\n",
        "  OPTIONAL { ?corr skos:prefLabel ?Label . }\n",
        "}\n"
      )
    } else {
      # CELLAR/others: optionally bind known schemes; try to infer src/tgt schemes
      paste0(
        prefix_header, "\n",
        "SELECT DISTINCT ?corr ?Label ?srcScheme ?tgtScheme\n",
        "WHERE {\n",
        if (nzchar(values_block)) paste0(values_block, "\n") else "",
        "  ?corr a xkos:Correspondence .\n",
        "  OPTIONAL { ?corr skos:prefLabel ?Label . }\n",
        "  OPTIONAL {\n",
        "    ?assoc xkos:inCorrespondence ?corr .\n",
        "    OPTIONAL { ?assoc xkos:sourceConcept ?src . ?src skos:inScheme ?srcScheme . }\n",
        "    OPTIONAL { ?assoc xkos:targetConcept ?tgt . ?tgt skos:inScheme ?tgtScheme . }\n",
        "  }\n",
        "}\n"
      )
    }
    
    if (isTRUE(showQuery)) {
      message("SPARQL query (", ep, "):\n", SPARQL.query)
    }
    
    # 3) Execute (read-only)
    csv_txt <- tryCatch(
      .do_post_csv(endpoint_url, SPARQL.query, timeout_sec = 60L, attempts = 2L),
      error = function(e) {
        message("SPARQL query failed for '", ep, "': ", conditionMessage(e),
                ". Returning empty result.")
        return("")
      }
    )
    if (!nzchar(csv_txt)) {
      return(data.frame(
        Prefix                = character(),
        ID                    = character(),
        Source.Classification = character(),
        Target.Classification = character(),
        Table.Name            = character(),
        URI                   = character(),
        stringsAsFactors      = FALSE
      ))
    }
    
    df <- tryCatch(
      utils::read.csv(text = csv_txt, stringsAsFactors = FALSE, check.names = FALSE),
      error = function(e) data.frame()
    )
    if (!is.data.frame(df) || !nrow(df)) {
      return(data.frame(
        Prefix                = character(),
        ID                    = character(),
        Source.Classification = character(),
        Target.Classification = character(),
        Table.Name            = character(),
        URI                   = character(),
        stringsAsFactors      = FALSE
      ))
    }
    
    # 4) Normalize output column names (robust to absent columns)
    df_nms_lc <- tolower(names(df))
    pick <- function(needle) {
      i <- match(needle, df_nms_lc)
      if (is.na(i)) NULL else names(df)[i]
    }
    col_corr  <- pick("corr")
    col_label <- pick("label")
    col_src   <- pick("srcscheme")
    col_tgt   <- pick("tgtscheme")
    
    n <- nrow(df)
    corr_vec  <- if (!is.null(col_corr))  as.character(df[[col_corr]])  else character(n)
    label_vec <- if (!is.null(col_label)) as.character(df[[col_label]]) else character(n)
    src_vec   <- if (!is.null(col_src))   as.character(df[[col_src]])   else character(n)
    tgt_vec   <- if (!is.null(col_tgt))   as.character(df[[col_tgt]])   else character(n)
    
    out <- data.frame(
      ID                    = if (length(corr_vec)) sub(".*/", "", corr_vec) else character(n),
      Source.Classification = src_vec,
      Target.Classification = tgt_vec,
      Table.Name            = label_vec,
      URI                   = corr_vec,
      stringsAsFactors      = FALSE
    )
    
    # 5) Compute Prefix (best effort) by joining with 'schemes' if available
    out$Prefix <- ""
    
    if (is.data.frame(schemes) && nrow(schemes)) {
      schemes_nms_lc   <- tolower(names(schemes))
      cand_prefix_cols <- c("prefix", "catalogprefix", "token", "namespaceprefix")
      prefix_col_idx   <- which(schemes_nms_lc %in% cand_prefix_cols)
      prefix_col       <- if (length(prefix_col_idx)) names(schemes)[prefix_col_idx[1]] else NULL
      
      candidate_uri_cols <- c("SchemeURI", "ConceptSchemeURI", "URI", "SchemeUri", "ConceptSchemeUri")
      scheme_uri_col_idx <- which(tolower(names(schemes)) %in% tolower(candidate_uri_cols))
      scheme_uri_col     <- if (length(scheme_uri_col_idx)) names(schemes)[scheme_uri_col_idx[1]] else NULL
      
      if (!is.null(prefix_col) && !is.null(scheme_uri_col)) {
        map_df <- unique(schemes[, c(scheme_uri_col, prefix_col), drop = FALSE])
        names(map_df) <- c("SchemeURI", "SchemePrefix")
        map_df <- map_df[!duplicated(map_df$SchemeURI), , drop = FALSE]
        
        src_df <- merge(
          data.frame(SchemeURI = out$Source.Classification, stringsAsFactors = FALSE),
          map_df, by = "SchemeURI", all.x = TRUE
        )
        tgt_df <- merge(
          data.frame(SchemeURI = out$Target.Classification, stringsAsFactors = FALSE),
          map_df, by = "SchemeURI", all.x = TRUE
        )
        if (!nrow(src_df)) src_df <- data.frame(SchemePrefix = rep(NA_character_, nrow(out)))
        if (!nrow(tgt_df)) tgt_df <- data.frame(SchemePrefix = rep(NA_character_, nrow(out)))
        
        choose_pref <- function(a, b) {
          a <- ifelse(is.na(a), "", as.character(a))
          b <- ifelse(is.na(b), "", as.character(b))
          if (nzchar(a) && nzchar(b)) {
            if (identical(a, b)) a else ""
          } else if (nzchar(a)) {
            a
          } else if (nzchar(b)) {
            b
          } else {
            ""
          }
        }
        out$Prefix <- mapply(choose_pref, src_df$SchemePrefix, tgt_df$SchemePrefix, USE.NAMES = FALSE)
      }
    }
    
    # 6) Fallbacks: derive Prefix from URI, then from ID
    if (any(!nzchar(out$Prefix) & nzchar(out$URI))) {
      uri_to_token <- function(u) {
        u <- as.character(u); if (!nzchar(u)) return("")
        u <- sub("/+$", "", u)
        m <- regmatches(u, regexec("^https?://[^/]+/(.+)$", u))[[1]]
        if (length(m) >= 2) {
          parts <- strsplit(m[2], "/", fixed = TRUE)[[1]]
          if (length(parts) >= 2) {
            token <- parts[length(parts) - 1]
            token <- gsub("[^A-Za-z0-9_]", "", token)
            tolower(token)
          } else ""
        } else ""
      }
      idx <- which(!nzchar(out$Prefix) & nzchar(out$URI))
      if (length(idx)) out$Prefix[idx] <- vapply(out$URI[idx], uri_to_token, character(1))
    }
    
    if (any(!nzchar(out$Prefix) & nzchar(out$ID))) {
      id_to_token <- function(id) {
        id <- as.character(id); if (!nzchar(id)) return("")
        m <- regexpr("[-_]", id)
        token <- if (m > 0) substr(id, 1, m - 1) else id
        token <- gsub("[^A-Za-z0-9_]", "", token)
        tolower(token)
      }
      idx <- which(!nzchar(out$Prefix) & nzchar(out$ID))
      if (length(idx)) out$Prefix[idx] <- vapply(out$ID[idx], id_to_token, character(1))
    }
    
    # Reorder & deduplicate
    out <- out[, c("Prefix", "ID", "Source.Classification", "Target.Classification", "Table.Name", "URI")]
    out <- out[!duplicated(out), , drop = FALSE]
    out
  }
  
  # ---- Single vs multiple endpoints return type ----
  if (length(eps) > 1L) {
    res <- setNames(vector("list", length(eps)), eps)
    for (i in seq_along(eps)) res[[i]] <- run_one(eps[[i]])
    return(res)
  } else {
    return(run_one(eps[[1]]))
  }
}

