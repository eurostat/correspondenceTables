#' @title Download a correspondence (mapping) table between two classifications
#'
#' @description
#' Downloads a correspondence table, a mapping of codes, from one statistical
#' classification (A) to another (B) from an online service (CELLAR or
#' FAO), and returns it as a plain data frame. The function has no side effects
#' (it does not read or write files).
#'
#' A correspondence table tells you which codes in classification A relate
#' to which codes in classification B (e.g., \code{NACE2} → \code{CPA21}).
#'
#' @param endpoint Character. The online service to query. Case-insensitive.
#'   Supported values are those returned by the internal endpoint registry
#'   (e.g., \code{"CELLAR"}, \code{"FAO"}).
#' @param prefix Character. Catalogue prefix where the correspondence is
#'   published (e.g., \code{"nace2"}, \code{"cpa21"}, \code{"cn2022"}).
#'   Use \code{\link{correspondenceTableList}()} to discover valid values.
#' @param ID_table Character. Identifier of the correspondence, typically of
#'   the form \code{"A_B"} such as \code{"NACE2_CPA21"} or \code{"CN2022_NACE2"}.
#'   Discover identifiers via \code{\link{correspondenceTableList}()}.
#' @param language Character. Preferred label language as a BCP‑47 code.
#'   Defaults to \code{"en"} (English). Examples: \code{"fr"}, \code{"de"}.
#' @param showQuery Logical. If \code{TRUE}, returns a list with the SPARQL
#'   query and the result data frame; otherwise (default) returns just the data frame.
#'
#' @details
#' \strong{When to use this.} Use \code{retrieveCorrespondenceTable()} when you
#' know (or have discovered) the \code{prefix} and \code{ID_table} of a
#' correspondence and want the mapping rows as a standard data frame.
#'
#' \strong{How it works (short).}
#' \enumerate{
#'   \item Validates the selected service (\code{endpoint}).
#'   \item Optionally resolves a stable anchor via \code{\link{correspondenceTableList}()}.
#'   \item Builds a SPARQL query returning source/target codes, labels, notes, and link URI.
#'   \item Issues an HTTP request and parses the response into a data frame.
#' }
#'
#'
#' @return
#' A \code{data.frame} where each row represents one mapping from the source
#' classification (\strong{A}) to the target classification (\strong{B}).
#'
#' Columns (when available):
#' \itemize{
#'   \item \code{CorrespondenceID}:  Requested identifier (e.g., \code{"NACE2_CPA21"}).
#'   \item \code{Prefix}: Catalogue prefix (constant per table).
#'   \item \code{A}: Source code (\code{A} parsed from \code{ID_table}).
#'   \item \code{B}: Target code (\code{B} parsed from \code{ID_table}).
#'   \item \code{Label_A}, \code{Label_B}: Human‑readable labels in \code{language}.
#'   \item \code{Include_A}, \code{Exclude_A}, \code{Include_B}, \code{Exclude_B}: Notes, if available.
#'   \item \code{Comment}: Free‑text comment on the mapping, if any.
#'   \item \code{URL}: URI of the mapping association.
#' }
#'
#' Attributes attached for traceability:
#' \itemize{
#'   \item \code{endpoint}, \code{endpoint_url}, \code{prefix}, \code{ID_table}
#'   \item \code{A}, \code{B}, \code{language}, \code{normalized_codes}, \code{anchor_mode}, \code{anchor}
#' }
#'
#' @section Notes:
#' \itemize{
#'   \item The function focuses on retrieval. Any code normalization is left to
#'         downstream steps.
#' }
#'
#' @examples
#' \dontrun{
#' # 1) Discover available correspondences at CELLAR
#' ct <- correspondenceTableList(endpoint = "CELLAR")
#' subset(ct, grepl("NACE2", ID) & grepl("CPA21", ID))
#'
#' # 2) Retrieve one correspondence (English labels)
#' x <- retrieveCorrespondenceTable(
#'   endpoint = "CELLAR",
#'   prefix   = "nace2",
#'   ID_table = "NACE2_CPA21",
#'   language = "en"
#' )
#' head(x)
#'
#' # 3) Save to CSV explicitly if needed (no automatic writing)
#' # utils::write.csv(x, "NACE2_CPA21.csv", row.names = FALSE)
#'
#' # 4) Inspect the generated SPARQL query
#' dbg <- retrieveCorrespondenceTable(
#'   endpoint = "FAO",
#'   prefix   = "cpa21",
#'   ID_table = "NACE2_CPA21",
#'   showQuery = TRUE
#' )
#' message(substr(dbg$SPARQL.query, 1, 400), "...")
#' }
#'
#' @importFrom httr GET content stop_for_status timeout accept user_agent
#' @export
retrieveCorrespondenceTable <- function(
    endpoint,
    prefix,
    ID_table,
    language  = "en",
    showQuery = FALSE
) {
  # -------- internal defaults (no user-visible side effects) --------
  precheck <- TRUE  # ENABLE correspondence discovery
  normalize_codes <- FALSE
  norm_case       <- "lower"
  norm_strip      <- "[[:space:].-]"
  timeout_sec <- 120L   
  max_retries     <- 1L
  
  # ------------------------------------------------------------------
  # Define empty result skeleton (used when no correspondence is found)
  # ------------------------------------------------------------------
  empty_result_df <- data.frame(
    CorrespondenceID = character(0),
    Prefix           = character(0),
    stringsAsFactors = FALSE,
    check.names      = FALSE
  )
  
  # ---- endpoint validation & URL (via internal helpers) ----
  endpoint <- .validate_endpoints(endpoint)
  if (identical(endpoint, "ALL")) {
    stop("'endpoint' must be a single endpoint (e.g., 'CELLAR' or 'FAO'), not 'ALL'.",
         call. = FALSE)
  }
  if (length(endpoint) != 1L) {
    stop("'endpoint' must be a single value.", call. = FALSE)
  }
  endpoint_url <- .sparql_endpoint(endpoint)
  
  # ---- sanitize inputs ----
  prefix   <- trimws(prefix)
  ID_table <- trimws(ID_table)
  language <- trimws(language)
  
  # ---- derive A and B tokens from ID_table (robust to spacing and hyphens) ----
  ID_table_std <- gsub("-", "_", ID_table, fixed = TRUE)
  ID_table_std <- gsub("__", "_", ID_table_std, fixed = TRUE)
  A_raw <- sub("_.*", "", ID_table_std)
  B_raw <- sub(".*_", "", ID_table_std)
  
  # ---- make SPARQL variable names safe ----
  .safe_var <- function(x) {
    x <- gsub("[^A-Za-z0-9_]", "_", x)
    if (!grepl("^[A-Za-z_]", x)) x <- paste0("X_", x)
    x
  }
  A <- .safe_var(A_raw)
  B <- .safe_var(B_raw)
  
  # ---- optional precheck to get a stable correspondence URI ----
  
  corr_uri <- NULL
  if (isTRUE(precheck)) {
    ct_list <- tryCatch(
      correspondenceTableList(endpoint),
      error = function(e) NULL
    )
    
    if (is.data.frame(ct_list) && nrow(ct_list)) {
      ct_df <- ct_list
      
      # Normalize column names from correspondenceTableList(): Prefix, ID, URI -> prefix, id, uri
      names(ct_df) <- tolower(gsub("\\s+", ".", names(ct_df)))  # [1](https://teams.microsoft.com/l/meeting/details?eventId=AAMkADU5YjkwZDczLWU5NTEtNGZkMC05MjUwLTg1ZmE0ZmFkZGM5YwBGAAAAAAAY52HbrUuXSYgNfQribIjRBwCJfZVPsYxqQJf_ENZMnpx2AAAAAAENAACJfZVPsYxqQJf_ENZMnpx2AAAA8uebAAA%3d)
      
      # Harmonize URI column name from correspondenceTableList()
      # (often returned as URL, not URI)
      if ("url" %in% names(ct_df) && !"uri" %in% names(ct_df)) {
        ct_df$uri <- ct_df$url
      }
      
      # correspondenceTableList() provides ID (-> 'id' after tolower); harmonize to id_table
      if ("id" %in% names(ct_df) && !"id_table" %in% names(ct_df)) {
        ct_df$id_table <- ct_df$id
      }
      
      # 1) Primary match: prefix + id_table (case-insensitive)
      hit <- integer(0)
      if (all(c("prefix", "id_table") %in% names(ct_df))) {
        hit <- which(
          tolower(trimws(ct_df$prefix))   == tolower(trimws(prefix)) &
            tolower(trimws(ct_df$id_table)) == tolower(trimws(ID_table))
        )
      }
      
      # 2) If not found, try by URI tail: .../<ID_table>
      if (!length(hit) && "uri" %in% names(ct_df)) {
        uri_tail <- function(u) {
          u <- as.character(u)
          u <- sub("/+$", "", u)
          sub(".*/", "", u)
        }
        tails <- vapply(ct_df$uri, uri_tail, character(1))
        cand  <- which(tolower(trimws(tails)) == tolower(trimws(ID_table)))
        
        if (length(cand)) {
          if ("prefix" %in% names(ct_df)) {
            rel <- which(tolower(trimws(ct_df$prefix[cand])) == tolower(trimws(prefix)))
            hit <- if (length(rel)) cand[rel[1]] else cand[1]
          } else {
            hit <- cand[1]
          }
        }
      }
      
      # 3) Take the URI if we found a match
      if (length(hit) && "uri" %in% names(ct_df)) {
        u <- ct_df$uri[hit[1]]
        if (is.character(u) && nzchar(trimws(u))) corr_uri <- trimws(u)
      }
    }
  }
  
  
  # ------------------------------------------------------------------
  # Derive scheme URI (namespace) from correspondence URI
  # ------------------------------------------------------------------
  if (is.null(corr_uri) || !nzchar(corr_uri)) {
    
    # If discovery is disabled, do NOT call correspondenceTableList()
    if (!isTRUE(precheck)) {
      warning(
        sprintf("No correspondence URI resolved for ID '%s' in %s (discovery disabled).",
                ID_table, endpoint),
        call. = FALSE
      )
      if (isTRUE(showQuery)) {
        return(list(SPARQL.query = NA_character_, CorrespondenceTable = empty_result_df))
      }
      return(empty_result_df)
    }
    
    # discovery-enabled: suggest close matches
    ct_list <- tryCatch(correspondenceTableList(endpoint), error = function(e) NULL)
    suggestion <- NULL
    if (is.data.frame(ct_list) && nrow(ct_list)) {
      names(ct_list) <- tolower(names(ct_list))
      if ("id" %in% names(ct_list)) {
        cand <- ct_list$id[
          grepl(A_raw, ct_list$id, ignore.case = TRUE) |
            grepl(B_raw, ct_list$id, ignore.case = TRUE) |
            grepl(ID_table, ct_list$id, ignore.case = TRUE)
        ]
        cand <- unique(cand)
        if (length(cand)) suggestion <- head(cand, 10)
      }
    }
    
    msg <- sprintf("No correspondence table with ID '%s' found in %s.", ID_table, endpoint)
    if (!is.null(suggestion)) msg <- paste0(msg, " Closest IDs: ", paste(suggestion, collapse = ", "))
    stop(msg, call. = FALSE)
    
    if (isTRUE(showQuery)) {
      return(list(SPARQL.query = NA_character_, CorrespondenceTable = empty_result_df))
    }
    return(empty_result_df)
  }
  
  # MPORTANT: corr_uri exists -> now define scheme_uri
  # Example corr_uri:  http://data.europa.eu/xsp/cn2018/CN2018_CPA21
  # scheme_uri becomes: http://data.europa.eu/xsp/cn2018/
  
  scheme_uri <- sub("/[^/]+$", "/", trimws(corr_uri))
  
  # ------------------------------------------------------------------
  # Build PREFIX block (REAL SPARQL, not HTML-escaped)
  # ------------------------------------------------------------------
  stopifnot(nzchar(prefix))
  stopifnot(nzchar(scheme_uri))
  
  prefix_block <- paste(
    "PREFIX dc:   <http://purl.org/dc/elements/1.1/>",
    "PREFIX dct:  <http://purl.org/dc/terms/>",
    "PREFIX rdf:  <http://www.w3.org/1999/02/22-rdf-syntax-ns#>",
    "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>",
    "PREFIX skos: <http://www.w3.org/2004/02/skos/core#>",
    "PREFIX xkos: <http://rdf-vocabulary.ddialliance.org/xkos#>",
    "PREFIX xsd:  <http://www.w3.org/2001/XMLSchema#>",
    sprintf("PREFIX %s: <%s>", prefix, scheme_uri),
    sep = "\n"
  )
  
  # ---- anchor mode: use full URI if available; else identifier ----
  if (!is.null(corr_uri) && nzchar(trimws(corr_uri))) {
    anchor_mode  <- "uri"
    anchor_info  <- paste0("<", trimws(corr_uri), ">")
    ct_bind_line <- paste0("  BIND (", anchor_info, " AS ?CT)\n")
    
    
  } else {
    anchor_mode  <- "identifier"
    anchor_info  <- ID_table
    ct_bind_line <- paste0('  ?CT dct:identifier "', ID_table, "\" .\n")
  }
  
  
  # ---- build SPARQL query (PAGED, lightweight) ----
  build_query_paged <- function(lang, limit, offset) {
    paste0(
      prefix_block, "\n",
      "SELECT ?SourceCode ?TargetCode ?Comment ?URL\n",
      "WHERE {\n",
      ct_bind_line,
      "  ?CT xkos:madeOf ?Associations .\n",
      "  ?Associations xkos:sourceConcept ?Source .\n",
      "  OPTIONAL { ?Associations xkos:targetConcept ?Target . }\n",
      "  OPTIONAL { ?Associations rdfs:comment ?Comment . }\n",
      "  ?Source skos:notation ?SourceNotation .\n",
      "  OPTIONAL { ?Target skos:notation ?TargetNotation . }\n",
      "  BIND (STR(?SourceNotation) AS ?SourceCode)\n",
      "  BIND (IF(BOUND(?TargetNotation), STR(?TargetNotation), '') AS ?TargetCode)\n",
      "  BIND (STR(?Associations) AS ?URL)\n",
      "}\n",
      "ORDER BY ?SourceCode\n",
      "LIMIT ", as.integer(limit), "\n",
      "OFFSET ", as.integer(offset), "\n"
    )
  }
  
  # ------------------------------------------------------------------
  # Helpers to detect non-CSV responses
  # ------------------------------------------------------------------
  is_html <- function(txt) {
    is.character(txt) && length(txt) == 1L &&
      nzchar(txt) &&
      grepl("^\\s*<!DOCTYPE\\s+html|<html", txt, ignore.case = TRUE)
  }
  is_xml <- function(txt) {
    is.character(txt) && length(txt) == 1L &&
      nzchar(txt) &&
      grepl("^\\s*<\\?xml|<sparql", txt, ignore.case = TRUE)
  }
  
  
  # ---- initialise paging state ----
  parts      <- list()
  offset     <- 0L
  chunk_size <- 5000L
  
  # ---- run paged queries ----
  repeat {
    SPARQL.query <- build_query_paged(language, chunk_size, offset)
    
    csv_text <- tryCatch(
      .do_post_csv(endpoint_url, SPARQL.query,
                   timeout_sec = timeout_sec,
                   attempts    = max_retries),
      error = function(e) ""
    )
    
    if (!nzchar(csv_text)) break
    if (isTRUE(is_html(csv_text)) || isTRUE(is_xml(csv_text))) break
    
    df_chunk <- .read_sparql_csv(csv_text)
    if (!nrow(df_chunk)) break
    
    parts[[length(parts) + 1L]] <- df_chunk
    
    if (nrow(df_chunk) < chunk_size) break
    offset <- offset + chunk_size
  }
  
  # ---- assemble result ----
  data <- if (length(parts)) do.call(rbind, parts) else data.frame()
  
  # ---- rename columns for downstream logic ----
  if (nrow(data)) {
    data[[A]] <- data[["SourceCode"]]
    data[[B]] <- data[["TargetCode"]]
  }
  
  if (!is.data.frame(data) || !nrow(data)) {
    warning(sprintf("No correspondence rows returned for ID '%s' in %s.", ID_table, endpoint),
            call. = FALSE)
    if (isTRUE(showQuery)) {
      return(list(SPARQL.query = SPARQL.query, CorrespondenceTable = empty_result_df))
    }
    return(empty_result_df)
  }
  
  # ---- optional normalization (off by default) ----
  ct_norm_code <- function(x, strip = "[[:space:].-]", case = c("lower","upper","asis")) {
    case <- match.arg(case)
    x <- as.character(x)
    x <- trimws(x)
    if (!is.null(strip) && nzchar(strip)) x <- gsub(strip, "", x)
    if (case == "lower") x <- tolower(x) else if (case == "upper") x <- toupper(x)
    x
  }
  if (isTRUE(normalize_codes) && is.data.frame(data) && nrow(data)) {
    if (nzchar(A) && A %in% names(data)) data[[A]] <- ct_norm_code(data[[A]], strip = norm_strip, case = norm_case)
    if (nzchar(B) && B %in% names(data)) data[[B]] <- ct_norm_code(data[[B]], strip = norm_strip, case = norm_case)
  }
  
  # ---- prepend CorrespondenceID and Prefix; attach attributes ----
  if (is.data.frame(data) && nrow(data) >= 0) {
    data <- cbind(
      CorrespondenceID = rep(ID_table, nrow(data)),
      Prefix           = rep(prefix,   nrow(data)),
      data
    )
  } 
  
  attr(data, "endpoint")         <- endpoint
  attr(data, "prefix")           <- prefix
  attr(data, "ID_table")         <- ID_table
  attr(data, "A")                <- A
  attr(data, "B")                <- B
  attr(data, "language")         <- language
  attr(data, "normalized_codes") <- isTRUE(normalize_codes)
  attr(data, "endpoint_url")     <- endpoint_url
  attr(data, "anchor_mode")      <- anchor_mode
  attr(data, "anchor")           <- anchor_info
  attr(data, "corr_uri")         <- corr_uri
  attr(data, "scheme_uri")       <- scheme_uri
  
  if (isTRUE(showQuery)) {
    return(list(SPARQL.query = SPARQL.query, CorrespondenceTable = data))
  } else {
    return(data)
  }
}
