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
#' @param CSVout Logical or character. \strong{Deprecated} and ignored.
#'   Retained only for backward compatibility. If you need a CSV, call
#'   \code{utils::write.csv()} on the returned data.
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
#' The function does not write to disk. If you need a file, export the returned
#' data frame explicitly (e.g., \code{utils::write.csv(x, file)}).
#'
#' @return
#' A \code{data.frame} where each row represents one mapping from the source
#' classification (\strong{A}) to the target classification (\strong{B}).
#'
#' Columns (when available):
#' \itemize{
#'   \item \code{CorrespondenceID} — Requested identifier (e.g., \code{"NACE2_CPA21"}).
#'   \item \code{Prefix} — Catalogue prefix (constant per table).
#'   \item \code{&lt;A&gt;} — Source code (\code{A} parsed from \code{ID_table}).
#'   \item \code{&lt;B&gt;} — Target code (\code{B} parsed from \code{ID_table}).
#'   \item \code{Label_&lt;A&gt;}, \code{Label_&lt;B&gt;} — Human‑readable labels in \code{language}.
#'   \item \code{Include_&lt;A&gt;}, \code{Exclude_&lt;A&gt;}, \code{Include_&lt;B&gt;}, \code{Exclude_&lt;B&gt;} — Notes, if present.
#'   \item \code{Comment} — Free‑text comment on the mapping, if any.
#'   \item \code{URL} — URI of the mapping association.
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
#'   \item \code{CSVout} is deprecated and ignored (no side effects).
#'   \item The function focuses on retrieval. Any code normalization is left to
#'         downstream steps (unless enabled internally by package options).
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
    CSVout    = FALSE,   # deprecated: ignored
    showQuery = FALSE
) {
  # -------- internal defaults (no user-visible side effects) --------
  precheck        <- TRUE    # try to resolve a stable anchor via correspondenceTableList()
  normalize_codes <- FALSE   # retrieval-only; report via attribute
  norm_case       <- "lower" # if normalization is enabled later
  norm_strip      <- "[[:space:].-]"
  timeout_sec     <- 60L
  max_retries     <- 2L
  
  # ---- deprecation notice for CSVout (no automatic I/O) ----
  if (!identical(CSVout, FALSE)) {
    warning(
      "Argument 'CSVout' is deprecated and ignored. ",
      "This function does not write files. Use utils::write.csv() on the returned data.",
      call. = FALSE
    )
  }
  
  # ---- endpoint validation & URL (via internal helpers) ----
  endpoint  <- .validate_endpoints(endpoint)
  if (identical(endpoint, "ALL")) {
    stop("'endpoint' must be a single endpoint (e.g., 'CELLAR' or 'FAO'), not 'ALL'.",
         call. = FALSE)
  }
  if (length(endpoint) != 1L) {
    stop("'endpoint' must be a single value.", call. = FALSE)
  }
  endpoint_url <- .sparql_endpoint(endpoint)
  
  # ---- derive A and B tokens from ID_table (robust to spacing and hyphens) ----
  ID_table_std <- gsub("-", "_", trimws(ID_table), fixed = TRUE)
  ID_table_std <- gsub("__", "_", ID_table_std, fixed = TRUE)
  A <- sub("_.*", "", ID_table_std)
  B <- sub(".*_", "", ID_table_std)
  
  # ---- minimal local helpers for prefix handling ----
  .clean_prefix_token <- function(x) {
    x <- gsub("[:\\s]+$", "", x)          # drop trailing colon/spaces
    x <- gsub("[^A-Za-z0-9_]", "", x)     # keep letters/digits/underscore
    x
  }
  .prefix_declared_in_block <- function(token, prefix_block) {
    if (!nzchar(token) || !nzchar(prefix_block)) return(FALSE)
    pattern <- paste0("(?mi)^\\s*PREFIX\\s+", token, ":\\s*<[^>]+>")
    grepl(pattern, prefix_block, perl = TRUE)
  }
  
  # ---- optional precheck to get a stable correspondence URI ----
  corr_uri <- NULL
  if (isTRUE(precheck)) {
    ct_list <- tryCatch(
      correspondenceTableList(endpoint),
      error = function(e) NULL
    )
    if (is.data.frame(ct_list) && nrow(ct_list)) {
      ct_df <- ct_list
      
      # Normalize column names to be robust to case/spacing
      names(ct_df) <- tolower(gsub("\\s+", ".", names(ct_df)))
      
      # Harmonize ID column if only `id` is present
      if ("id" %in% names(ct_df) && !"id_table" %in% names(ct_df)) {
        ct_df$id_table <- ct_df$id
      }
      
      # 1) Primary match: by prefix + id_table (exact)
      hit <- integer(0)
      if (all(c("prefix", "id_table") %in% names(ct_df))) {
        hit <- which(
          tolower(trimws(ct_df$prefix)) == tolower(trimws(prefix)) &
            trimws(ct_df$id_table)      == trimws(ID_table)
        )
      }
      
      # 2) If not found, try by URI tail: .../<ID_table>
      if (!length(hit) && "uri" %in% names(ct_df)) {
        uri_tail <- function(u) {
          u <- as.character(u)
          u <- sub("/+$", "", u)      # strip trailing slashes
          sub(".*/", "", u)           # last path segment
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
        if (is.character(u) && nzchar(trimws(u))) {
          corr_uri <- trimws(u)
        }
      }
    }
    
    # 4) Provider-specific deterministic fallback (FAO CPC 2.1 ag)
    if (is.null(corr_uri) &&
        identical(toupper(endpoint), "FAO") &&
        identical(tolower(prefix), "cpc21ag") &&
        nzchar(ID_table)) {
      corr_uri <- paste0(
        "https://stats.fao.org/classifications/CPC/v2.1/ag/",
        ID_table
      )
    }
  }
  
  # ---- build PREFIX block: core + discovered prefixes (best-effort) ----
  core <- c(
    "PREFIX dc:   <http://purl.org/dc/elements/1.1/>",
    "PREFIX dct:  <http://purl.org/dc/terms/>",
    "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>",
    "PREFIX rdf:  <http://www.w3.org/1999/02/22-rdf-syntax-ns#>",
    "PREFIX skos: <http://www.w3.org/2004/02/skos/core#>",
    "PREFIX xkos: <http://rdf-vocabulary.ddialliance.org/xkos#>",
    "PREFIX xsd:  <http://www.w3.org/2001/XMLSchema#>"
  )
  prefix_vec   <- unique(c(.clean_prefix_token(A), .clean_prefix_token(B), .clean_prefix_token(prefix)))
  discovered   <- tryCatch({
    pl <- prefixList(endpoint, prefix = prefix_vec)
    paste(as.character(pl), collapse = "\n")
  }, error = function(e) "")
  prefix_block <- paste(c(core, discovered), collapse = "\n")
  
  # ---- decide anchor mode: prefer full URI; otherwise bind by dct:identifier ----
  anchor_mode  <- "identifier"   # "uri" or "identifier"
  anchor_info  <- NULL           # for metadata / attribute
  ct_bind_line <- NULL           # SPARQL line(s) that bind ?CT
  
  if (!is.null(corr_uri) && nzchar(trimws(corr_uri))) {
    anchor_mode  <- "uri"
    anchor_info  <- paste0("<", trimws(corr_uri), ">")
    ct_bind_line <- paste0("  BIND (", anchor_info, " AS ?CT) \n")
  } else {
    anchor_mode  <- "identifier"
    anchor_info  <- paste0("dct:identifier=", ID_table)
    ct_bind_line <- paste0('  ?CT dct:identifier "', ID_table, '" .\n')
  }
  
  # ---- build SPARQL using ?CT anchor (no QName dependence) ----
  build_query <- function(langA, langB) {
    paste0(
      prefix_block, "\n",
      "SELECT ?", A, " ?", B, " ?Label_", A, " ?Label_", B,
      " ?Include_", A, " ?Exclude_", A,
      " ?Include_", B, " ?Exclude_", B,
      " ?Comment ?URL\n",
      "WHERE {\n",
      ct_bind_line,
      "  ?CT xkos:madeOf ?Associations .\n",
      "  ?Associations xkos:sourceConcept ?Source .\n",
      "  OPTIONAL { ?Associations xkos:targetConcept ?Target . }\n",
      "  OPTIONAL { ?Associations rdfs:comment ?Comment . }\n",
      "  ?Source skos:notation ?SourceNotation .\n",
      "  OPTIONAL { ?Source skos:prefLabel ?PrefA FILTER (LANG(?PrefA) = '", langA, "') }\n",
      "  OPTIONAL { ?Source skos:altLabel  ?AltA  FILTER (LANG(?AltA)  = '", langA, "') }\n",
      "  OPTIONAL { ?Source skos:scopeNote     ?Include_", A, " FILTER (LANG(?Include_", A, ") = '", langA, "') }\n",
      "  OPTIONAL { ?Source xkos:exclusionNote ?Exclude_", A, " FILTER (LANG(?Exclude_", A, ") = '", langA, "') }\n",
      "  BIND (COALESCE(?AltA, ?PrefA) AS ?Label_", A, ")\n",
      "  OPTIONAL {\n",
      "    ?Target skos:notation ?TargetNotation .\n",
      "    OPTIONAL { ?Target skos:prefLabel ?PrefB FILTER (LANG(?PrefB) = '", langB, "') }\n",
      "    OPTIONAL { ?Target skos:altLabel  ?AltB  FILTER (LANG(?AltB)  = '", langB, "') }\n",
      "    OPTIONAL { ?Target skos:scopeNote     ?Include_", B, " FILTER (LANG(?Include_", B, ") = '", langB, "') }\n",
      "    OPTIONAL { ?Target xkos:exclusionNote ?Exclude_", B, " FILTER (LANG(?Exclude_", B, ") = '", langB, "') }\n",
      "    BIND (COALESCE(?AltB, ?PrefB) AS ?Label_", B, ")\n",
      "    BIND (STR(?TargetNotation) AS ?", B, ")\n",
      "  }\n",
      "  BIND (STR(?Associations)   AS ?URL)\n",
      "  BIND (STR(?SourceNotation) AS ?", A, ")\n",
      "}\n",
      "ORDER BY ?", A, "\n"
    )
  }
  SPARQL.query <- build_query(language, language)
  
  # ---- execute: prefer shared POST helper; GET fallback on HTML ----
  is_html <- function(txt) {
    nzchar(txt) && grepl("^\\s*<!DOCTYPE\\s+html|<html", txt, ignore.case = TRUE)
  }
  do_get <- function(q) {
    httr::GET(
      url    = endpoint_url,
      query  = list(query = q),
      httr::accept("application/sparql-results+csv, text/csv; q=0.9, */*; q=0.1"),
      httr::user_agent("correspondenceTables (R)"),
      httr::timeout(timeout_sec)
    )
  }
  
  csv_text <- ""
  if (exists(".do_post_csv", mode = "function")) {
    csv_text <- tryCatch(
      .do_post_csv(endpoint_url, SPARQL.query, timeout_sec = timeout_sec, attempts = max_retries),
      error = function(e) ""
    )
  } else {
    attempt <- 0L
    resp    <- NULL
    repeat {
      attempt <- attempt + 1L
      resp <- tryCatch(
        httr::POST(
          url    = endpoint_url,
          httr::accept("application/sparql-results+csv, text/csv; q=0.9, */*; q=0.1"),
          httr::content_type("application/x-www-form-urlencoded; charset=UTF-8"),
          body   = list(query = SPARQL.query),
          encode = "form",
          httr::user_agent("correspondenceTables (R)"),
          httr::timeout(timeout_sec)
        ),
        error = function(e) e
      )
      if (!inherits(resp, "error")) break
      if (attempt >= max_retries) break
      Sys.sleep(1.0 * attempt)
    }
    if (!inherits(resp, "error")) {
      httr::stop_for_status(resp)
      csv_text <- httr::content(resp, as = "text", encoding = "UTF-8")
      if (nzchar(csv_text)) csv_text <- sub("^\ufeff", "", csv_text)
    } else {
      csv_text <- ""
    }
  }
  
  # GET fallback if we received HTML
  if (is_html(csv_text)) {
    resp <- tryCatch(do_get(SPARQL.query), error = function(e) e)
    if (!inherits(resp, "error")) {
      httr::stop_for_status(resp)
      csv_text <- httr::content(resp, as = "text", encoding = "UTF-8")
      if (nzchar(csv_text)) csv_text <- sub("^\ufeff", "", csv_text)
    } else {
      csv_text <- ""
    }
  }
  
  # ---- parse into data.frame (base R only) ----
  parse_text_to_df <- function(txt) {
    if (!nzchar(txt)) return(data.frame())
    con <- textConnection(txt); on.exit(close(con), add = TRUE)
    df <- tryCatch(
      utils::read.csv(con, stringsAsFactors = FALSE, check.names = FALSE),
      error = function(e) {
        con2 <- textConnection(txt); on.exit(close(con2), add = TRUE)
        utils::read.delim(con2, stringsAsFactors = FALSE, check.names = FALSE)
      }
    )
    df
  }
  data <- parse_text_to_df(csv_text)
  
  # ---- optional normalization (off by default; attribute records state) ----
  ct_norm_code <- function(x, strip = "[[:space:].-]", case = c("lower","upper","asis")) {
    case <- match.arg(case)
    x <- as.character(x)
    x <- trimws(x)
    if (!is.null(strip) && nzchar(strip)) x <- gsub(strip, "", x)
    if (case == "lower") x <- tolower(x) else if (case == "upper") x <- toupper(x)
    x
  }
  if (isTRUE(normalize_codes)) {
    if (is.character(A) && nzchar(A) && A %in% names(data)) {
      data[[A]] <- ct_norm_code(data[[A]], strip = norm_strip, case = norm_case)
    }
    if (is.character(B) && nzchar(B) && B %in% names(data)) {
      data[[B]] <- ct_norm_code(data[[B]], strip = norm_strip, case = norm_case)
    }
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
  
  if (isTRUE(showQuery)) {
    return(list(SPARQL.query = SPARQL.query, CorrespondenceTable = data))
  } else {
    return(data) 
  }
}  


