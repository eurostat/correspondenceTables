#' Build a SPARQL PREFIX block for CELLAR or FAO classifications
#'
#' Builds a complete SPARQL `PREFIX` block by combining a fixed set of
#' common prefixes with classification-specific namespaces retrieved
#' dynamically from the selected registry endpoint (**CELLAR** or **FAO**).
#'
#' The function returns a structured result containing the generated
#' prefixes and, optionally, a validation table describing the outcome
#' of SPARQL and/or HTTP checks on the discovered ConceptScheme IRIs.
#'
#' @section Returns:
#' The returned object is a list of class \code{prefixList_result} with:
#' \itemize{
#'   \item \code{prefixes}: character vector or 1‑column matrix of SPARQL
#'     \code{PREFIX} declarations (controlled by \code{return})
#'   \item \code{validation}: a \code{data.frame} with validation results,
#'     or \code{NULL} when \code{validate = "none"}
#' }
#'
#' @section Supported features:
#' \itemize{
#'   \item EUVOC acceptance (default \code{accept = "skos_or_euvoc"})
#'   \item relaxed mode (timeouts and indeterminate checks warn but do not fail)
#'   \item manual whitelist (explicitly skip validation for selected schemes)
#'   \item auto‑whitelisting with graduated rules
#'     (\code{auto_whitelist = "strict"} applies rules A + B + C)
#' }
#'
#' @param endpoint Character scalar. Registry endpoint to query.
#'   Must be either \code{"CELLAR"} or \code{"FAO"} (case‑insensitive).
#' @param prefix Optional character vector of scheme aliases to keep.
#'   Matching is case‑insensitive and tolerant to dots and punctuation,
#'   and works for both machine identifiers and cleaned human labels.
#' @param validate Character string. Validation mode:
#'   \code{"ask"} (SPARQL ASK checks),
#'   \code{"http"} (HTTP HEAD checks),
#'   or \code{"none"} (no validation).
#' @param accept Character string. ConceptScheme typing accepted by ASK
#'   validation: \code{"skos_or_euvoc"} or \code{"skos"}.
#' @param relaxed Logical. If \code{TRUE}, validation failures due to timeouts
#'   or HTTP issues generate warnings instead of hard failures.
#' @param whitelist Optional character vector of scheme aliases or cleaned
#'   labels that should be treated as valid without remote validation.
#' @param auto_whitelist Character string controlling automatic acceptance
#'   of borderline cases. One of \code{"none"}, \code{"minimal"},
#'   \code{"moderate"}, or \code{"strict"}.
#' @param stop_on_invalid Logical. If \code{TRUE}, stop execution when
#'   invalid or indeterminate schemes remain after whitelisting.
#' @param timeout_sec Numeric. Timeout (in seconds) for SPARQL and HTTP requests.
#' @param return Character string. Output shape of \code{prefixes}:
#'   \code{"matrix"} (default) or \code{"vector"}.
#' @param verbose Logical. If \code{TRUE}, print progress messages and
#'   validation summaries.
#'
#' @details
#' When validation is enabled, the returned \code{validation} table includes
#' normalized logical fields (\code{ASK_true}, \code{http_ok}) and a derived
#' \code{decision} column summarizing the outcome (e.g. \code{"ok"},
#' \code{"fail"}, \code{"indeterminate"}, \code{"whitelisted"},
#' \code{"auto_whitelisted:*"}).
#'
#' Use \code{\link{validationOf}} to extract the validation table and
#' \code{\link{prefixLines}} to obtain the prefix block as a simple
#' character vector suitable for embedding in SPARQL queries.
#'
#' @examples
#' \dontrun{
#' ## -----------------------------------------------------------------------------
#' ## FAO: Retrieve prefixes, validate, summarize
#' ## -----------------------------------------------------------------------------
#'
#' # 1) Retrieve prefix list from FAO SPARQL endpoint
#' fao <- prefixList(
#'   endpoint       = "FAO",
#'   auto_whitelist = "strict",
#'   verbose        = TRUE
#' )
#'
#' # 2) Show generated PREFIX lines (for embedding in SPARQL queries)
#' head(prefixLines(fao))
#'
#' # 3) Validate all FAO scheme IRIs and inspect results
#' val_fao <- validationOf(fao)
#' if (!is.null(val_fao)) {
#'   # Keep only columns that exist in the selected validation mode (ASK/HTTP)
#'   cols_fao <- intersect(c("alias","ASK_true","http_ok","note","decision"), names(val_fao))
#'
#'   # Derive a robust 'decision' if not already present
#'   if (!"decision" %in% names(val_fao)) {
#'     val_fao$decision <- with(
#'       val_fao,
#'       ifelse(
#'         !is.na(note), note,                                 # explicit note wins
#'         ifelse(
#'           isFALSE(ASK_true) | isFALSE(http_ok), "fail",     # any check fails
#'           ifelse(
#'             isTRUE(ASK_true) & isTRUE(http_ok), "ok",       # perfect validation
#'             "indeterminate"                                 # last fallback
#'           )
#'         )
#'       )
#'     )
#'     cols_fao <- unique(c(cols_fao, "decision"))
#'   }
#'
#'   # Quick peek + decision summary
#'   print(head(val_fao[, cols_fao, drop = FALSE]))
#'   print(table(val_fao$decision, useNA = "ifany"))
#' }
#'
#' ## -----------------------------------------------------------------------------
#' ## CELLAR: Retrieve prefixes, validate, summarize
#' ## -----------------------------------------------------------------------------
#'
#' cellar <- prefixList("CELLAR", auto_whitelist = "strict", verbose = TRUE)
#' head(prefixLines(cellar))
#' val_cellar <- validationOf(cellar)
#'
#' if (!is.null(val_cellar)) {
#'   cols_cellar <- intersect(c("alias","ASK_true","http_ok","note","decision"), names(val_cellar))
#'
#'   # Derive 'decision' if missing
#'   if (!"decision" %in% names(val_cellar)) {
#'     val_cellar$decision <- with(
#'       val_cellar,
#'       ifelse(
#'         !is.na(note), note,                                  # explicit note wins
#'         ifelse(
#'           isFALSE(ASK_true) | isFALSE(http_ok), "fail",      # any check fails
#'           ifelse(
#'             isTRUE(ASK_true) & isTRUE(http_ok), "ok",        # both checks pass
#'             "indeterminate"                                  # last fallback
#'           )
#'         )
#'       )
#'     )
#'   }
#'
#'   # (Optional) factor ordering for cleaner summaries
#'   if (is.character(val_cellar$decision)) {
#'     val_cellar$decision <- factor(val_cellar$decision, levels = c("ok","fail","indeterminate"))
#'   }
#'
#'   # Head with key columns and a summary table
#'   cols_cellar <- unique(c(cols_cellar, "decision"))
#'   print(head(val_cellar[, cols_cellar, drop = FALSE]))
#'   print(table(val_cellar$decision, useNA = "ifany"))
#' }
#' }
#'
#' @seealso \code{\link{validationOf}}, \code{\link{prefixLines}},
#'   \code{\link{classificationList}}
#'
#' @export
prefixList <- function(endpoint,
                       prefix = NULL,
                       validate = c("ask", "none", "http"),
                       accept   = c("skos_or_euvoc", "skos"),
                       relaxed  = FALSE,
                       whitelist = NULL,                     # manual whitelist (aliases or cleaned labels)
                       auto_whitelist = c("none","minimal","moderate","strict"),
                       stop_on_invalid = FALSE,
                       timeout_sec = 30,
                       return = c("matrix", "vector"),
                       verbose = FALSE) {
  
  # --- Robust bool normalizer (handles logical, numeric 0/1, factor, "true"/"false") ---
  to_bool <- function(x) {
    if (is.null(x)) return(x)
    if (is.logical(x)) return(x)
    if (is.factor(x)) x <- as.character(x)
    if (is.numeric(x)) return(ifelse(is.na(x), NA, x != 0))
    if (is.character(x)) {
      xl <- tolower(trimws(x))
      out <- rep(NA, length(xl))
      out[xl %in% c("true", "t", "1", "yes", "y")]  <- TRUE
      out[xl %in% c("false","f","0","no","n")]      <- FALSE
      return(out)
    }
    suppressWarnings(as.logical(x))
  }
  
  endpoint <- toupper(endpoint)
  if (!endpoint %in% c("CELLAR", "FAO")) {
    stop("`endpoint` must be either 'CELLAR' or 'FAO'.")
  }
  validate       <- match.arg(validate)
  accept         <- match.arg(accept)
  auto_whitelist <- match.arg(auto_whitelist)
  return         <- match.arg(return)
  
  # --- Static/common prefixes (use real < and >) ---------------------------
  prefix_init <- as.matrix(rbind(
    "PREFIX dc: <http://purl.org/dc/elements/1.1/>",
    "PREFIX dct: <http://purl.org/dc/terms/>",
    "PREFIX cb: <http://cbasewrap.ontologycentral.com/vocab#>",
    "PREFIX eli: <http://data.europa.eu/eli/ontology#>",
    "PREFIX euvoc: <http://publications.europa.eu/ontology/euvoc#>",
    "PREFIX owl: <http://www.w3.org/2002/07/owl#>",
    "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>",
    "PREFIX skosxl: <http://www.w3.org/2008/05/skos-xl#>",
    "PREFIX xml: <http://www.w3.org/XML/1998/namespace>",
    "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>",
    "PREFIX skos: <http://www.w3.org/2004/02/skos/core#>",
    "PREFIX xkos: <http://rdf-vocabulary.ddialliance.org/xkos#>",
    "PREFIX is: <http://purl.org/ontology/is/core#>",
    "PREFIX isi: <http://purl.org/ontology/is/inst/>",
    "PREFIX cpc: <https://data.epo.org/linked-data/def/cpc/>",
    "PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>",
    "PREFIX CPC20: <https://unstats.un.org/classifications/CPC/v2.0/>",
    "PREFIX CPC21: <https://unstats.un.org/classifications/CPC/v2.1/>"
  ))
  
  # --- Retrieve registry ----------------------------------------------------
  res <- classificationList(endpoint)
  if (is.list(res) && !is.null(res[[endpoint]]) && is.data.frame(res[[endpoint]])) {
    res <- res[[endpoint]]
  }
  if (!is.data.frame(res) || !all(c("Prefix", "ConceptScheme", "URI") %in% colnames(res))) {
    stop("Unexpected structure returned by classificationList().")
  }
  
  # --- Build alias candidates & namespaces ---------------------------------
  alias_machine <- res$ConceptScheme                       # e.g. "nace2", "CPC21"
  alias_human   <- gsub("[^A-Za-z0-9._-]", "", res$Prefix) # e.g. "NACE2", "CPC21"
  
  # Case-insensitive + dotless
  alias_machine_lc       <- tolower(alias_machine)
  alias_human_lc         <- tolower(alias_human)
  alias_machine_nodot_lc <- gsub("\\.", "", alias_machine_lc)
  alias_human_nodot_lc   <- gsub("\\.", "", alias_human_lc)
  
  # Proper namespace = parent folder of URI (drop final segment/local name)
  ns <- sub("/[^/]+$", "/", res$URI)
  
  # Use machine id as alias in SPARQL; stable
  dynamic_all <- as.matrix(paste0("PREFIX ", alias_machine, ": <", ns, ">"))
  
  # --- Filter requested prefixes (if any) -----------------------------------
  keep <- rep(TRUE, length(alias_machine))
  if (!is.null(prefix)) {
    prefix_clean    <- gsub("[^A-Za-z0-9._-]", "", prefix)
    prefix_lc       <- tolower(prefix_clean)
    prefix_nodot_lc <- gsub("\\.", "", prefix_lc)
    
    keep <- (alias_machine_lc       %in% prefix_lc)       |
      (alias_human_lc         %in% prefix_lc)       |
      (alias_machine_nodot_lc %in% prefix_nodot_lc) |
      (alias_human_nodot_lc   %in% prefix_nodot_lc)
    
    if (!any(keep)) {
      stop("Desired prefixes not found for endpoint ", endpoint, ".")
    }
  }
  
  dynamic <- dynamic_all[keep, , drop = FALSE]
  
  # --- Optional validation --------------------------------------------------
  validation_df <- NULL
  if (validate != "none") {
    kept_idx <- which(keep)
    df <- data.frame(
      alias      = alias_machine[kept_idx],
      namespace  = ns[kept_idx],
      scheme_iri = res$URI[kept_idx],
      stringsAsFactors = FALSE
    )
    
    # Manual whitelist matching (aliases or cleaned labels)
    is_whitelisted <- rep(FALSE, nrow(df))
    if (!is.null(whitelist)) {
      wl_clean    <- gsub("[^A-Za-z0-9._-]", "", whitelist)
      wl_lc       <- tolower(wl_clean)
      wl_nodot_lc <- gsub("\\.", "", wl_lc)
      
      alias_machine_keep_lc       <- alias_machine_lc[kept_idx]
      alias_human_keep_lc         <- alias_human_lc[kept_idx]
      alias_machine_keep_nodot_lc <- alias_machine_nodot_lc[kept_idx]
      alias_human_keep_nodot_lc   <- alias_human_nodot_lc[kept_idx]
      
      is_whitelisted <- (alias_machine_keep_lc       %in% wl_lc) |
        (alias_human_keep_lc         %in% wl_lc) |
        (alias_machine_keep_nodot_lc %in% wl_nodot_lc) |
        (alias_human_keep_nodot_lc   %in% wl_nodot_lc)
      
      if (verbose && !any(is_whitelisted)) {
        warning("None of the provided 'whitelist' items matched the kept schemes.", immediate. = TRUE)
      }
    }
    
    # Endpoint URL for SPARQL/HTTP validation
    get_endpoint_url <- function(ep) {
      config_url <- "https://raw.githubusercontent.com/eurostat/correspondenceTables/main/inst/extdata/endpoint_source_config.json"
      cfg <- jsonlite::fromJSON(config_url)
      switch(ep,
             "CELLAR" = cfg$CELLAR,
             "FAO"    = cfg$FAO,
             stop("Unknown endpoint: ", ep))
    }
    source_url <- get_endpoint_url(endpoint)
    
    # ASK validator (supports SKOS and optionally EUVOC)
    run_ask <- function(source_url, alias, ns, scheme_iri, accept) {
      prefix_block <- paste(
        "PREFIX skos: <http://www.w3.org/2004/02/skos/core#>",
        "PREFIX euvoc: <http://publications.europa.eu/ontology/euvoc#>",
        sprintf("PREFIX %s: <%s>", alias, ns),
        sep = "\n"
      )
      if (identical(accept, "skos")) {
        ask_body <- sprintf("<%s> a skos:ConceptScheme .", scheme_iri)
      } else {
        ask_body <- sprintf("{ <%s> a skos:ConceptScheme . } UNION { <%s> a euvoc:ConceptScheme . }",
                            scheme_iri, scheme_iri)
      }
      ask_query <- paste0(prefix_block, "\nASK WHERE { ", ask_body, " }")
      
      resp <- httr::POST(
        url    = source_url,
        body   = list(query = ask_query),
        encode = "form",
        httr::accept("application/sparql-results+json"),
        httr::timeout(timeout_sec)
      )
      ok <- TRUE; val <- NA; err <- NA_character_
      tryCatch({
        httr::stop_for_status(resp)
        js <- jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"))
        val <- isTRUE(js$boolean)
      }, error = function(e) {
        ok  <<- FALSE
        err <<- conditionMessage(e)
      })
      list(http_ok = ok, ASK_true = val, error = err)
    }
    
    # EUVOC-only ASK (for auto-whitelist Rule A when accept == "skos")
    run_ask_euvoc_only <- function(source_url, alias, ns, scheme_iri) {
      prefix_block <- paste(
        "PREFIX euvoc: <http://publications.europa.eu/ontology/euvoc#>",
        sprintf("PREFIX %s: <%s>", alias, ns),
        sep = "\n"
      )
      ask_query <- paste0(prefix_block, "\nASK WHERE { <", scheme_iri, "> a euvoc:ConceptScheme . }")
      resp <- httr::POST(
        url    = source_url,
        body   = list(query = ask_query),
        encode = "form",
        httr::accept("application/sparql-results+json"),
        httr::timeout(timeout_sec)
      )
      ok <- TRUE; val <- NA; err <- NA_character_
      tryCatch({
        httr::stop_for_status(resp)
        js <- jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"))
        val <- isTRUE(js$boolean)
      }, error = function(e) {
        ok  <<- FALSE
        err <<- conditionMessage(e)
      })
      list(http_ok = ok, ASK_true = val, error = err)
    }
    
    # HTTP validator (best-effort) for validate == "http"
    run_http <- function(url) {
      ok <- FALSE; code <- NA_integer_; err <- NA_character_
      resp <- try(httr::HEAD(url, httr::timeout(timeout_sec)), silent = TRUE)
      if (inherits(resp, "try-error")) {
        err <- as.character(resp)
      } else {
        code <- httr::status_code(resp)
        ok <- code >= 200 && code < 400
      }
      list(http_ok = ok, status = code, error = err)
    }
    
    if (validate == "ask") {
      if (verbose) message("Validating scheme IRIs with SPARQL ASK (", endpoint,
                           "; accept=", accept, "; relaxed=", relaxed,
                           "; auto_whitelist=", auto_whitelist, ") …")
      
      # Build whitelisted rows (forced OK)
      df_whitelist <- NULL
      if (any(is_whitelisted)) {
        df_whitelist <- transform(df[is_whitelisted, , drop = FALSE],
                                  ASK_true = TRUE, http_ok = TRUE, error = NA_character_, note = "whitelisted")
      }
      
      # Validate remaining rows
      df_validate <- NULL
      if (any(!is_whitelisted)) {
        rows <- lapply(which(!is_whitelisted), function(i) {
          r <- df[i, ]
          res_ <- run_ask(source_url, r$alias, r$namespace, r$scheme_iri, accept)
          data.frame(r, ASK_true = res_$ASK_true, http_ok = res_$http_ok,
                     error = res_$error, note = NA_character_, row.names = NULL)
        })
        df_validate <- do.call(rbind, rows)
        
        # ---- Auto-whitelisting rules --------------------------------------
        if (!is.null(df_validate) && nrow(df_validate) > 0 && auto_whitelist != "none") {
          apply_rule_A <- auto_whitelist %in% c("minimal","moderate","strict")
          apply_rule_B <- auto_whitelist %in% c("moderate","strict")
          apply_rule_C <- auto_whitelist %in% c("strict")
          
          # Rule A: if ASK FALSE and accept == "skos", allow EUVOC type
          if (apply_rule_A) {
            idx_false <- which(isFALSE(df_validate$ASK_true))
            if (length(idx_false) > 0 && identical(accept, "skos")) {
              for (j in idx_false) {
                r <- df_validate[j, ]
                euv <- run_ask_euvoc_only(source_url, r$alias, r$namespace, r$scheme_iri)
                if (isTRUE(euv$ASK_true)) {
                  df_validate$ASK_true[j] <- TRUE
                  df_validate$http_ok[j]  <- TRUE
                  df_validate$note[j]     <- "auto_whitelisted:A(euvoc)"
                  df_validate$error[j]    <- NA_character_
                }
              }
            }
          }
          
          # Rule B: ASK NA but HTTP ok -> treat as OK
          if (apply_rule_B) {
            idx_na_ok <- which(is.na(df_validate$ASK_true) & df_validate$http_ok %in% TRUE)
            if (length(idx_na_ok) > 0) {
              df_validate$ASK_true[idx_na_ok] <- TRUE
              df_validate$note[idx_na_ok]     <- ifelse(is.na(df_validate$note[idx_na_ok]),
                                                        "auto_whitelisted:B(askNA_httpOK)",
                                                        paste0(df_validate$note[idx_na_ok], ",B(askNA_httpOK)"))
            }
          }
          
          # Rule C: present in registry -> any remaining FALSE -> OK
          if (apply_rule_C) {
            idx_false2 <- which(isFALSE(df_validate$ASK_true))
            if (length(idx_false2) > 0) {
              df_validate$ASK_true[idx_false2] <- TRUE
              df_validate$http_ok[idx_false2]  <- ifelse(is.na(df_validate$http_ok[idx_false2]), TRUE, df_validate$http_ok[idx_false2])
              df_validate$note[idx_false2]     <- ifelse(is.na(df_validate$note[idx_false2]),
                                                         "auto_whitelisted:C(registry)",
                                                         paste0(df_validate$note[idx_false2], ",C(registry)"))
              df_validate$error[idx_false2]    <- NA_character_
            }
          }
        }
      }
      
      # Combine validation pieces
      validation_df <- do.call(rbind, Filter(Negate(is.null), list(df_whitelist, df_validate)))
      
      # --- Normalize and derive decision (ASK mode) -------------------------
      if (!is.null(validation_df) && nrow(validation_df)) {
        if ("ASK_true" %in% names(validation_df)) validation_df$ASK_true <- to_bool(validation_df$ASK_true)
        if ("http_ok"  %in% names(validation_df)) validation_df$http_ok  <- to_bool(validation_df$http_ok)
        
        validation_df$decision <- with(validation_df,
                                       ifelse(!is.na(note), note,                                        # overrides win
                                              ifelse(isTRUE(ASK_true) & isTRUE(http_ok), "ok",                # required rule
                                                     ifelse(isFALSE(ASK_true), "fail",
                                                            ifelse(isFALSE(http_ok) | is.na(ASK_true) | is.na(http_ok),
                                                                   "indeterminate", "unknown")
                                                     )
                                              )
                                       )
        )
      }
      
      # Decision checks (messages/warnings only)
      if (!is.null(validation_df) && nrow(validation_df)) {
        if (relaxed) {
          fail_idx <- which(isFALSE(validation_df$ASK_true) & is.na(validation_df$note))
          warn_idx <- which((is.na(validation_df$ASK_true) | !validation_df$http_ok) & is.na(validation_df$note))
          if (length(fail_idx) > 0) {
            msg <- "Validation (ASK, relaxed): some scheme IRIs are NOT typed as ConceptScheme (after auto-whitelisting)."
            if (stop_on_invalid) stop(msg, call. = FALSE) else warning(msg, immediate. = TRUE)
          } else if (length(warn_idx) > 0) {
            warning("Validation (ASK, relaxed): some checks indeterminate (timeouts or HTTP errors).", immediate. = TRUE)
          }
        } else {
          bad <- is.na(validation_df$ASK_true) | !validation_df$ASK_true | !validation_df$http_ok
          bad[!is.na(validation_df$note)] <- FALSE
          if (any(bad)) {
            msg <- "Validation (ASK) failed or indeterminate for some scheme IRIs."
            if (stop_on_invalid) stop(msg, call. = FALSE) else warning(msg, immediate. = TRUE)
          }
        }
      }
      
      if (verbose && !is.null(validation_df)) {
        ok_n <- sum(validation_df$ASK_true %in% TRUE, na.rm = TRUE)
        wl_n <- sum(!is.na(validation_df$note) & grepl("^whitelisted", validation_df$note))
        aw_n <- sum(!is.na(validation_df$note) & grepl("^auto_whitelisted", validation_df$note))
        message("Validation summary (ASK): ", ok_n, "/", nrow(validation_df),
                " OK",
                if (wl_n > 0) paste0(" (", wl_n, " whitelisted)") else "",
                if (aw_n > 0) paste0(" (", aw_n, " auto-whitelisted)") else "")
      }
      
    } else if (validate == "http") {
      if (verbose) message("Validating namespaces and scheme IRIs with HTTP HEAD (best-effort) …")
      
      make_row <- function(alias, url, what, ok, status, err, note = NA_character_) {
        data.frame(alias = alias, url = url, what = what,
                   http_ok = ok, status = status, error = err, note = note,
                   stringsAsFactors = FALSE)
      }
      
      rows <- list()
      for (i in seq_len(nrow(df))) {
        r <- df[i, ]
        if (is_whitelisted[i]) {
          rows[[length(rows) + 1]] <- make_row(r$alias, r$namespace, "namespace", TRUE, NA_integer_, NA_character_, "whitelisted")
          rows[[length(rows) + 1]] <- make_row(r$alias, r$scheme_iri, "scheme_iri", TRUE, NA_integer_, NA_character_, "whitelisted")
        } else {
          ns_res  <- run_http(r$namespace)
          iri_res <- run_http(r$scheme_iri)
          rows[[length(rows) + 1]] <- make_row(r$alias, r$namespace, "namespace", ns_res$http_ok, ns_res$status, ns_res$error)
          rows[[length(rows) + 1]] <- make_row(r$alias, r$scheme_iri, "scheme_iri", iri_res$http_ok, iri_res$status, iri_res$error)
        }
      }
      validation_df <- do.call(rbind, rows)
      
      # --- Normalize and derive decision (HTTP mode) ------------------------
      if (!is.null(validation_df) && nrow(validation_df)) {
        if ("http_ok" %in% names(validation_df)) validation_df$http_ok <- to_bool(validation_df$http_ok)
        # No ASK_true in HTTP mode; classify simply:
        validation_df$decision <- with(validation_df,
                                       ifelse(!is.na(note), note,
                                              ifelse(isTRUE(http_ok), "ok", "indeterminate")
                                       )
        )
      }
      
      # HTTP mode warning summary
      bad <- !validation_df$http_ok
      bad[!is.na(validation_df$note) & validation_df$note == "whitelisted"] <- FALSE
      if (any(bad, na.rm = TRUE)) {
        msg <- "HTTP validation failed for some namespace/scheme IRIs (this can be normal if IRIs are not dereferenceable)."
        if (stop_on_invalid) stop(msg, call. = FALSE) else warning(msg, immediate. = TRUE)
      }
      if (verbose) {
        ok_n <- sum(validation_df$http_ok %in% TRUE, na.rm = TRUE)
        wl_n <- sum(!is.na(validation_df$note) & validation_df$note == "whitelisted")
        message("Validation summary (HTTP): ", ok_n, "/", nrow(validation_df),
                " OK (rows include namespace & scheme_iri)",
                if (wl_n > 0) paste0(" (", wl_n, " whitelisted)") else "")
      }
    }
  }
  
  # --- Combine, deduplicate -------------------------------------------------
  prefix_all <- rbind(prefix_init, dynamic)
  prefix_all <- prefix_all[!duplicated(prefix_all), , drop = FALSE]
  
  # SAFETY: ensure real "<" and ">" (in case something upstream HTML-escaped)
  unescape_html <- function(x) {
    x <- gsub("&amp;lt;", "<", x, fixed = TRUE)
    x <- gsub("&amp;gt;", ">", x, fixed = TRUE)
    x <- gsub("&lt;",     "<", x, fixed = TRUE)
    x <- gsub("&gt;",     ">", x, fixed = TRUE)
    x <- gsub("&#60;",    "<", x, fixed = TRUE)
    x <- gsub("&#62;",    ">", x, fixed = TRUE)
    x
  }
  prefix_all <- matrix(unescape_html(prefix_all), ncol = 1)
  
  # Prepare the output with requested type
  prefix_out <- if (return == "vector") as.vector(prefix_all) else prefix_all
  
  # --- Return as a list -----------------------------------------------------
  result <- list(
    prefixes       = prefix_out,
    validation     = validation_df,     # now includes normalized booleans + decision
    endpoint       = endpoint,
    validate       = validate,
    accept         = accept,
    relaxed        = relaxed,
    whitelist      = whitelist,
    auto_whitelist = auto_whitelist,
    aliases_kept   = alias_machine[keep],
    generated_at   = Sys.time()
  )
  class(result) <- c("prefixList_result", class(result))
  return(result)
}

# ---- Helpers ---------------------------------------------------
prefixLines <- function(x) {
  if (inherits(x, "prefixList_result")) return(as.vector(x$prefixes))
  as.vector(x)
}
validationOf <- function(x) {
  if (inherits(x, "prefixList_result")) return(x$validation)
  attr(x, "validation", exact = TRUE)
}


