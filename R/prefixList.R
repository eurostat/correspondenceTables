#' Normalize a SPARQL prefix label (pure)
#'
#' @description
#' Cleans a SPARQL prefix token so it is safe and consistent:
#' - lowercases,
#' - trims whitespace,
#' - removes trailing ":" and whitespace,
#' - keeps only [a-z0-9_] characters,
#' - drops empty results.
#'
#' @param x character vector of candidate prefix labels
#' @return cleaned, safe labels (lowercase, [a-z0-9_])
#' @keywords internal
#' 
normalize_prefix_token <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x <- gsub("[:\\s]+$", "", x)
  x <- gsub("[^a-z0-9_]", "", x)
  x[nzchar(x)]
}

#' Normalize a namespace IRI (pure)
#'
#' @description
#' Ensures each IRI:
#' - has an HTTP(S) scheme (defaults to https if missing),
#' - has no whitespace,
#' - ends with exactly one trailing slash.
#'
#' @param uri character vector
#' @return normalized IRIs
#' @keywords internal
#' 
normalize_namespace_iri <- function(uri) {
  u <- trimws(as.character(uri))
  has_scheme <- grepl("^(?i)https?://", u, perl = TRUE)
  u[!has_scheme] <- paste0("https://", u[!has_scheme])
  u <- gsub("\\s+", "", u)
  u <- sub("/+$", "", u)
  paste0(u, "/")
}

#' Build PREFIX lines from tokens and IRIs (pure)
#'
#' @param tokens character vector of prefix tokens (will be normalized)
#' @param iris character vector of namespace IRIs (will be normalized)
#' @return character vector like "PREFIX token: <iri/>"
#' @keywords internal
build_prefix_lines <- function(tokens, iris) {
  stopifnot(length(tokens) == length(iris))
  tokens <- normalize_prefix_token(tokens)
  iris   <- normalize_namespace_iri(iris)
  unique(sprintf("PREFIX %s: <%s>", tokens, iris))
}

#' Validate that a prefix token is declared in a prefix block (pure)
#'
#' @param token a candidate prefix token (will be normalized)
#' @param prefix_block a single string containing the PREFIX lines
#' @return TRUE if declared, FALSE otherwise
#' @keywords internal
prefix_declared_in_block <- function(token, prefix_block) {
  token <- normalize_prefix_token(token)
  if (!length(token)) return(FALSE)
  grepl(paste0("\\bPREFIX\\s+", token, ":"), prefix_block, ignore.case = TRUE)
}






#' @title Build SPARQL PREFIX declarations for an endpoint
#'
#' @description
#' Returns a character vector of `PREFIX ...` lines used by package queries.
#' The result always includes core vocabularies (SKOS/XKOS/RDFS/DC/RDF/XSD),
#' and—when available—adds classification scheme namespaces discovered via
#' `classificationList(endpoint)`. You can filter to specific schemes with
#' the `prefix` argument.
#'
#' @param endpoint Character. Endpoint name (case-insensitive). Typically
#'   `"CELLAR"` or `"FAO"`. The set of supported names is defined by the
#'   package's internal endpoint registry (see `.valid_endpoints()`).
#' @param prefix Character vector of scheme tokens to include (e.g.,
#'   `c("cn2022","nace2")`). If `NULL` (default), returns all discovered schemes.
#'
#' @return
#' A character vector of SPARQL PREFIX declarations.
#' Use `paste(result, collapse = "\\n")` to inject into a query string.
#'
#' @details
#' - This function has no side effects and does not perform network I/O
#'   by itself. Discovery is delegated to `classificationList()`.
#'   
#' - If `classificationList()` is unavailable or fails, the function returns
#'   only the core vocabularies and emits a warning.
#'
#' @examples
#' \dontrun{
#' # All known scheme prefixes for the endpoint
#' pl <- prefixList("CELLAR")
#' cat(paste(pl, collapse = "\n"))
#'
#' # Only include CN2022 and NACE2 if they exist
#' pl2 <- prefixList("CELLAR", prefix = c("cn2022", "nace2"))
#' }
#'
#' @export

prefixList <- function(endpoint, prefix = NULL) {
  # --- helpers (non-exported) ---
  .norm_name <- function(x) toupper(trimws(as.character(x)))
  .core_prefixes <- function() {
    c(
      "PREFIX dc: <http://purl.org/dc/elements/1.1/>",
      "PREFIX dct: <http://purl.org/dc/terms/>",
      "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>",
      "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>",
      "PREFIX skos: <http://www.w3.org/2004/02/skos/core#>",
      "PREFIX xkos: <http://rdf-vocabulary.ddialliance.org/xkos#>",
      "PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>"
    )
  }
  
  # --- validate endpoint via internal registry if present ---
  ep <- .norm_name(endpoint)
  if (exists(".valid_endpoints", mode = "function")) {
    valid <- tryCatch(.valid_endpoints(), error = function(e) character())
    if (length(valid) && !(ep %in% valid)) {
      stop(sprintf(
        "Unsupported endpoint '%s'. Supported: %s",
        endpoint, paste(valid, collapse = ", ")
      ), call. = FALSE)
    }
  } else {
    if (!nzchar(ep)) stop("`endpoint` must be a non-empty string.", call. = FALSE)
  }
  
  # --- start with core vocabularies ---
  prefix_lines <- .core_prefixes()
  
  # --- discover scheme namespaces via classificationList() ---
  schemes <- tryCatch(
    classificationList(ep),
    error = function(e) {
      warning(
        "prefixList(): classificationList('", ep, "') failed: ", conditionMessage(e),
        ". Returning only core prefixes."
      )
      NULL
    }
  )
  
  # If discovery failed or returned nothing usable, return core only
  if (!is.data.frame(schemes) || nrow(schemes) == 0L) {
    return(prefix_lines)
  }
  
  # Normalize expected columns: need a scheme token ("Prefix") and scheme URI ("URI")
  if (!("Prefix" %in% names(schemes))) {
    if ("ConceptScheme" %in% names(schemes)) {
      schemes$Prefix <- tolower(schemes$ConceptScheme)
    } else if ("URI" %in% names(schemes)) {
      schemes$Prefix <- tolower(sub(".*/", "", schemes$URI))
    } else {
      warning("prefixList(): 'Prefix' not found and cannot be derived. Returning core prefixes only.")
      return(prefix_lines)
    }
  }
  
  if (!("URI" %in% names(schemes))) {
    if ("SchemeURI" %in% names(schemes)) {
      schemes$URI <- schemes$SchemeURI
    } else if ("ConceptSchemeURI" %in% names(schemes)) {
      schemes$URI <- schemes$ConceptSchemeURI
    } else {
      warning("prefixList(): no URI column found to build scheme prefixes. Returning core prefixes only.")
      return(prefix_lines)
    }
  }
  
  # Optionally filter requested tokens
  scheme_tokens <- schemes$Prefix
  scheme_uris   <- schemes$URI
  
  if (!is.null(prefix)) {
    want <- normalize_prefix_token(prefix)
    have <- normalize_prefix_token(scheme_tokens)
    keep <- have %in% want
    if (!any(keep)) {
      warning(
        "prefixList(): requested prefix(es) not found for endpoint ", ep, ": ",
        paste(prefix, collapse = ", "), ". Returning core prefixes only."
      )
      return(prefix_lines)
    }
    scheme_tokens <- scheme_tokens[keep]
    scheme_uris   <- scheme_uris[keep]
  }
  
  add <- build_prefix_lines(scheme_tokens, scheme_uris)
  out <- c(prefix_lines, add)
  # Drop duplicates while preserving order
  out[!duplicated(out)]
}
