# R/endpoints.R
# Centralized endpoint handling (internal-only)

#' @keywords internal
.sparql_endpoints <- function() {
  c(
    "CELLAR" = "https://publications.europa.eu/webapi/rdf/sparql",
    "FAO"    = "https://caliper.integratedmodelling.org/caliper/sparql"
  )
}

#' @keywords internal
.valid_endpoints <- function() {
  names(.sparql_endpoints())
}

# Optional: small normalizer to keep code DRY across helpers
#' @keywords internal
.normalize_endpoints <- function(x) {
  x <- as.character(x)
  toupper(trimws(x))
}

#' @keywords internal
.validate_endpoints <- function(x) {
  x <- .normalize_endpoints(x)
  
  # Guard against NULL/empty/NA
  if (is.null(x) || !length(x) || any(is.na(x))) {
    stop("'endpoint' must be a non-empty character value.", call. = FALSE)
  }
  
  # Disallow mixing "ALL" with others to avoid ambiguity
  if (any(x == "ALL") && length(x) > 1L) {
    stop("'ALL' cannot be combined with specific endpoints. Use 'ALL' alone or list endpoints explicitly.",
         call. = FALSE)
  }
  
  # Allow "ALL" to pass through (expanded by .expand_endpoints())
  if (identical(x, "ALL")) return("ALL")
  
  # Validate against registry
  bad <- setdiff(x, .valid_endpoints())
  if (length(bad)) {
    stop(
      sprintf(
        "Unsupported endpoint(s): %s. Supported endpoints are: %s",
        paste(bad, collapse = ", "),
        paste(.valid_endpoints(), collapse = ", ")
      ),
      call. = FALSE
    )
  }
  x
}

#' @keywords internal
.expand_endpoints <- function(x) {
  x <- .validate_endpoints(x)
  if (identical(x, "ALL")) .valid_endpoints() else x
}

#' @keywords internal
.sparql_endpoint <- function(x) {
  key  <- .normalize_endpoints(x)
  urls <- .sparql_endpoints()
  if (!nzchar(key) || !(key %in% names(urls))) {
    stop(sprintf(
      "Unsupported endpoint '%s'. Supported endpoints are: %s",
      x, paste(names(urls), collapse = ", ")
    ), call. = FALSE)
  }
  unname(urls[[key]])
}