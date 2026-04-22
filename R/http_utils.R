# -------------------------------------------------------------------
# Internal SPARQL POST helper (CSV, Virtuoso-compatible)
# -------------------------------------------------------------------
.do_post_csv <- function(endpoint_url, query, timeout_sec = 120L, attempts = 3L) {
  
  resp <- httr::RETRY(
    verb  = "POST",
    url   = endpoint_url,
    times = attempts,
    pause_base = 1,
    pause_cap  = 10,
    terminate_on = c(200, 400, 403, 404, 500, 503),
    httr::accept("text/csv"),
    httr::content_type("application/x-www-form-urlencoded; charset=UTF-8"),
    body = list(
      query   = query,
      format  = "text/csv",
      timeout = 0        # Virtuoso server-side execution hint
    ),
    encode = "form",
    httr::user_agent("correspondenceTables (R)"),
    httr::timeout(timeout_sec)
  )
  
  httr::stop_for_status(resp)
  
  txt <- httr::content(resp, as = "text", encoding = "UTF-8")
  if (nzchar(txt)) txt <- sub("^\ufeff", "", txt)
  
  txt
}


# -------------------------------------------------------------------
# Internal helper: read CSV text returned by SPARQL endpoints
# -------------------------------------------------------------------
.read_sparql_csv <- function(csv_text) {
  
  if (!is.character(csv_text) || !nzchar(csv_text)) {
    return(data.frame())
  }
  
  utils::read.csv(
    text            = csv_text,
    sep             = ",",
    stringsAsFactors = FALSE,
    check.names     = FALSE
  )
}



# -------------------------------------------------------------------
# Internal helpers: detect non-CSV responses from SPARQL endpoints
# -------------------------------------------------------------------

# HTML pages (error pages, redirects, etc.)
is_html <- function(x) {
  if (!is.character(x) || !length(x)) return(FALSE)
  grepl("^\\s*<!DOCTYPE\\s+html|^\\s*<html", x, ignore.case = TRUE)
}

# XML responses (Virtuoso error XML, SPARQL XML output, etc.)
is_xml <- function(x) {
  if (!is.character(x) || !length(x)) return(FALSE)
  grepl("^\\s*<\\?xml|^\\s*<sparql", x, ignore.case = TRUE)
}




