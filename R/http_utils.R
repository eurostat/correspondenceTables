#' Retry a SPARQL POST and return CSV text (with BOM stripped)
#'
#' @param url Endpoint URL (character). 
#' @param query SPARQL query string.
#' @param timeout_sec Per-attempt timeout in seconds (integer).
#' @param attempts Number of attempts (integer).
#'
#' @return Character string with CSV content (may be "").
#' @keywords internal
.do_post_csv <- function(url, query, timeout_sec = 60L, attempts = 2L) {
  for (i in seq_len(attempts)) {
    resp <- tryCatch(
      httr::POST(
        url,
        httr::accept("text/csv"),
        body   = list(query = query),
        encode = "form",
        httr::timeout(timeout_sec)
      ),
      error = function(e) e
    )
    if (!inherits(resp, "error")) {
      httr::stop_for_status(resp)
      txt <- httr::content(resp, "text", encoding = "UTF-8")
      # Strip UTF-8 BOM if present
      if (nzchar(txt)) txt <- sub("^\ufeff", "", txt)
      return(txt)
    }
    Sys.sleep(1.0 * i)  # simple linear backoff
  }
  stop("SPARQL POST failed after ", attempts, " attempts.")
}