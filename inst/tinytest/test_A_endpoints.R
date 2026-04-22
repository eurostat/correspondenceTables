
if (at_home()) {
  
  # Minimal SPARQL query: asks only if the endpoint can answer a basic ASK request
  ask_query <- "ASK { ?s ?p ?o }"
  
  ping_endpoint <- function(endpoint_name) {
    # Resolve endpoint URL via internal helper used by the package
    endpoint_url <- .sparql_endpoint(endpoint_name)
    
    # Perform a minimal POST request to check reachability
    resp <- httr::POST(
      url    = endpoint_url,
      body   = list(query = ask_query),
      encode = "form",
      httr::accept("text/plain")
    )
    
    status <- httr::status_code(resp)
    txt    <- httr::content(resp, as = "text", encoding = "UTF-8")
    
    # Basic reachability: HTTP 2xx
    expect_true(
      status >= 200 && status < 300,
      info = paste0("Endpoint ", endpoint_name, " reachable (HTTP ", status, ")")
    )
    
    # Basic sanity: non-empty response body
    expect_true(
      nchar(txt) > 0,
      info = paste0("Endpoint ", endpoint_name, " returned a non-empty response")
    )
  }
  
  # Check both supported endpoints
  ping_endpoint("CELLAR")
  ping_endpoint("FAO")
}

