# ---- 1) rejects invalid endpoint -------------------------------------------

expect_error(
  retrieveClassificationTable(
    endpoint      = "INVALID",
    prefix        = "cn2022",
    conceptScheme = "cn2022"
  ),
  pattern = "Unsupported endpoint"
)



if(at_home()){
  
  # ---- 2) returns a data.frame when showQuery = FALSE ------------------------
  
  res_df <- 
    retrieveClassificationTable(
      endpoint      = "FAO",
      prefix        = "coicop1999",
      conceptScheme = "scheme",
      language      = "en",
      level         = "ALL",
      showQuery     = FALSE
      
    )
  
  expect_inherits(res_df, "data.frame")
  # Columns depend on endpoint response; keep only stable checks.
  expect_true(all(c("Concept", "Code", "Label", "Depth") %in% names(res_df)))
  
  # ---- 3) returns query and table when showQuery = TRUE ----------------------
  
  
  
  
  res_q <- (
    retrieveClassificationTable(
      endpoint      = "FAO",
      prefix        = "coicop1999",
      conceptScheme = "scheme",
      language      = "en",
      level         = "ALL",
      showQuery     = TRUE
    )
  )
  
  expect_true(is.list(res_q))
  expect_true(all(c("SPARQL.query", "scheme_uri", "ClassificationTable") %in% names(res_q)))
  expect_true(is.character(res_q$SPARQL.query))
  expect_true(is.character(res_q$scheme_uri))
  expect_inherits(res_q$ClassificationTable, "data.frame")
  
  # ---- 4) level filter is applied when level != ALL ----------------
  # This adds a light semantic check on 'level' using the live endpoint.
  
  
  
  
  df_all <- (
    retrieveClassificationTable(
      endpoint      = "FAO",
      prefix        = "coicop1999",
      conceptScheme = "scheme",
      language      = "en",
      level         = "ALL",
      showQuery     = FALSE
    )
  )
  
  df_2 <- (
    retrieveClassificationTable(
      endpoint      = "FAO",
      prefix        = "coicop1999",
      conceptScheme = "scheme",
      language      = "en",
      level         = "2",
      showQuery     = FALSE
    )
  )
  
  expect_inherits(df_all, "data.frame")
  expect_inherits(df_2,  "data.frame")
  expect_true(nrow(df_2) <= nrow(df_all))
  # Depth may be character; compare as character to avoid coercion issues
  if ("Depth" %in% names(df_2) && nrow(df_2) > 0) {
    expect_true(all(as.character(df_2$Depth) == "2"))
  }
  
}

# Tests for knownSchemes & preferMappingOnly
# These tests do NOT require network calls.

# Fake knownSchemes mapping (valid)
ks <- data.frame(
  Prefix        = "cbf",
  ConceptScheme = "cbf",
  URI           = "http://data.europa.eu/38u/cbf1.0/cbf",
  stringsAsFactors = FALSE
)

res <- retrieveClassificationTable(
  endpoint      = "CELLAR",
  prefix        = "cbf",
  conceptScheme = "cbf",
  knownSchemes  = ks,
  showQuery     = TRUE,  # to extract resolved scheme_uri
  preferMappingOnly = TRUE # to stick to the given dataset
)

# The scheme_uri MUST equal the mapping (no SPARQL discovery)
expect_true(is.list(res))
expect_true(res$scheme_uri == "http://data.europa.eu/38u/cbf1.0/cbf")



ks_matto <- data.frame(
  Prefix        = c("c","aooo"),
  ConceptScheme = c("c","aooo"),
  URI           = c("http://example.com/fake/scheme","casa"),
  stringsAsFactors = FALSE
)


expect_error(
  retrieveClassificationTable(
    endpoint      = "CELLAR",
    prefix        = "c",
    conceptScheme = "c",
    knownSchemes  = ks_matto,
    showQuery     = TRUE,  
    preferMappingOnly = T
  ),
  pattern = "The URI 'http://example.com/fake/scheme' from knownSchemes is not valid for endpoint 'CELLAR'"
)


# Mapping intentionally misses the (prefix, scheme) pair
ks_missing <- data.frame(
  Prefix        = "OTHER",
  ConceptScheme = "OTHER",
  URI           = "http://example.com/other/scheme",
  stringsAsFactors = FALSE
)

expect_error(
  retrieveClassificationTable(
    endpoint          = "CELLAR",
    prefix            = "cbf",
    conceptScheme     = "cbf",
    knownSchemes      = ks_missing,
    preferMappingOnly = TRUE
  ),
  pattern = "Prefix or ConceptScheme not present in this endpoint."
)



# Fake knownSchemes mapping (valid)
ks <- data.frame(
  Prefix        = "cbf",
  ConceptScheme = "cbf",
  URI           = "http://data.europa.eu/38u/cbf1.0/cbf",
  stringsAsFactors = FALSE
)

res <- retrieveClassificationTable(
  endpoint      = "CELLAR",
  prefix        = "cbf",
  conceptScheme = "cbf",
  knownSchemes  = ks,
  showQuery     = TRUE,  # to extract resolved scheme_uri
  preferMappingOnly = TRUE # to stick to the given dataset
)

# The scheme_uri MUST equal the mapping (no SPARQL discovery)
expect_true(is.list(res))
expect_true(res$scheme_uri == "http://data.europa.eu/38u/cbf1.0/cbf")



ks_matto <- data.frame(
  Prefix        = c("c","aooo"),
  ConceptScheme = c("c","aooo"),
  URI           = c("http://example.com/fake/scheme","casa"),
  stringsAsFactors = FALSE
)


expect_error(
  retrieveClassificationTable(
    endpoint      = "CELLAR",
    prefix        = "c",
    conceptScheme = "c",
    knownSchemes  = ks_matto,
    showQuery     = TRUE,  
    preferMappingOnly = T
  ),
  pattern = "The URI 'http://example.com/fake/scheme' from knownSchemes is not valid for endpoint 'CELLAR'"
)



# Mapping intentionally misses the (prefix, scheme) pair
ks_missing <- data.frame(
  Prefix        = "OTHER",
  ConceptScheme = "OTHER",
  URI           = "http://example.com/other/scheme",
  stringsAsFactors = FALSE
)

expect_error(
  retrieveClassificationTable(
    endpoint          = "CELLAR",
    prefix            = "cbf",
    conceptScheme     = "cbf",
    knownSchemes      = ks_missing,
    preferMappingOnly = TRUE
  ),
  pattern = "Prefix or ConceptScheme not present in this endpoint"
)

