# Utility to extract token names from "PREFIX token: <...>" lines
.extract_tokens <- function(x) {
  sub("^PREFIX\\s+([^:]+):.*$", "\\1", x)
}

# Core tokens as hard-coded by prefixList()
.core_tokens <- c("dc", "dct", "rdfs", "rdf", "skos", "xkos", "xsd")

# 1) Validation of 'endpoint' argument ---------------------------------------

# Error for unsupported endpoints (message can vary if .valid_endpoints() is present)
expect_error(
  prefixList("ALL"),
  "(?i)(unsupported endpoint|'CELLAR'|FAO|non-empty|endpoint)",
  fixed  = FALSE
)

expect_error(
  prefixList("INVALID"),
  "(?i)(unsupported endpoint|'CELLAR'|FAO|non-empty|endpoint)",
  fixed  = FALSE
)

if (at_home()) {
  # Valid endpoints should not error (even if discovery fails => core-only)
  res_cellar <- try(prefixList("CELLAR"), silent = TRUE)
  expect_true(!inherits(res_cellar, "try-error"))
  
  res_fao <- try(prefixList("FAO"), silent = TRUE)
  expect_true(!inherits(res_fao, "try-error"))
  
  # 2) 'CELLAR' structure checks (character vector) ----------------------------
  
  res <- prefixList("CELLAR")
  expect_true(is.character(res))
  expect_true(length(res) >= length(.core_tokens))
  expect_true(all(grepl("^PREFIX\\s+", res)))
  
  
  # 3) 'FAO' structure checks (character vector) -------------------------------
  
  res <- prefixList("FAO")
  expect_true(is.character(res))
  expect_true(length(res) >= length(.core_tokens))
  expect_true(all(grepl("^PREFIX\\s+", res)))
  
  
  # 4) Filtering by subset of prefixes -----------------------------------------
  
  
  all_prefixes <- prefixList("FAO")
  prefix_names <- .extract_tokens(all_prefixes)
  
  # Consider "dynamic" as anything not in known core tokens
  dynamic_prefixes <- setdiff(prefix_names, .core_tokens)
  
  if (length(dynamic_prefixes) == 0L) {
    message("No dynamic prefixes found for FAO; skipping filtering test.")
  } else {
    target_prefix <- dynamic_prefixes[1L]
    
    filtered <- prefixList("FAO", prefix = target_prefix)
    expect_true(is.character(filtered))
    
    filtered_tokens <- .extract_tokens(filtered)
    # requested prefix should be present
    expect_true(target_prefix %in% filtered_tokens)
    # at least one core prefix should still be present
    expect_true(any(.core_tokens %in% filtered_tokens))
  }
  
  
  # 5) Requested prefixes do not exist -> warning + core-only result -----------
  
  # Current behavior: warn + return only core prefixes
  expect_warning(
    not_found <- prefixList("FAO", prefix = "THISDOESNOTEXIST"),
    "(?i)(requested prefix|not found|Returning.*core)",
    fixed  = FALSE
  )
  expect_true(is.character(not_found))
  expect_true(all(grepl("^PREFIX\\s+", not_found)))
  
  tokens_nf <- .extract_tokens(not_found)
  # Should NOT contain the requested token (note: tokens are normalized to lowercase)
  expect_false("thisdoesnotexist" %in% tokens_nf)
  # Should contain core tokens (at least these)
  expect_true(all(.core_tokens %in% tokens_nf))
  
}
