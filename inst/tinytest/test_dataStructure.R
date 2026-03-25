# -------- 1) Input validation: endpoint -------------------------------------

expect_error(
  dataStructure(endpoint = "INVALID", prefix = "CPC20", conceptScheme = "CPC20"),
  "(?i)endpoint.*(CELLAR|FAO)|single value|must be",
  fixed = FALSE
)

# -------- 2) Online path: CELLAR returns a data.frame -----------------------

if (at_home()) {
  prefix        <- "cn2022"
  conceptScheme <- "cn2022"
  
  res <- dataStructure(
    endpoint      = "CELLAR",
    prefix        = prefix,
    conceptScheme = conceptScheme,
    language      = "en",
    showQuery     = FALSE
  )
  
  expect_inherits(res, "data.frame")
  expect_true(is.numeric(nrow(res)))
} else {
  message("Skipping CELLAR online test: not at_home().")
}

# -------- 3) Online path: FAO returns a data.frame ---------------------------

if (at_home()) {
  prefix        <- "icc11"
  conceptScheme <- "scheme"
  
  res <- dataStructure(
    endpoint      = "FAO",
    prefix        = prefix,
    conceptScheme = conceptScheme,
    language      = "en",
    showQuery     = FALSE
  )
  
  expect_inherits(res, "data.frame")
  expect_true(is.numeric(nrow(res)))
} else {
  message("Skipping FAO online test: not at_home().")
}

# -------- 4) showQuery = TRUE: structure per 'return' -----------------------

if (at_home()) {
  prefix        <- "icc11"
  conceptScheme <- "scheme"
  
  # return = "summary"
  res_sum <- dataStructure(
    endpoint      = "FAO",
    prefix        = prefix,
    conceptScheme = conceptScheme,
    language      = "en",
    showQuery     = TRUE,
    return        = "summary"
  )
  expect_true(is.list(res_sum))
  expect_true(all(c("resolved", "query.summary", "dataStructure") %in% names(res_sum)))
  expect_true(is.character(res_sum[["query.summary"]]))
  expect_inherits(res_sum[["dataStructure"]], "data.frame")
  
  # return = "details"
  res_det <- dataStructure(
    endpoint      = "FAO",
    prefix        = prefix,
    conceptScheme = conceptScheme,
    language      = "en",
    showQuery     = TRUE,
    return        = "details"
  )
  expect_true(is.list(res_det))
  expect_true(all(c("resolved", "query.details", "details") %in% names(res_det)))
  expect_true(is.character(res_det[["query.details"]]))
  expect_inherits(res_det[["details"]], "data.frame")
  
  # return = "both"
  res_both <- dataStructure(
    endpoint      = "FAO",
    prefix        = prefix,
    conceptScheme = conceptScheme,
    language      = "en",
    showQuery     = TRUE,
    return        = "both"
  )
  expect_true(is.list(res_both))
  expect_true(all(c("resolved", "query.summary", "query.details", "summary", "details") %in% names(res_both)))
  expect_true(is.character(res_both[["query.summary"]]))
  expect_true(is.character(res_both[["query.details"]]))
  expect_inherits(res_both[["summary"]], "data.frame")
  expect_inherits(res_both[["details"]], "data.frame")
} else {
  message("Skipping showQuery structure tests: not at_home().")
}

# -------- 5) Unresolvable prefix/conceptScheme for FAO -----------------------

# This test does not need internet (it fails at registry resolution),
# but classificationList('FAO') must be available in the package.
expect_error(
  suppressWarnings(
    dataStructure(
      endpoint      = "FAO",
      prefix        = "XXX", 
      conceptScheme = "YYY",  
      language      = "en"
    )
  ),
  "(?i)Could not resolve the requested classification",
  fixed = FALSE
)
