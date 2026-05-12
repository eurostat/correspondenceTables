
# --- 1) retrieveCorrespondenceTable returns a data.frame (basic sanity) ----

if(at_home()){
  {
    res_try <- try(
      retrieveCorrespondenceTable("FAO", "v21", "CPC21-CPC20"
      ),
      silent = TRUE
    )
    
    # If endpoint unreachable, skip gracefully
    if (inherits(res_try, "try-error")) {
      msg <- conditionMessage(attr(res_try, "condition"))
      message("Skipping test: SPARQL endpoint unreachable: ", msg)
    } else {
      res <- res_try
      expect_true(is.data.frame(res))
      expect_true(nrow(res) > 0)
    }
  }
} 

# --- 2) Reject invalid endpoint --------------------------------------------
{
  expect_error(
    retrieveCorrespondenceTable(
      prefix   = "cn2022",
      endpoint = "INVALID",
      ID_table = "CN2022_NST2007"
    ),
    "Unsupported endpoint"
  )
}

# --- 3) Error when Prefix and ID_table not available ---
{
  expect_error(retrieveCorrespondenceTable("FAO", prefix   = "other", ID_table = "OTHER_TABLE"))
}
