
# --- 1) Wrong input: AB dataset with wrong column names ------------------
{
  AB_df <- read.csv(system.file("extdata/test", "ab_data.csv",
                                package = "correspondenceTables"),
                    stringsAsFactors = FALSE, check.names = FALSE)
  A_df  <- read.csv(system.file("extdata/test", "a_data.csv",
                                package = "correspondenceTables"),
                    stringsAsFactors = FALSE, check.names = FALSE)
  B_df  <- read.csv(system.file("extdata/test", "b_data.csv",
                                package = "correspondenceTables"),
                    stringsAsFactors = FALSE, check.names = FALSE)
  
  expect_error(aggregateCorrespondenceTable(AB_df, A_df, B_df))
}

# --- 2) Duplicate AB data: no error; weights normalized per code_A ----------
{
  AB_df <- read.csv(system.file("extdata/test", "ab_data_duplicate.csv",
                                package = "correspondenceTables"),
                    stringsAsFactors = FALSE, check.names = FALSE, col.names = c('from_code','to_code') )
  A_df  <- read.csv(system.file("extdata/test", "a_data.csv",
                                package = "correspondenceTables"),
                    stringsAsFactors = FALSE, check.names = FALSE)
  B_df  <- read.csv(system.file("extdata/test", "b_data.csv",
                                package = "correspondenceTables"),
                    stringsAsFactors = FALSE, check.names = FALSE)
  
  res_try <- try(aggregateCorrespondenceTable(AB_df, A_df, B_df), silent = TRUE)
  expect_false(inherits(res_try, "try-error"))
  
  ACT <- res_try
  expect_true(is.list(ACT))
  expect_true(is.data.frame(ACT$mapping))
  expect_true(all(c("code_A", "code_B", "weight") %in% names(ACT$mapping)))
  
  # Sum of weights per code_A must be (approximately) 1
  wsum <- aggregate(ACT$mapping$weight,
                    by = list(ACT$mapping$code_A),
                    FUN = function(z) sum(z, na.rm = TRUE))
  names(wsum) <- c("code_A", "sum_w")
  expect_true(all(abs(wsum$sum_w - 1) < 1e-12))
}

# --- 3) Empty AB data: expect error ----------------------------------------
{
  AB_df <- read.csv(system.file("extdata/test", "ab_data_empty.csv",
                                package = "correspondenceTables"),
                    stringsAsFactors = FALSE, check.names = FALSE, col.names = c('from_code','to_code') )
  A_df  <- read.csv(system.file("extdata/test", "a_data.csv",
                                package = "correspondenceTables"),
                    stringsAsFactors = FALSE, check.names = FALSE)
  B_df  <- read.csv(system.file("extdata/test", "b_data.csv",
                                package = "correspondenceTables"),
                    stringsAsFactors = FALSE, check.names = FALSE)
  
  expect_error(aggregateCorrespondenceTable(AB_df, A_df, B_df))
}

# --- 4) Missing code columns in AB: expect no error, the function removes it ---------------------------
{
  AB_df <- read.csv(system.file("extdata/test", "ab_data_missingcode.csv",
                                package = "correspondenceTables"),
                    stringsAsFactors = FALSE, check.names = FALSE, col.names = c('from_code','to_code') )
  is.na(AB_df)
  A_df  <- read.csv(system.file("extdata/test", "a_data.csv",
                                package = "correspondenceTables"),
                    stringsAsFactors = FALSE, check.names = FALSE)
  B_df  <- read.csv(system.file("extdata/test", "b_data.csv",
                                package = "correspondenceTables"),
                    stringsAsFactors = FALSE, check.names = FALSE)
  
  res_try <- try(aggregateCorrespondenceTable(AB_df, A_df, B_df), silent = TRUE)
  expect_false(inherits(res_try, "try-error"))
}

# --- 5) Text suppression / non-numeric A$value: expect no error, the function saves the unmatched values ---------------
{
  AB_df <- read.csv(system.file("extdata/test", "ab_data.csv",
                                package = "correspondenceTables"),
                    stringsAsFactors = FALSE, check.names = FALSE, col.names = c('from_code','to_code'))
  A_df  <- read.csv(system.file("extdata/test", "a_data_textsup.csv",
                                package = "correspondenceTables"),
                    stringsAsFactors = FALSE, check.names = FALSE)
  B_df  <- read.csv(system.file("extdata/test", "b_data.csv",
                                package = "correspondenceTables"),
                    stringsAsFactors = FALSE, check.names = FALSE)
  
  res_try <- try(aggregateCorrespondenceTable(AB_df, A_df, B_df), silent = TRUE)
  expect_false(inherits(res_try, "try-error"))
}

# --- 6) Return structure: result/mapping/diagnostics present ----------------
{
  AB_df <- read.csv(system.file("extdata/test", "ab_data.csv",
                                package = "correspondenceTables"),
                    stringsAsFactors = FALSE, check.names = FALSE, col.names = c('from_code','to_code'))
  A_df  <- read.csv(system.file("extdata/test", "a_data.csv",
                                package = "correspondenceTables"),
                    stringsAsFactors = FALSE, check.names = FALSE)
  B_df  <- read.csv(system.file("extdata/test", "b_data.csv",
                                package = "correspondenceTables"),
                    stringsAsFactors = FALSE, check.names = FALSE)
  
  ACT <- aggregateCorrespondenceTable(AB_df, A_df, B_df)
  
  expect_true(is.list(ACT))
  expect_true(is.data.frame(ACT$result))
  expect_true(is.data.frame(ACT$mapping))
  expect_true(is.list(ACT$diagnostics))
  
  expect_true(all(c("total_value_in_A",
                    "mapped_value_to_B",
                    "mapping_coverage_ratio",
                    "unmapped_A_codes") %in% names(ACT$diagnostics)))
}

# --- 7) Alignment to B domain: unmapped B codes get 0 ----------------------
{
  A_df <- data.frame(code = c("A1","A2"),
                     value = c(100, 50),
                     stringsAsFactors = FALSE)
  B_df <- data.frame(code = c("B1","B2"),
                     label = c("one","two"),
                     stringsAsFactors = FALSE)
  AB_df <- data.frame(from_code = "A2",
                      to_code   = "B2",
                      stringsAsFactors = FALSE)
  
  ACT <- aggregateCorrespondenceTable(AB_df, A_df, B_df)
  
  out <- ACT$result
  expect_true(all(c("code_B","label","value") %in% names(out)))
  expect_equal(out$value[out$code_B == "B1"], 0)
  expect_equal(out$value[out$code_B == "B2"], 50)
}

