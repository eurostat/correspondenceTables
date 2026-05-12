############################################################
# [1] analyseCorrespondenceTable works with valid data and explicit A/B classifications
############################################################
{
  AB_path <- system.file("extdata/test", "TestAnalyse_ExempleAnnexe.csv", package = "correspondenceTables")
  A_path  <- system.file("extdata/test", "TestAnalyse_SourceClassification.csv", package = "correspondenceTables")
  B_path  <- system.file("extdata/test", "TestAnalyse_TargetClassification.csv", package = "correspondenceTables")
  
  AB_df <- read.csv(AB_path, encoding = "UTF-8", stringsAsFactors = FALSE)
  A_df  <- read.csv(A_path,  encoding = "UTF-8", stringsAsFactors = FALSE)
  B_df  <- read.csv(B_path,  encoding = "UTF-8", stringsAsFactors = FALSE)
  
  result <- analyseCorrespondenceTable(
    AB = AB_df, A = A_df, B = B_df,
    longestAcodeOnly = FALSE, longestBcodeOnly = FALSE
  )
  
  expect_true(is.list(result))
  expect_equal(names(result), c("Inventory", "Analysis"))
  
  expect_inherits(result$Inventory, "data.frame")
  expect_inherits(result$Analysis,  "data.frame")
  
  expect_true(nrow(result$Inventory) > 0)
  
  expect_equal(nrow(result$Analysis), nrow(AB_df))
  
  expected_cols_analysis <- c("Acode","Bcode","nTargetClasses","SourceToTargetMapping",
                              "nSourceClasses","TargetToSourceMapping")
  expect_true(all(expected_cols_analysis %in% colnames(result$Analysis)))
  
  expect_true(any(result$Analysis$Acode == "A1" & result$Analysis$Bcode == "B1"))
}

############################################################
# [2] emits warnings for unmatched codes in A and B
############################################################
{
  AB_path        <- system.file("extdata/test", "TestAnalyse_ExempleAnnexe.csv", package = "correspondenceTables")
  A_mismatch_path <- system.file("extdata/test", "TestAnalyse_SourceClassification_missing.csv", package = "correspondenceTables")
  B_mismatch_path <- system.file("extdata/test", "TestAnalyse_TargetClassification_missing.csv", package = "correspondenceTables")
  
  AB_df        <- read.csv(AB_path,         stringsAsFactors = FALSE)
  A_mismatchDF <- read.csv(A_mismatch_path, stringsAsFactors = FALSE)
  B_mismatchDF <- read.csv(B_mismatch_path, stringsAsFactors = FALSE)
  
  expect_warning(
    analyseCorrespondenceTable(
      AB = AB_df, A = A_mismatchDF, B = B_mismatchDF,
      longestAcodeOnly = FALSE, longestBcodeOnly = FALSE
    ),
    "Unmatched source classification codes in A"
  )
  
  expect_warning(
    analyseCorrespondenceTable(
      AB = AB_df, A = A_mismatchDF, B = B_mismatchDF,
      longestAcodeOnly = FALSE, longestBcodeOnly = FALSE
    ),
    "Unmatched target classification codes in B"
  )
}

############################################################
# [3] correctly filters longestAcodeOnly and longestBcodeOnly
############################################################
{
  AB_path <- system.file("extdata/test", "TestAnalyse_longest_AB.csv", package = "correspondenceTables")
  A_path  <- system.file("extdata/test", "TestAnalyse_longest_A.csv",  package = "correspondenceTables")
  B_path  <- system.file("extdata/test", "TestAnalyse_longest_B.csv",  package = "correspondenceTables")
  
  AB_df <- read.csv(AB_path, stringsAsFactors = FALSE)
  A_df  <- read.csv(A_path,  stringsAsFactors = FALSE)
  B_df  <- read.csv(B_path,  stringsAsFactors = FALSE)
  
  max_len_A <- max(nchar(as.character(A_df$A)))
  max_len_B <- max(nchar(as.character(B_df$B)))
  
  res_A <- suppressWarnings(analyseCorrespondenceTable(AB = AB_df, A = A_df, longestAcodeOnly = TRUE))
  expect_true(all(nchar(as.character(res_A$Inventory$Acode)) == max_len_A),
              info = "Some A codes are not of maximum length when longestAcodeOnly = TRUE")
  
  res_B <- suppressWarnings(analyseCorrespondenceTable(AB = AB_df, B = B_df, longestBcodeOnly = TRUE))
  expect_true(all(nchar(as.character(res_B$Inventory$Bcode)) == max_len_B),
              info = "Some B codes are not of maximum length when longestBcodeOnly = TRUE")
  
  res_both <- suppressWarnings(analyseCorrespondenceTable(
    AB = AB_df, A = A_df, B = B_df,
    longestAcodeOnly = TRUE, longestBcodeOnly = TRUE
  ))
  expect_true(all(nchar(as.character(res_both$Inventory$Acode)) == max_len_A),
              info = "Some A codes in combined test are not of maximum length")
  expect_true(all(nchar(as.character(res_both$Inventory$Bcode)) == max_len_B),
              info = "Some B codes in combined test are not of maximum length")
}

############################################################
# [4] detects duplicates in AB, A, and B (datasets loaded then passed as data.frames)
############################################################
{
  AB_dup_path <- system.file("extdata/test", "ExempleAnnexe_duplicated.csv", package = "correspondenceTables")
  A_path      <- system.file("extdata/test", "TestAnalyse_SourceClassification.csv", package = "correspondenceTables")
  B_path      <- system.file("extdata/test", "TestAnalyse_TargetClassification.csv", package = "correspondenceTables")
  
  A_dup_path  <- system.file("extdata/test", "SourceClassification_duplicated.csv", package = "correspondenceTables")
  B_dup_path  <- system.file("extdata/test", "TargetClassification_duplicated.csv", package = "correspondenceTables")
  
  AB_dup_df <- read.csv(AB_dup_path, stringsAsFactors = FALSE)
  A_df      <- read.csv(A_path,     stringsAsFactors = FALSE)
  B_df      <- read.csv(B_path,     stringsAsFactors = FALSE)
  
  A_dup_df  <- read.csv(A_dup_path, stringsAsFactors = FALSE)
  B_dup_df  <- read.csv(B_dup_path, stringsAsFactors = FALSE)
  
  expect_error(
    analyseCorrespondenceTable(AB = AB_dup_df, A = A_df, B = B_df),
    "Duplicate"
  )
  
  expect_error(
    analyseCorrespondenceTable(AB = AB_dup_df, A = A_dup_df, B = B_df),
    "Duplicate"
  )
  
  expect_error(
    analyseCorrespondenceTable(AB = AB_dup_df, A = A_df, B = B_dup_df),
    "Duplicate"
  )
}

############################################################
# [5] handles invalid input types (flags must be logical)
############################################################
{
  AB_path <- system.file("extdata/test", "TestAnalyse_ExempleAnnexe.csv", package = "correspondenceTables")
  AB_df   <- read.csv(AB_path, stringsAsFactors = FALSE)
  
  expect_error(
    analyseCorrespondenceTable(AB = AB_df, longestAcodeOnly = "YES"),
    "Argument 'longestAcodeOnly' must be TRUE or FALSE"
  )
  
  expect_error(
    analyseCorrespondenceTable(AB = AB_df, longestBcodeOnly = 1),
    "Argument 'longestBcodeOnly' must be TRUE or FALSE"
  )
}

############################################################
# [6] output structure of Inventory and Analysis is correct (loaded AB)
############################################################
{
  AB_path <- system.file("extdata/test", "ExempleAnnexe.csv", package = "correspondenceTables")
  AB_df   <- read.csv(AB_path, stringsAsFactors = FALSE)
  
  result <- analyseCorrespondenceTable(AB = AB_df)
  
  expect_true(is.list(result))
  expect_true( all(names(result)== c("Inventory", "Analysis")))
  
  expected_cols_inventory <- c("Component","CorrespondenceType","SourcePositions","TargetPositions",
                               "nSourcePositions","nTargetPositions")
  expected_cols_analysis  <- c("ClassA","ClassB","nTargetClasses",
                               "SourceToTargetMapping","nSourceClasses","TargetToSourceMapping")
  
  expect_inherits(result$Inventory, "data.frame")
  expect_true(all(expected_cols_inventory %in% names(result$Inventory)))
  
  expect_inherits(result$Analysis, "data.frame")
  expect_true(all(expected_cols_analysis %in% names(result$Analysis)))
  
  expect_true( class(result$Inventory$Component)==            "character")
  expect_true( class(result$Inventory$nSourcePositions) ==     "integer")
  expect_true( class(result$Inventory$nTargetPositions) ==     "integer")
  expect_true( class(result$Inventory$SourcePositions) ==      "character")
  expect_true( class(result$Inventory$TargetPositions) ==      "character")
  
  expect_true( class(result$Analysis$ClassA) ==                "character")
  expect_true( class(result$Analysis$ClassB) ==                "character")
  expect_true( class(result$Analysis$nTargetClasses) ==        "integer")
  expect_true( class(result$Analysis$nSourceClasses) ==        "integer")
  expect_true( class(result$Analysis$SourceToTargetMapping) == "character")
  expect_true( class(result$Analysis$TargetToSourceMapping) == "character")
}

############################################################
# [7] has AB, A, B as first arguments and runs on a simple in-memory example
############################################################
{
  
  
  args <- names(formals(analyseCorrespondenceTable))
  expect_identical(args, c("AB", "Aname", "Bname", "A", "B", "longestAcodeOnly", "longestBcodeOnly"))
  
  AB_df <- data.frame(
    Acode = c("A1","A2"),
    Bcode = c("B1","B2"),
    stringsAsFactors = FALSE
  )
  
  res <- analyseCorrespondenceTable(
    AB = AB_df, A = NULL, B = NULL,
    longestAcodeOnly = FALSE, longestBcodeOnly = FALSE
  )
  
  expect_true(is.list(res))
  expect_true(all(c("Inventory","Analysis") %in% names(res)))
}


# new parameters' tests!

############################################################
# [A1] Defaults: Aname/Bname NULL -> first two columns are used
############################################################
{
  AB_default <- data.frame(
    X = c("A1","A2","A2","A3"),
    Y = c("B1","B1","B2","B3"),
    Z = 1:4,
    stringsAsFactors = FALSE
  )
  
  # Case 1: Aname/Bname left NULL (first two columns used)
  res_null <- analyseCorrespondenceTable(
    AB = AB_default,
    Aname = NULL, Bname = NULL,
    A = NULL, B = NULL,
    longestAcodeOnly = FALSE, longestBcodeOnly = FALSE
  )
  
  # Case 2: Explicitly map the same columns via names
  res_named <- analyseCorrespondenceTable(
    AB = AB_default,
    Aname = "X", Bname = "Y",
    A = NULL, B = NULL,
    longestAcodeOnly = FALSE, longestBcodeOnly = FALSE
  )
  
  expect_true(is.list(res_null) && is.list(res_named))
  expect_true(all(c("Inventory","Analysis") %in% names(res_null)))
  expect_true(all(c("Inventory","Analysis") %in% names(res_named)))
  
  # Results should be equivalent modulo column order propagation
  expect_equal(res_null$Analysis[, c("X","Y")],
               res_named$Analysis[, c("X","Y")])
}

############################################################
# [A2] Works when A/B columns are not the first two (mapping by names)
############################################################
{
  AB_shuffled <- data.frame(
    extra1 = 101:104,
    Acode = c("A1","A2","A2","A21"),
    extra2 = letters[1:4],
    Bcode = c("B1","B1","B2","B2"),
    stringsAsFactors = FALSE
  )
  
  res <- analyseCorrespondenceTable(
    AB = AB_shuffled,
    Aname = "Acode", Bname = "Bcode",
    A = NULL, B = NULL,
    longestAcodeOnly = FALSE, longestBcodeOnly = FALSE
  )
  
  expect_true(is.list(res))
  expect_true(all(c("Inventory","Analysis") %in% names(res)))
  expect_inherits(res$Inventory, "data.frame")
  expect_inherits(res$Analysis,  "data.frame")
  expect_true(all(c("Acode","Bcode") %in% names(res$Analysis)))
  expect_true(nrow(res$Analysis) == nrow(AB_shuffled))
}

############################################################
# [A3] Error if only one of Aname/Bname is provided
############################################################
{
  AB_two <- data.frame(
    colA = c("A1","A2"),
    colB = c("B1","B2"),
    stringsAsFactors = FALSE
  )
  
  expect_error(
    analyseCorrespondenceTable(AB = AB_two, Aname = "colA", Bname = NULL),
    "Aname and Bname must be both NULL or both non-NULL"
  )
  
  expect_error(
    analyseCorrespondenceTable(AB = AB_two, Aname = NULL, Bname = "colB"),
    "Aname and Bname must be both NULL or both non-NULL"
  )
}

############################################################
# [A4] Error if Aname/Bname do not exist in AB
############################################################
{
  AB_cols <- data.frame(
    left  = c("A1","A2"),
    right = c("B1","B2"),
    stringsAsFactors = FALSE
  )
  
  expect_error(
    analyseCorrespondenceTable(AB = AB_cols, Aname = "foo", Bname = "right"),
    "Both Aname and Bname must correspond to existing columns in AB"
  )
  
  expect_error(
    analyseCorrespondenceTable(AB = AB_cols, Aname = "left", Bname = "bar"),
    "Both Aname and Bname must correspond to existing columns in AB"
  )
}

############################################################
# [A5] Type checks: Aname/Bname must be character scalars or NULL
############################################################
{
  AB_cols <- data.frame(
    left  = c("A1","A2"),
    right = c("B1","B2"),
    stringsAsFactors = FALSE
  )
  
  expect_error(
    analyseCorrespondenceTable(AB = AB_cols, Aname = 123, Bname = "right"),
    "Aname must be a character or NULL"
  )
  
  expect_error(
    analyseCorrespondenceTable(AB = AB_cols, Aname = "left", Bname = TRUE),
    "Bname must be a character or NULL"
  )
  
  expect_error(
    analyseCorrespondenceTable(AB = AB_cols, Aname = c("left","alt"), Bname = "right"),
    "Aname must be a character or NULL"
  )
  
  expect_error(
    analyseCorrespondenceTable(AB = AB_cols, Aname = "left", Bname = c("right","alt")),
    "Bname must be a character or NULL"
  )
}

############################################################
# [A6] Missing values in mapped columns should trigger the AB missing-code error
############################################################
{
  AB_with_na <- data.frame(
    src = c("A1", NA, "A2", ""),
    dst = c("B1", "B2", "",  "B3"),
    stringsAsFactors = FALSE
  )
  
  expect_error(
    analyseCorrespondenceTable(AB = AB_with_na, Aname = "src", Bname = "dst"),
    "Rows with missing values in the"
  )
}

############################################################
# [A7] Duplicate Acode-Bcode pairs detected after mapping by names
############################################################
{
  AB_dup <- data.frame(
    src = c("A1","A1"),
    dst = c("B1","B1"),
    stringsAsFactors = FALSE
  )
  
  expect_error(
    analyseCorrespondenceTable(AB = AB_dup, Aname = "src", Bname = "dst"),
    "Duplicate row\\(s\\) found in AB file"
  )
}

############################################################
# [A8] Result stability: explicit names vs default-first-two
############################################################
{
  AB_base <- data.frame(
    Acode = c("A1","A2","A21"),
    Bcode = c("B1","B2","B2"),
    extra = 1:3,
    stringsAsFactors = FALSE
  )
  
  # Default (first two columns)
  r_def <- analyseCorrespondenceTable(AB = AB_base)
  
  # Explicit naming
  r_exp <- analyseCorrespondenceTable(AB = AB_base, Aname = "Acode", Bname = "Bcode")
  
  expect_equal(r_def$Analysis[, c("Acode","Bcode")],
               r_exp$Analysis[, c("Acode","Bcode")])
}



############################################################
# [A9] Non acceptance of the same name
############################################################

expect_error(analyseCorrespondenceTable(AB = AB_base, Aname = "Acode", Bname = "Acode"),
             "Aname and Bname must refer to two different columns.")
