test_that("analyseCorrespondenceTable works with valid data and explicit A/B classifications", {
  AB <- system.file("extdata/test", "TestAnalyse_ExempleAnnexe.csv", package = "correspondenceTables")
  A <- system.file("extdata/test", "TestAnalyse_SourceClassification.csv", package = "correspondenceTables")
  B <- system.file("extdata/test", "TestAnalyse_TargetClassification.csv", package = "correspondenceTables")
  
  expect_true(file.exists(AB), info = paste("Missing file:", basename(AB)))
  expect_true(file.exists(A), info = paste("Missing file:", basename(A)))
  expect_true(file.exists(B), info = paste("Missing file:", basename(B)))
  
  expect_error({
    result <- analyseCorrespondenceTable(AB, A = A, B = B, longestAcodeOnly = FALSE, longestBcodeOnly = FALSE)
  }, NA)
  
  # Check that the output is a list with exactly the expected components
  expect_true(is.list(result))
  expect_equal(names(result), c("Inventory", "Analysis"))
   
  expect_s3_class(result$Inventory, "data.frame")
  expect_s3_class(result$Analysis, "data.frame")
  
  # Ensure both outputs are not empty
  expect_gt(nrow(result$Inventory), 0)
  
  # Check that Analysis has the same number of rows as AB when longestAcodeOnly = FALSE and longestBcodeOnly = FALSE
  AB_data <- read.csv(AB, encoding = "UTF-8")
  expect_equal(nrow(result$Analysis), nrow(AB_data))
  
  # Check that the expected columns are present in Analysis
  expected_cols_analysis <- c("Acode", "Bcode", "nTargetClasses", "SourceToTargetMapping",
                              "nSourceClasses", "TargetToSourceMapping")
  expect_true(all(expected_cols_analysis %in% colnames(result$Analysis)))
  
  # Check for a known correspondence pair
  expect_true(any(result$Analysis$Acode == "A1" & result$Analysis$Bcode == "B1"))
})


test_that("test_2 - analyseCorrespondenceTable emits warnings for unmatched codes in A and B", {
  AB <- system.file("extdata/test", "TestAnalyse_ExempleAnnexe.csv", package = "correspondenceTables")
  A_mismatch <- system.file("extdata/test", "TestAnalyse_SourceClassification_missing.csv", package = "correspondenceTables")
  B_mismatch <- system.file("extdata/test", "TestAnalyse_TargetClassification_missing.csv", package = "correspondenceTables")
  
  # Vérifie que les fichiers existent
  expect_true(file.exists(AB), info = "File AB not found")
  expect_true(file.exists(A_mismatch), info = "File A_mismatch not found")
  expect_true(file.exists(B_mismatch), info = "File B_mismatch not found")
  
  # Capture les warnings générés par les codes absents dans A et B
  warnings <- capture_warnings(
    analyseCorrespondenceTable(
      AB = AB,
      A = A_mismatch,
      B = B_mismatch,
      longestAcodeOnly = FALSE,
      longestBcodeOnly = FALSE
    )
  )
  
  # Vérifie qu'un warning est émis pour les codes sources non trouvés dans A
  expect_true(
    any(grepl("Unmatched source classification codes in A", warnings, fixed = TRUE)),
    info = "Expected a warning about unmatched source codes"
  )
  
  # Vérifie qu'un warning est émis pour les codes cibles non trouvés dans B
  expect_true(
    any(grepl("Unmatched target classification codes in B", warnings, fixed = TRUE)),
    info = "Expected a warning about unmatched target codes"
  )
})
test_that("test_3 - analyseCorrespondenceTable handles missing or empty AB file with clear messages", {
  empty_AB <- system.file("extdata/test", "TestAnalyse_empty.csv", package = "correspondenceTables")
  
  # Check that the empty AB file actually exists
  expect_true(file.exists(empty_AB), info = "Empty AB file is missing")
  
  # Case 1: Nonexistent AB file should trigger a specific error message
  expect_error(
    analyseCorrespondenceTable("nonexistent.csv"),
    regexp = "File not found: nonexistent.csv. Please check the path and filename of the correspondence table.",
    fixed = TRUE
  )
  
  # Case 2: Existing but empty AB file should trigger a different specific error
  expect_error(
    analyseCorrespondenceTable(empty_AB),
    regexp = "No valid records found in the input correspondence table AB",
    fixed = TRUE
  )
})

test_that("test_4 - analyseCorrespondenceTable correctly filters longestAcodeOnly and longestBcodeOnly", {
  AB <- system.file("extdata/test", "TestAnalyse_longest_AB.csv", package = "correspondenceTables")
  A <- system.file("extdata/test", "TestAnalyse_longest_A.csv", package = "correspondenceTables")
  B <- system.file("extdata/test", "TestAnalyse_longest_B.csv", package = "correspondenceTables")
  
  expect_true(file.exists(AB), info = "AB file is missing")
  expect_true(file.exists(A), info = "A file is missing")
  expect_true(file.exists(B), info = "B file is missing")
  
  df_A <- read.csv(A)
  df_B <- read.csv(B)
  
  max_len_A <- max(nchar(as.character(df_A$A)))
  max_len_B <- max(nchar(as.character(df_B$B)))
  
  # --- Longest Acode only ---
  res_A <- suppressWarnings(analyseCorrespondenceTable(AB, A = A, longestAcodeOnly = TRUE))
  expect_true(
    all(nchar(as.character(res_A$Inventory$Acode)) == max_len_A),
    info = "Some A codes are not of maximum length when longestAcodeOnly = TRUE"
  )
  
  # --- Longest Bcode only ---
  res_B <- suppressWarnings(analyseCorrespondenceTable(AB, B = B, longestBcodeOnly = TRUE))
  expect_true(
    all(nchar(as.character(res_B$Inventory$Bcode)) == max_len_B),
    info = "Some B codes are not of maximum length when longestBcodeOnly = TRUE"
  )
  
  # --- Both longest A and B codes ---
  res_both <- suppressWarnings(analyseCorrespondenceTable(
    AB, A = A, B = B,
    longestAcodeOnly = TRUE,
    longestBcodeOnly = TRUE
  ))
  expect_true(
    all(nchar(as.character(res_both$Inventory$Acode)) == max_len_A),
    info = "Some A codes in combined test are not of maximum length"
  )
  expect_true(
    all(nchar(as.character(res_both$Inventory$Bcode)) == max_len_B),
    info = "Some B codes in combined test are not of maximum length"
  )
})

test_that("test_5 - analyseCorrespondenceTable detects duplicates in AB, A, and B files", {
  # File with duplicates in AB
  AB_dup <- system.file("extdata/test", "ExempleAnnexe_duplicated.csv", package = "correspondenceTables")
  A <- system.file("extdata/test", "SourceClassification.csv", package = "correspondenceTables")
  B <- system.file("extdata/test", "TargetClassification.csv", package = "correspondenceTables")
  
  # File with duplicates in A
  A_dup <- system.file("extdata/test", "SourceClassification_duplicated.csv", package = "correspondenceTables")
  
  # File with duplicates in B
  B_dup <- system.file("extdata/test", "TargetClassification_duplicated.csv", package = "correspondenceTables")
  
  # Check for duplicates in AB
  expect_error(
    analyseCorrespondenceTable(AB_dup, A = A, B = B),
    regexp = "Duplicate[s]?",
    ignore.case = TRUE
  )
  
  # Check for duplicates in A
  expect_error(
    analyseCorrespondenceTable(AB_dup, A = A_dup, B = B),
    regexp = "Duplicate[s]?",
    ignore.case = TRUE
  )
  
  # Check for duplicates in B
  expect_error(
    analyseCorrespondenceTable(AB_dup, A = A, B = B_dup),
    regexp = "Duplicate[s]?",
    ignore.case = TRUE
  )
})


test_that("test_6 - analyseCorrespondenceTable handles invalid input types", {
  valid_AB <- system.file("extdata/test", "TestAnalyse_ExempleAnnexe.csv", package = "correspondenceTables")
  
  # Error if longestAcodeOnly is not a logical value
  expect_error(
    analyseCorrespondenceTable(
      AB = valid_AB,
      longestAcodeOnly = "YES"
    ),
    "Argument 'longestAcodeOnly' must be TRUE or FALSE"
  )
  
  # Error if longestBcodeOnly is not a logical value
  expect_error(
    analyseCorrespondenceTable(
      AB = valid_AB,
      longestBcodeOnly = 1
    ),
    "Argument 'longestBcodeOnly' must be TRUE or FALSE"
  )
})



test_that("test_7 - analyseCorrespondenceTable correctly filters with longestAcodeOnly and longestBcodeOnly (Inventory only)", {
  AB <- system.file("extdata/test", "TestAnalyse_longest_AB.csv", package = "correspondenceTables")
  A <- system.file("extdata/test", "TestAnalyse_longest_A.csv", package = "correspondenceTables")
  B <- system.file("extdata/test", "TestAnalyse_longest_B.csv", package = "correspondenceTables")
  
  # Filter by longest Acode only
  suppressWarnings({
    resultA <- analyseCorrespondenceTable(AB, A = A, longestAcodeOnly = TRUE)
    longest_A <- max(nchar(resultA$Inventory$Acode))
    expect_true(all(nchar(resultA$Inventory$Acode) == longest_A))
  })
  
  # Filter by longest Bcode only
  suppressWarnings({
    resultB <- analyseCorrespondenceTable(AB, B = B, longestBcodeOnly = TRUE)
    longest_B <- max(nchar(resultB$Inventory$Bcode))
    expect_true(all(nchar(resultB$Inventory$Bcode) == longest_B))
  })
  
  # Filter by both longest Acode and Bcode
  suppressWarnings({
    resultBoth <- analyseCorrespondenceTable(
      AB, A = A, B = B,
      longestAcodeOnly = TRUE,
      longestBcodeOnly = TRUE
    )
    expect_true(all(nchar(resultBoth$Inventory$Acode) == longest_A))
    expect_true(all(nchar(resultBoth$Inventory$Bcode) == longest_B))
  })
})
test_that("test_8 - output structure of Inventory and Analysis is correct", {
  AB <- system.file("extdata/test", "ExempleAnnexe.csv", package = "correspondenceTables")
  result <- analyseCorrespondenceTable(AB)
  
  # Check result is a list with correct names
  expect_true(is.list(result))
  expect_named(result, c("Inventory", "Analysis"))
  
  # Expected minimal columns
  expected_cols_inventory <- c("Component", "CorrespondenceType", "SourcePositions", "TargetPositions",
                               "nSourcePositions", "nTargetPositions")
  expected_cols_analysis <- c("ClassA", "ClassB", "nTargetClasses",
                              "SourceToTargetMapping", "nSourceClasses", "TargetToSourceMapping")
  
  # Inventory checks
  expect_s3_class(result$Inventory, "data.frame")
  expect_true(all(expected_cols_inventory %in% names(result$Inventory)))
  expect_type(result$Inventory$Component, "character")
  expect_type(result$Inventory$nSourcePositions, "integer")
  expect_type(result$Inventory$nTargetPositions, "integer")
  expect_type(result$Inventory$SourcePositions, "character")
  expect_type(result$Inventory$TargetPositions, "character")
  
  # Analysis checks
  expect_s3_class(result$Analysis, "data.frame")
  expect_true(all(expected_cols_analysis %in% names(result$Analysis)))
  expect_type(result$Analysis$ClassA, "character")
  expect_type(result$Analysis$ClassB, "character")
  expect_type(result$Analysis$nTargetClasses, "integer")
  expect_type(result$Analysis$nSourceClasses, "integer")
  expect_type(result$Analysis$SourceToTargetMapping, "character")
  expect_type(result$Analysis$TargetToSourceMapping, "character")
})


test_that("test_9 - unmatched codes are correctly detected and reported", {
  # This test checks that analyseCorrespondenceTable emits appropriate warnings
  # when some codes in A and B are not used in AB
  
  AB <- system.file("extdata/test", "TestAnalyse_ExempleAnnexe.csv", package = "correspondenceTables")
  A_missing <- system.file("extdata/test", "TestAnalyse_SourceClassification_missing.csv", package = "correspondenceTables")
  B_missing <- system.file("extdata/test", "TestAnalyse_TargetClassification_missing.csv", package = "correspondenceTables")
  
  warnings <- capture_warnings({
    analyseCorrespondenceTable(AB, A = A_missing, B = B_missing)
  })
  
  expect_true(
    any(grepl("Unmatched source classification codes in A", warnings, fixed = TRUE)),
    info = "Expected a warning about unmatched source classification codes in A"
  )
  
  expect_true(
    any(grepl("Unmatched target classification codes in B", warnings, fixed = TRUE)),
    info = "Expected a warning about unmatched target classification codes in B"
  )
})

