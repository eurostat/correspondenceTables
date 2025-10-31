# Test 1 - classificationQC() works with real NACE example
test_that("[1] classificationQC() works with real NACE example", {
  classification_file <- system.file("extdata", "Nace2.csv", package = "correspondenceTables")
  lengths_file <- system.file("extdata", "lenghtsNace.csv", package = "correspondenceTables")
  
  result <- classificationQC(
    classification = classification_file,
    lengths = lengths_file,
    fullHierarchy = TRUE,
    labelUniqueness = TRUE,
    labelHierarchy = TRUE,
    singleChildCode = NULL,
    sequencing = NULL,
    CSVout = NULL
  )
   
  expect_type(result, "list")
  expect_true("QC_output" %in% names(result))
  expect_s3_class(result$QC_output, "data.frame")
})

# Test 2 - classificationQC() throws an error if classification is not a csv file
test_that("[2] classificationQC() throws an error if classification is not a csv file", {
  classification <- "./inst/extdata/invalid_classification.txt"
  lengths_path <- system.file("extdata", "lenghtsNace.csv", package = "correspondenceTables")
  
  expect_error(
    classificationQC(
      classification = classification,
      lengths = lengths_path,
      fullHierarchy = FALSE,
      labelUniqueness = FALSE,
      labelHierarchy = FALSE
    ),
    regexp = "The classification should be provided as a csv file"
  )
})

# Test 3 - classificationQC() throws an error if lengths is NA
test_that("[3] classificationQC() throws an error if lengths is NA", {
  classification <- system.file("extdata", "Nace2.csv", package = "correspondenceTables")
  
  expect_error(
    classificationQC(
      classification = classification,
      lengths = NA,
      fullHierarchy = FALSE,
      labelUniqueness = FALSE,
      labelHierarchy = FALSE
    ),
    regexp = "must be a data frame or a path to a CSV file"
  )
})


# Test 4 - classificationQC() throws an error if lengths has wrong extension
test_that("[4] classificationQC() throws an error if lengths has wrong extension", {
  classification <- system.file("extdata", "Nace2.csv", package = "correspondenceTables")
  lengths_file <- system.file("extdata", "invalid_lengths.txt", package = "correspondenceTables")
  
  expect_error(
    classificationQC(
      classification = classification,
      lengths = lengths_file,
      fullHierarchy = FALSE,
      labelUniqueness = FALSE,
      labelHierarchy = FALSE
    ),
    regexp = "The provided file does not have a CSV extension"
  )
})




# Test 5 - classificationQC() throws an error if duplicate codes are present
test_that("[5] classificationQC() throws an error if duplicate codes are present", {
  classification <- system.file("extdata", "nace_duplicate_code.csv", package = "correspondenceTables")
  lengths <- system.file("extdata", "lenghtsNace.csv", package = "correspondenceTables")

  expect_error(
    classificationQC(
      classification = classification,
      lengths = lengths,
      fullHierarchy = FALSE,
      labelUniqueness = FALSE,
      labelHierarchy = FALSE
    ),
    regexp = "Codes in classification file must be unique"
  )
})
# Test 6 - classificationQC() throws an error if length segments overlap
test_that("[6] classificationQC() throws an error if length segments overlap", {
  classification_file <- system.file("extdata", "Nace2.csv", package = "correspondenceTables")
  lengths_file <- system.file("extdata", "nace_lengths_overlap.csv", package = "correspondenceTables")

  expect_error(
    classificationQC(
      classification = classification_file,
      lengths = lengths_file,
      fullHierarchy = FALSE,
      labelUniqueness = FALSE,
      labelHierarchy = FALSE
    ),
    regexp = "Sequences should not overlap"
  )
})

test_that("[7] classificationQC() warns on duplicated labels", {
  classification <- system.file("extdata", "nace_duplicate_label.csv", package = "correspondenceTables")
  lengths <- system.file("extdata", "lenghtsNace.csv", package = "correspondenceTables")
  
  expect_warning(
    classificationQC(
      classification = classification,
      lengths = lengths,
      fullHierarchy = FALSE,
      labelUniqueness = TRUE,
      labelHierarchy = FALSE
    ),
    regexp = "same labels.*QC_duplicatesLabel"
  )
})

# Test 8 - classificationQC() detects label hierarchy violations
test_that("[8] classificationQC() detects label hierarchy violations", {
  classification <- system.file("extdata", "nace_label_hierarchy_test.csv", package = "correspondenceTables")
  lengths <- system.file("extdata", "lenghtsNace.csv", package = "correspondenceTables")
  
  expect_warning(
    classificationQC(
      classification = classification,
      lengths = lengths,
      fullHierarchy = FALSE,
      labelUniqueness = FALSE,
      labelHierarchy = TRUE
    ),
    regexp = "same label as their parent.*QC_singleChildMismatch"
  )
})


# Test 9 - classificationQC() detects single child code rule violations
test_that("[9] classificationQC() detects single child code rule violations", {
  classification <- system.file("extdata", "test_singleChildCode_classification.csv", package = "correspondenceTables")
  lengths <- system.file("extdata", "lenghtsNace.csv", package = "correspondenceTables")
  singleChildCode <- system.file("extdata", "test_singleChildCode_rules.csv", package = "correspondenceTables")
  
  result <- suppressWarnings(classificationQC(
    classification = classification,
    lengths = lengths,
    fullHierarchy = FALSE,
    labelUniqueness = FALSE,
    labelHierarchy = FALSE,
    singleChildCode = singleChildCode
  ))
  
  expect_true("QC_singleCodeError" %in% names(result))
  expect_true("QC_multipleCodeError" %in% names(result))
  expect_gt(nrow(result$QC_singleCodeError), 0)
  expect_gt(nrow(result$QC_multipleCodeError), 0)
})


# 
# # Test 10 - classificationQC() detects sequencing rule violations
# test_that("[10] classificationQC() detects sequencing rule violations", {
#   classification <- system.file("extdata", "test_sequencing_classification.csv", package = "correspondenceTables")
#   lengths <- system.file("extdata", "lenghtsNace.csv", package = "correspondenceTables")
#   sequencing <- system.file("extdata", "test_sequencing_rules.csv", package = "correspondenceTables")
#   
#   result <- suppressWarnings(classificationQC(
#     classification = classification,
#     lengths = lengths,
#     fullHierarchy = FALSE,
#     labelUniqueness = FALSE,
#     labelHierarchy = FALSE,
#     sequencing = sequencing
#   ))
#   
#   expect_true("QC_gapBefore" %in% names(result))
#   expect_gt(nrow(result$QC_gapBefore), 0)
# })

test_that("[11] classificationQC() works with data.frames for classification and lengths", {
  classification_df <- data.frame(
    Code = c("A", "A01", "A02", "B", "B01"),
    Label = c("Agriculture", "Crops", "Animals", "Mining", "Coal")
  )
  
  lengths_df <- data.frame(
    charb = c(1, 2),
    chare = c(1, 3)
  )
  
  result <- classificationQC(
    classification = classification_df,
    lengths = lengths_df,
    fullHierarchy = TRUE,
    labelUniqueness = TRUE,
    labelHierarchy = TRUE,
    CSVout = NULL
  )
  
  expect_type(result, "list")
  expect_s3_class(result$QC_output, "data.frame")
  expect_true("level" %in% names(result$QC_output))
})

test_that("[12] classificationQC() works with data.frames for singleChildCode and sequencing", {
  classification_df <- data.frame(
    Code = c("A", "A01", "A02", "B", "B01"),
    Label = c("Agriculture", "Crops", "Animals", "Mining", "Coal")
  )
  
  lengths_df <- data.frame(
    charb = c(1, 2),
    chare = c(1, 3)
  )
  
  singleChildCode_df <- data.frame(
    level = 2,
    singleCode = "1",
    multipleCode = "2,3,4"
  )
  
  sequencing_df <- data.frame(
    level = 2,
    multipleCode = "1,2,3"
  )
  
  result <- suppressWarnings(classificationQC(
    classification = classification_df,
    lengths = lengths_df,
    singleChildCode = singleChildCode_df,
    sequencing = sequencing_df,
    fullHierarchy = TRUE,
    labelUniqueness = FALSE,
    labelHierarchy = FALSE
  ))
  
  expect_true("singleCodeError" %in% names(result$QC_output))
  expect_true("gapBefore" %in% names(result$QC_output))
})



test_that("[13] classificationQC() returns full expected structure with correct names and formats", {
  classification <- system.file("extdata", "Nace2.csv", package = "correspondenceTables")
  lengths <- system.file("extdata", "lenghtsNace.csv", package = "correspondenceTables")
  
  result <- suppressWarnings(classificationQC(
    classification = classification,
    lengths = lengths,
    fullHierarchy = TRUE,
    labelUniqueness = TRUE,
    labelHierarchy = TRUE
  ))
  
  expected_names <- c(
    "QC_output",
    "QC_noLevels",
    "QC_duplicatesCode",
    "QC_duplicatesLabel",
    "QC_orphan",
    "QC_childless",
    "QC_singleChildMismatch"
  )
  expect_equal(names(result), expected_names)
  
  lapply(result, function(x) expect_s3_class(x, "data.frame"))
  
  expect_true(all(c("Code", "Label", "level") %in% colnames(result$QC_output)))
  expect_true(all(c("Code", "Label", "level") %in% colnames(result$QC_orphan)))
  expect_true(all(c("Code", "duplicateCode") %in% colnames(result$QC_duplicatesCode)))
  expect_true(all(c("Code", "Label", "duplicateLabel") %in% colnames(result$QC_duplicatesLabel)))
  expect_true(all(c("Code", "Label", "level", "childless") %in% colnames(result$QC_childless)))
  expect_true(all(c("Code", "Label", "level", "singleChildMismatch") %in% colnames(result$QC_singleChildMismatch)))
})

