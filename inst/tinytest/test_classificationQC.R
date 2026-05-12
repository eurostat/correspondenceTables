## Helper to read extdata CSVs as data.frames (strings not factors)
read_extdata_csv <- function(fname) {
  utils::read.csv(
    system.file("extdata/test", fname, package = "correspondenceTables"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

############################################################
# [1] classificationQC() works with real NACE example
############################################################

{
  classification_df <- read_extdata_csv("NACE2.csv")
  lengths_df        <- data.frame(charb = c(1,2,3,5), chare = c(1,2,4,5))
  
  expect_warning(result <- classificationQC(
    classification  = classification_df,
    lengths         = lengths_df,
    fullHierarchy   = TRUE,
    labelUniqueness = TRUE,
    labelHierarchy  = TRUE,
    singleChildCode = NULL,
    sequencing      = NULL
  ))
  
  expect_inherits(result, "list")
  expect_true("QC_output" %in% names(result))
  expect_inherits(result$QC_output, "data.frame")
} 



############################################################
# [2] classification must be a data.frame (reject non-DF)
# (Replaces old "path must be csv" test)
############################################################
{
  lengths_df <- data.frame(charb = c(1,2,3,5), chare = c(1,2,4,5))
  
  expect_error(
    classificationQC(
      classification  = "not-a-data-frame",
      lengths         = lengths_df,
      fullHierarchy   = FALSE,
      labelUniqueness = FALSE,
      labelHierarchy  = FALSE
    ),
    "The classification must be a data frame\\."
  )
}


############################################################
# [3] lengths must be a data.frame (reject NA / non-DF)
# (Replaces old NA/wrong-extension tests)
############################################################
{
  classification_df <- read_extdata_csv("NACE2.csv")
  
  expect_error(
    classificationQC(
      classification  = classification_df,
      lengths         = NA,
      fullHierarchy   = FALSE,
      labelUniqueness = FALSE,
      labelHierarchy  = FALSE
    ),
    "The 'lengths' argument must be a data frame\\."
  )
  
  expect_error(
    classificationQC(
      classification  = classification_df,
      lengths         = "not-a-data-frame",
      fullHierarchy   = FALSE,
      labelUniqueness = FALSE,
      labelHierarchy  = FALSE
    ),
    "The 'lengths' argument must be a data frame\\."
  )
}

# Tests from 5 to 9 momentarily skipped as we are missing the datasets

############################################################
# [5] error on duplicate codes
############################################################
# {
#   classification_df <- read_extdata_csv("nace_duplicate_code.csv")
#   lengths_df        <- data.frame(charb = c(1,2,3,5), chare = c(1,2,4,5))
#   
#   expect_error(
#     classificationQC(
#       classification  = classification_df,
#       lengths         = lengths_df,
#       fullHierarchy   = FALSE,
#       labelUniqueness = FALSE,
#       labelHierarchy  = FALSE
#     ),
#     "Codes in classification file must be unique"
#   )
# }


############################################################
# [6] error on overlapping length segments
############################################################
# {
#   classification_df <- read_extdata_csv("NACE2.csv")
#   lengths_df        <- read_extdata_csv("nace_lengths_overlap.csv")
#   lengths_df        <- read.csv(system.file("extdata", "nace_lengths_overlap.csv", package = "correspondenceTables"))
#   
#   expect_error(
#     classificationQC(
#       classification  = classification_df,
#       lengths         = lengths_df,
#       fullHierarchy   = FALSE,
#       labelUniqueness = FALSE,
#       labelHierarchy  = FALSE
#     ),
#     "Sequences should not overlap in the lengths definition"
#   )
# }


############################################################
# [7] warn on duplicated labels at same level
############################################################
# {
#   classification_df <- read_extdata_csv("nace_duplicate_label.csv")
#   lengths_df        <- read_extdata_csv("lenghtsNace.csv")
#   lengths_df        <- data.frame(charb = c(1,2,3,5), chare = c(1,2,4,5))
#   
#   expect_warning(
#     classificationQC(
#       classification  = classification_df,
#       lengths         = lengths_df,
#       fullHierarchy   = FALSE,
#       labelUniqueness = TRUE,
#       labelHierarchy  = FALSE
#     ),
#     "same labels.*QC_duplicatesLabel"
#   )
# }


############################################################
# [8] warn on label hierarchy violations
############################################################
# {
#   classification_df <- read_extdata_csv("nace_label_hierarchy_test.csv")
#   lengths_df        <- read_extdata_csv("lenghtsNace.csv")
# lengths_df        <- data.frame(charb = c(1,2,3,5), chare = c(1,2,4,5))
#   
#   expect_warning(
#     classificationQC(
#       classification  = classification_df,
#       lengths         = lengths_df,
#       fullHierarchy   = FALSE,
#       labelUniqueness = FALSE,
#       labelHierarchy  = TRUE
#     ),
#     "same label as their parent.*QC_singleChildMismatch"
#   )
# }


############################################################
# [9] singleChildCode rule violations are reported
############################################################
# {
#   classification_df  <- read_extdata_csv("test_singleChildCode_classification.csv")
#   #lengths_df         <- read_extdata_csv("lenghtsNace.csv")
#   lengths_df        <- data.frame(charb = c(1,2,3,5), chare = c(1,2,4,5))
#   singleChildCode_df <- read_extdata_csv("test_singleChildCode_rules.csv")
#   
#   result <- suppressWarnings(
#     classificationQC(
#       classification  = classification_df,
#       lengths         = lengths_df,
#       fullHierarchy   = FALSE,
#       labelUniqueness = FALSE,
#       labelHierarchy  = FALSE,
#       singleChildCode = singleChildCode_df
#     )
#   )
#   
#   expect_true("QC_singleCodeError"   %in% names(result))
#   expect_true("QC_multipleCodeError" %in% names(result))
#   expect_true(nrow(result$QC_singleCodeError)   > 0)
#   expect_true(nrow(result$QC_multipleCodeError) > 0)
# }
# 

############################################################
# [10] Works with in-memory data.frames (minimal example)
############################################################
{
  classification_df <- data.frame(
    Code  = c("A", "A01", "A02", "B", "B01"),
    Label = c("Agriculture", "Crops", "Animals", "Mining", "Coal"),
    stringsAsFactors = FALSE
  )
  
  lengths_df <- data.frame(
    charb = c(1, 2),
    chare = c(1, 3)
  )
  
  expect_warning(
    result <- classificationQC(
      classification  = classification_df,
      lengths         = lengths_df,
      fullHierarchy   = TRUE,
      labelUniqueness = TRUE,
      labelHierarchy  = TRUE
    ) )
  
  expect_inherits(result, "list")
  expect_inherits(result$QC_output, "data.frame")
  expect_true(all(c("Code", "Label", "level") %in% names(result$QC_output)))
}


############################################################
# [11] Works with in-memory singleChildCode & sequencing
############################################################
{
  classification_df <- data.frame(
    Code  = c("A", "A01", "A02", "B", "B01"),
    Label = c("Agriculture", "Crops", "Animals", "Mining", "Coal"),
    stringsAsFactors = FALSE
  )
  
  lengths_df <- data.frame(
    charb = c(1, 2),
    chare = c(1, 3)
  )
  
  singleChildCode_df <- data.frame(
    level       = 2,
    singleCode  = "1",
    multipleCode = "2,3,4",
    stringsAsFactors = FALSE
  )
  
  sequencing_df <- data.frame(
    level        = 2,
    multipleCode = "1,2,3",
    stringsAsFactors = FALSE
  )
  
  result <- classificationQC(
    classification  = classification_df,
    lengths         = lengths_df,
    singleChildCode = singleChildCode_df,
    sequencing      = sequencing_df,
    fullHierarchy   = TRUE,
    labelUniqueness = FALSE,
    labelHierarchy  = FALSE
  )
  
  
  expect_true(all(c("singleCodeError", "gapBefore") %in% names(result$QC_output)))
}


############################################################
# [12] Structure sanity: always has QC_output; optional frames appear only if triggered
# (Safer than asserting an exact full list of names)
############################################################
{
  classification_df <- read_extdata_csv("NACE2.csv")
  lengths_df        <- data.frame(charb = c(1,2,3,5), chare = c(1,2,4,5))
  
  expect_warning(
    result <- classificationQC(
      classification  = classification_df,
      lengths         = lengths_df,
      fullHierarchy   = TRUE,
      labelUniqueness = TRUE,
      labelHierarchy  = TRUE
    )
  )
  
  expect_true("QC_output" %in% names(result))
  expect_inherits(result$QC_output, "data.frame")
  expect_true(all(c("Label", "level") %in% names(result$QC_output)))
  
  # Optional artifacts: if present, they must be data.frames with sensible columns
  optional <- intersect(
    names(result),
    c("QC_noLevels","QC_duplicatesCode","QC_duplicatesLabel",
      "QC_orphan","QC_childless","QC_singleChildMismatch",
      "QC_singleCodeError","QC_multipleCodeError",
      "QC_gapBefore","QC_lastSibling")
  )
  
  for (nm in optional) {
    expect_inherits(result[[nm]], "data.frame")
  }
}
