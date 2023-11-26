#' @title aggregateCorrespondenceTable aggregates correspondence tables to higher hierarchical levels
#' @description The `aggregateCorrespondenceTable` function is designed to aggregate correspondence tables between two hierarchical classifications A and B to higher hierarchical levels. This is particularly useful when correspondence information is needed at levels other than the most granular level. The function provides a 'mechanically defined' aggregation, offering users candidate aggregations for subsequent analysis by statistical classification experts.
#' @param AB a mandatory argument containing a correspondence table data frame with columns "Acode" and "Bcode" representing the correspondence between classifications A and B at the most granular level.
#' @param A a path to a CSV file containing source classification data with an "Acode" column.
#' @param B a path to a CSV file containing target classification data with a "Bcode" column.
#'
#' @param CSVout a character string providing the path where the aggregated correspondence table CSV file should be saved. If NULL, no CSV file is generated.
#'
#' @return A data frame representing the aggregated correspondence table.
#'
#' @export
#' @examples 
#' # Use data from the folder extdata
#' AB <- (system.file("extdata", "ab_data.csv", package = "correspondenceTables"))
#' A <- (system.file("extdata", "a_data.csv", package = "correspondenceTables"))
#' B <- (system.file("extdata", "b_data.csv", package = "correspondenceTables"))
#' 
#' 
#' result <- aggregateCorrespondenceTable(AB = AB, A = A, B = B, CSVout = FALSE)
#' print(result)
#'


AB <- "C:/Users/clement.thomas/Desktop/Rproject/CorrespondanceTable/Task4/CorrespondenceTable/ab_data.csv"
A <-  "C:/Users/clement.thomas/Desktop/Rproject/CorrespondanceTable/Task4/CorrespondenceTable/a_data.csv"
B <-  "C:/Users/clement.thomas/Desktop/Rproject/CorrespondanceTable/Task4/CorrespondenceTable/b_data.csv"


aggregateCorrespondenceTable <- function(AB, A, B, CSVout = NULL ) {
  # Check if input files exist
  if (!file.exists(AB) || !file.exists(A) || !file.exists(B)) {
    stop("One or more input files do not exist.")
  }
  # Check if the input files are csv
  if (!grepl("\\.csv$", AB) || !grepl("\\.csv$", A) || !grepl("\\.csv$", B)) {
    stop("One or more input files do not have a .csv extension.")
  }
  
  # Read the input correspondence table AB
  ab_data <- read.csv2(AB, header = TRUE, sep =",")
  ColumnNames <- colnames(ab_data)[1:2]
  colnames(ab_data)[1:2] = c("Acode","Bcode")
  # Check if AB table has the required columns
  if (!all(c("Acode", "Bcode") %in% colnames(ab_data))) {
    stop("Input correspondence table AB must have columns 'Acode' and 'Bcode'.")
  }
  
  # Check for duplicate combinations of Acode and Bcode in AB
  if (any(duplicated(ab_data[c("Acode", "Bcode")]))) {
    stop("Duplicate combinations of Acode and Bcode found in the input correspondence table AB.")
  }
  
  # Filter out records with missing Acode or Bcode
  ab_data <- ab_data[!is.na(ab_data$Acode) & !is.na(ab_data$Bcode), ]
  
  # Check if there are any records left
  if (nrow(ab_data) == 0) {
    stop("No valid records found in the input correspondence table AB.")
  }
  
  ######## 
  ####Read the source classification table A
  a_data <- read.csv(A, header = TRUE, sep = ",")
  colnames(a_data)[1:3] = c("Acode","Alevel","Asuperior")
  # Check if A table has the required columns
  if (!all(c("Acode", "Alevel", "Asuperior") %in% colnames(a_data))) {
    stop("Source classification table A must have columns 'Acode', 'Alevel', and 'Asuperior'.")
  }
  
  #Uniqueness 3.2 Check for duplicate Acode values in A
  if (any(duplicated(a_data$Acode))) {
    stop("Duplicate Acode values found in the source classification table A.")
  }
  
  # Check if Alevel is numeric
  if (!all(is.numeric(a_data$Alevel))) {
    stop("Alevel column in the source classification table A must contain numeric values.")
  }
  
  # Check if Asuperior is a character or blank
  if (!all(is.character(a_data$Asuperior) | a_data$Alevel == 1)) {
    stop("Asuperior column in the source classification table A must contain characters or be blank for records at level 1.")
  }
  
  # Initialize the variable to store the current level for A
  mostGranularA <- max(a_data$Alevel)
  currentLevelA <- mostGranularA
  
  # Loop to check hierarchy at each level for A
  while (currentLevelA >= 2) {
    # Select rows at the current level and the level below
    Ai <- a_data[a_data$Alevel == currentLevelA, ]
    AiMinus1 <- a_data[a_data$Alevel == (currentLevelA - 1), ]
    
    # Check if all values of Asuperior (in Ai) correspond to values of Acode (in AiMinus1)
    error_rows <- which(!(Ai$Asuperior %in% AiMinus1$Acode))
    if (length(error_rows) > 0) {
      cat("Hierarchy error in A-data at level:", currentLevelA, "\n")
      cat("Error occurred in rows:", error_rows, "\n")
      break  # Exit the loop if an error is detected
    }
    
    # Check if all values of Acode (in AiMinus1) correspond to values of Asuperior (in Ai)
    error_rows <- which(!(AiMinus1$Acode %in% Ai$Asuperior))
    if (length(error_rows) > 0) {
      cat("Hierarchy error in A-data at level:", currentLevelA - 1, "\n")
      cat("Error occurred in rows:", error_rows, "\n")
      break  # Exit the loop if an error is detected
    }
    
    # Move to the next level
    currentLevelA <- currentLevelA - 1
  }
  
  # Read the target classification table B
  b_data <- read.csv(B, header = TRUE, sep = ",")
  colnames(b_data)[1:3] = c("Bcode","Blevel","Bsuperior")
  # Check if B table has the required columns
  if (!all(c("Bcode", "Blevel", "Bsuperior") %in% colnames(b_data))) {
    stop("Target classification table B must have columns 'Bcode', 'Blevel', and 'Bsuperior'.")
  }
  
  #Uniqueness 3.2 Check for duplicate Bcode values in B
  if (any(duplicated(b_data$Bcode))) {
    stop("Duplicate Bcode values found in the target classification table B.")
  }
  
  # Check if Blevel is numeric in B
  if (!all(is.numeric(b_data$Blevel))) {
    stop("Blevel column in the target classification table B must contain numeric values.")
  }
  
  # Check if Bsuperior is a character or blank in B
  if (!all(is.character(b_data$Bsuperior) | b_data$Blevel == 1)) {
    stop("Bsuperior column in the target classification table B must contain characters or be blank for records at level 1.")
  }
  
  # Initialize the variable to store the current level
  mostGranularB <- max(b_data$Blevel)
  currentLevelB <- mostGranularB
  
  # Loop to check hierarchy at each level for B
  while (currentLevelB >= 2) {
    # Select rows at the current level and the level below
    Bi <- b_data[b_data$Blevel == currentLevelB, ]
    BiMinus1 <- b_data[b_data$Blevel == (currentLevelB - 1), ]
    
    # Check if all values of Bsuperior (in Bi) correspond to values of Bcode (in BiMinus1)
    error_rows <- which(!(Bi$Bsuperior %in% BiMinus1$Bcode))
    if (length(error_rows) > 0) {
      cat("Hierarchy error in B-data at level:", currentLevelB, "\n")
      cat("Error occurred in rows:", error_rows, "\n")
      break  # Exit the loop if an error is detected
    }
    
    # Check if all values of Bcode (in BiMinus1) correspond to values of Bsuperior (in Bi)
    error_rows <- which(!(BiMinus1$Bcode %in% Bi$Bsuperior))
    if (length(error_rows) > 0) {
      cat("Hierarchy error in B_data at level:", currentLevelB - 1, "\n")
      cat("Error occurred in rows:", error_rows, "\n")
      break  # Exit the loop if an error is detected
    }
    
    # Move to the next level
    currentLevelB <- currentLevelB - 1
  }
  
  #Uniqueness  Check if Acode and Bcode in AB exist in A and B respectively
  if (!all(ab_data$Acode %in% a_data$Acode) || !all(ab_data$Bcode %in% b_data$Bcode)) {
    stop("Acode or Bcode in the input correspondence table does not exist in source or target classification table.")
  }
  
 ###3.4 Correct and complete correspondences 
  
  ###add additional the column because here you just add the code you need all the column
  AmostGranular <- subset(a_data, Alevel == max(Alevel), select = c(Acode, Asuperior))
  BmostGranular <- subset(b_data, Blevel == max(Blevel), select = c(Bcode,Bsuperior))
  
  AB_mostGranular <- merge(AmostGranular, BmostGranular, by.x = "Acode", by.y = "Bcode")
  
  if (!(all(ab_data$Acode %in% AmostGranular$Acode) && all(ab_data$Bcode %in% BmostGranular$Bcode))) {
    stop("Acode or Bcode in the input correspondence table does not exist in source or target classification table.")
  }
  
  if (!(all(AB_mostGranular$Acode %in% ab_data$Acode) && all(AB_mostGranular$Bcode %in% ab_data$Bcode))) {
    stop("Acode or Bcode in the most granular correspondence table does not exist in the input correspondence table.")
  }
  ########## 4.1 Creation of the table and merge it. 
  
  # Create an empty list to store the levels
  A_levels <- list()
  
  # Loop through each level and subset the data
  for (i in 1:mostGranularA) {
    level_data <- subset(a_data, a_data$Alevel == i, select = c(Acode, Asuperior))
    A_levels[[i]] <- level_data
  }

  # Create an empty data frame to store the final result for A
  resultA <- data.frame()
  
  # Initialize the result with the most granular level for A
  resultA <- A_levels[[mostGranularA]]
  
  # Merge the tables hierarchically starting from the second most granular level for A
  for (i in (mostGranularA - 1):1) {
    level_data <- A_levels[[i]]
    
    # Merge with the result using Asuperior and Acode columns
    resultA <- merge(level_data, resultA, by.x = "Acode", by.y = "Asuperior", all.x = TRUE, all.y = TRUE, suffixes = c(paste0(".x", i), paste0(".y", i)))
    
    # Rename columns to reflect the hierarchy for A
    colnames(resultA)[colnames(resultA) == paste0("Acode.x", i)] <- paste0("Acode", i)
    colnames(resultA)[colnames(resultA) == paste0("Acode.y", i)] <- paste0("Acode", (i + 1))
  }
  
  # Result will contain the final aggregated correspondence table with hierarchical code columns for A
  resultA$test <- resultA[[paste0("Acode", mostGranularA)]]
  resultA$Asuperior <- NULL
  
  # Determine the most granular level dynamically
  mostGranularB <- max(b_data$Blevel)
  
  # Create an empty list to store the levels
  B_levels <- list()
  
  # Loop through each level and subset the data
  for (i in 1:mostGranularB) {
    level_data <- subset(b_data, b_data$Blevel == i, select = c(Bcode, Bsuperior))
    B_levels[[i]] <- level_data
  }
  
  # Create an empty data frame to store the final result for B
  resultB <- data.frame()
  
  # Initialize the result with the most granular level for B
  resultB <- B_levels[[mostGranularB]]
  
  # Merge the tables hierarchically starting from the second most granular level for B
  for (i in (mostGranularB - 1):1) {
    level_data_B <- B_levels[[i]]
    
    # Merge with the result using Bsuperior and Bcode columns
    resultB <- merge(level_data_B, resultB, by.x = "Bcode", by.y = "Bsuperior", all.x = TRUE, all.y = TRUE, suffixes = c(paste0(".x", i), paste0(".y", i)))
    
    # Rename columns to reflect the hierarchy for B
    colnames(resultB)[colnames(resultB) == paste0("Bcode.x", i)] <- paste0("Bcode", i)
    colnames(resultB)[colnames(resultB) == paste0("Bcode.y", i)] <- paste0("Bcode", (i + 1))
  }
  
  # Result will contain the final aggregated correspondence table with hierarchical code columns for B
  resultB$test <- resultB[[paste0("Bcode", mostGranularB)]]
  resultB$Bsuperior <- NULL
  
  
  # Merge resultA and resultB using the 'test' column as the key
  Merged_AB <- merge(resultA, resultB, by.x = "test", by.y = "test", all = F)
  Merged_AB$test <- NULL

  
  ##Table merged 
  final_result <-Merged_AB
  ###4.2 Pairwise matching
  
  # Identify Acode and Bcode Columns
  acode_columns <- grep("^Acode", colnames(final_result), value = TRUE)
  bcode_columns <- grep("^Bcode", colnames(final_result), value = TRUE)
  
  # Loop Through Acode and Bcode Columns
  results_matrices <- list()
  
  for (acode_column in acode_columns) {
    level_Acode <- match(acode_column, acode_columns)
    
    for (bcode_column in bcode_columns) {
      level_Bcode <- match(bcode_column, bcode_columns)
      
      unique_combinations <- unique(final_result[, c(acode_column, bcode_column)])
      
      for (i in 1:nrow(unique_combinations)) {
        combination <- unique_combinations[i, ]
        
        # Perform the equality comparison using subset
        subset_data <- final_result[final_result[[acode_column]] == combination[[acode_column]] & 
                                      final_result[[bcode_column]] == combination[[bcode_column]], ]
        
        # Count Unique Occurrences
        count_Acode <- sapply(acode_columns, function(col) length(unique(subset_data[[col]])))
        count_Bcode <- sapply(bcode_columns, function(col) length(unique(subset_data[[col]])))
        
        # Build Results Matrix
        result_matrix <- c(level_Acode, level_Bcode, combination[[acode_column]], combination[[bcode_column]], count_Acode, count_Bcode)
        results_matrices <- append(results_matrices, list(result_matrix))
      }
    }
  }
  
  # Convert Matrices to Dataframe
  results_df <- as.data.frame(do.call(rbind, results_matrices))
  
  # Extract levels from Acode and Bcode columns
  acode_levels <- gsub("^Acode(\\d+)$", "\\1", acode_columns)
  bcode_levels <- gsub("^Bcode(\\d+)$", "\\1", bcode_columns)
  max_levels <- max(length(acode_levels), length(bcode_levels))
  acode_levels <- rep(acode_levels, length.out = max_levels)
  bcode_levels <- rep(bcode_levels, length.out = max_levels)
  # Define the new column names
  new_colnames <- c(paste0(ColumnNames[1]," level"),
                    paste0(ColumnNames[2]," level"),
                    ColumnNames[1],
                    ColumnNames[2],
                    paste0("N of ", ColumnNames[1], acode_levels, " level values "),
                    paste0("N of ", ColumnNames[2] , bcode_levels , " level values "))
  
  # Update column names in results_df
  colnames(results_df) <- new_colnames
  # Display Results
  
  if (!is.null(CSVout)) {
    if (is.logical(CSVout) && CSVout == TRUE) {
      file_name <- paste0("AgrgregateCorrespondeceTable_", ColumnNames[1], " & ",ColumnNames[2], ".csv")
      path_file <- file.path(getwd(), file_name)
      write.csv(results_df, path_file, row.names = FALSE)
      message(paste0("The table was saved in ", getwd(), file_name))
    } else if (is.character(CSVout)) {
      write.csv(QC_output, CSVout, row.names = FALSE)
    }
  }
  return(results_df)
  
}

