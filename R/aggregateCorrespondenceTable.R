#' @title aggregateCorrespondenceTable aggregates correspondence tables to higher hierarchical levels
#' @description The `aggregateCorrespondenceTable` function is designed to aggregate correspondence tables between two hierarchical classifications A and B to higher hierarchical levels. This is particularly useful when correspondence information is needed at levels other than the most granular level. The function provides a 'mechanically defined' aggregation, offering users candidate aggregations for subsequent analysis by statistical classification experts.
#' @param AB a mandatory argument containing a correspondence table data frame with columns "Acode" and "Bcode" representing the correspondence between classifications A and B at the most granular level. This argument is mandatory
#' @param A a path to a CSV file containing source classification data with an Acode ALevel,ASuperior column. This argument is mandatory 
#' @param B a path to a CSV file containing target classification data with a Bcode Blevel BSuperior column. This argument is mandatory
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
#' result <- aggregateCorrespondenceTable(AB = AB, A = A, B = B, CSVout = NULL)
#' print(result)
#'
aggregateCorrespondenceTable <- function(AB, A, B, CSVout = NULL ) {
  
  ab_data <- testInputTable("Correspondence table (AB)", AB)
  a_data <- testInputTable("Source classification (A)", A)
  b_data <- testInputTable("Target classification (B)", B)
  
#Check if required number of columns are present in each input
check_n_columns(ab_data,"Correspondence table (AB)", 2)
check_n_columns(a_data, "Source classification (A)", 3)
check_n_columns(b_data,"Target classification (B)", 3)
  
  # Read the input correspondence table AB
  # ab_data <- read.csv2(AB, header = TRUE, sep =",")
  ColumnNames_ab <- colnames(ab_data)[1:2]
  colnames(ab_data)[1:2] = c("Acode","Bcode")
 
  # Check if there are any records 
  tryCatch(
    {
      if (nrow(ab_data) == 0) {
        stop("No valid records found in the input correspondence table AB.")
      }
    }, error = function(e) {
      message("Error in aggregateCorrespondenceTable: ",conditionMessage(e))
      stop(e)
    })
  
  # Find duplicated combinations of Acode and Bcode in AB
  duplicated_rows <- ab_data[duplicated(ab_data[c("Acode", "Bcode")]), c("Acode", "Bcode")]
  tryCatch(
    {
  # Check for duplicate combinations of Acode and Bcode
  if (nrow(duplicated_rows) > 0) {
    stop("Please remove duplicate(s) combinations of Acode and Bcode from the input correspondence table AB.")
          }
    }, error = function(e) {
      message("Error in aggregateCorrespondenceTable:",conditionMessage(e))
      print(duplicated_rows)
      stop(e)
    })
  
  
  # Filter rows where Acode or Bcode is missing in the AB data
  missing_code_rows <- ab_data[is.na(ab_data$Acode) | ab_data$Acode == "" | is.na(ab_data$Bcode) | ab_data$Bcode == "", ]
  tryCatch(
    {
  # Display problematic rows
  if (nrow(missing_code_rows) > 0) {
  stop(paste("Rows with missing values in the", ColumnNames_ab[1], "or", ColumnNames_ab[2], "column of the AB data:"))
    }
      }, error = function(e) {
        message("Error in aggregateCorrespondenceTable: ",conditionMessage(e))
        print(missing_code_rows)
        stop(e)
    })
   
  
  ######## 
  ####Read the source classification table A
 
  ColumnNames_a <- colnames(a_data)[1:3]
  colnames(a_data)[1:3] = c("Acode","Alevel","Asuperior")
  
  # Check if there are records in table A
  tryCatch({
    if (nrow(a_data) == 0) {
      stop("No valid records found in the input correspondence table A.")
    }
  }, error = function(e) {
    message("Error in aggregateCorrespondenceTable while processing input correspondence table A\n",conditionMessage(e))
    stop(e)
  })
  
  # Filter rows where there are NA or empty values in the Alevel column
  problematic_rows <- a_data[is.na(a_data$Alevel) | a_data$Alevel == "", ]
  
  # Display problematic rows
  if (nrow(problematic_rows) > 0) {
    print(paste("Rows with missing or empty values in the Alevel column:", ColumnNames_a[2]))
    print(problematic_rows)
    cat("\n")
  }
  
  # Check for duplicate Acode values in table A
  Aduplicated_rows <- a_data[duplicated(a_data$Acode), "Acode"]
  if (length(Aduplicated_rows) > 0) {
    message(paste("Duplicate(s) value(s) of Acode column named:", ColumnNames_a[1], "found in the input table A:"))
    print(Aduplicated_rows)
    stop(paste("Please remove duplicate(s) values of Acode column named:", ColumnNames_a[1], "in the input table A."))
  } else {
    # print("No duplicate(s) value(s) of Acode in the input table A.")
  }
  
  
  # Identify rows with text in Asuperior for level 1 records
  a_level_1_with_text <- a_data[a_data$Alevel == 1 & !is.na(a_data$Asuperior) & a_data$Asuperior != "", ]
  
  # Display rows with text in Asuperior for level 1 records
  if (nrow(a_level_1_with_text) > 0) {
    message(paste("In the source classification table, the following records at level 1 have text in the Asuperior column:", ColumnNames_a[3]))
    print(a_level_1_with_text)
    stop()
  }
  
  # Check if Asuperior is a character or blank for records at level 1
  tryCatch({
    if (!all((is.character(a_data$Asuperior) & a_data$Alevel != 1) | (is.na(a_data$Asuperior) & a_data$Alevel == 1))) {
      stop(paste("Asuperior column,", ColumnNames_a[3], "in the source classification table A must be blank for records at level 1."))
    }
  }, error = function(e) {
    stop(e)
  })
  
  # Initialize the variable to store the current level for A
  mostGranularA <- max(a_data$Alevel)
  currentLevelA <- mostGranularA
  
  # Loop to check hierarchy at each level for A
  while (currentLevelA >= 2) {
    # Select rows at the current level and the level below
    Ai <- a_data[a_data$Alevel == currentLevelA, ]
    AiMinus1 <- a_data[a_data$Alevel == (currentLevelA - 1), ]
    
    # Check if all values of Asuperior (in Ai) correspond to values of Acode (in AiMinus1)
    error_occurence <- which(!(Ai$Asuperior %in% AiMinus1$Acode))
    error_rows <- Ai[error_occurence,]
    if (length(error_occurence) > 0) {
      cat("Hierarchy error in A_data at level:", currentLevelA, "\n")
      cat("For the specified level, error at occurence:", error_occurence, "\n")
      cat("Offending row:\n")
      print(error_rows)
      stop("Hierarchy error detected in A_data.")
    } 
    
    # Check if all values of Acode (in AiMinus1) correspond to values of Asuperior (in Ai)
    error_occurence <- which(!(AiMinus1$Acode %in% Ai$Asuperior))
    error_rows <- AiMinus1[error_occurence,]
    if (length(error_occurence) > 0) {
      cat("Hierarchy error in A-data at level:", currentLevelA - 1, "\n")
      cat("For the specified level, error at occurence:", error_occurence, "\n")
      cat("Offending row:\n" )
      print(error_rows)
      stop("Hierarchy error detected in A_data.")
    }
    
    # Move to the next level
    currentLevelA <- currentLevelA - 1
  }
  
  
  # Read the target classification table B
  ColumnNames_b <- colnames(b_data)[1:3]
  colnames(b_data)[1:3] = c("Bcode","Blevel","Bsuperior")
  
  
  # Check if there are any records left in table B
  if (nrow(b_data) == 0) {
  stop("No valid records found in the input correspondence table B.")
  }
  
  # Filter rows where there are NA or empty values in the Blevel column
  problematic_rows <- b_data[is.na(b_data$Blevel) | b_data$Blevel == "", ]
  
  # Display problematic rows
  if (nrow(problematic_rows) > 0) {
    print(paste("Rows with missing or empty values in the Blevel column:", ColumnNames_b[2]))
    print(problematic_rows)
    cat("\n")
  }
  
  # Check for duplicate Bcode values in table B
  Bduplicated_rows <- b_data[duplicated(b_data$Bcode), "Bcode"]
  if (length(Bduplicated_rows) > 0) {
    message(paste("Duplicate(s) value(s) of Bcode column named:", ColumnNames_b[1], "found in the input table B :"))
    print(Bduplicated_rows)
    stop(paste("Please remove duplicate(s) value(s) of Bcode column named:", ColumnNames_b[1],"in the input table B ."))
  } else {
    # print("No duplicate(s) value(s) of Bcode in the input table B .")
  }
  
  
  # Identify rows with text in Bsuperior for level 1 records
  b_level_1_with_text <- b_data[b_data$Blevel == 1 & !is.na(b_data$Bsuperior) & b_data$Bsuperior != "", ]
  
  # Display rows with text in Bsuperior for level 1 records
  if (nrow(b_level_1_with_text) > 0) {
    message(paste("Bsuperior column,", ColumnNames_b[3], "in the target classification table B must be blank for records at level 1."))
    print(b_level_1_with_text)
    stop()
  }
  
  # Check if Bsuperior is a character or blank for records at level 1
  tryCatch({
    if (!all((is.character(b_data$Bsuperior) & b_data$Blevel != 1) | (is.na(b_data$Bsuperior) & b_data$Blevel == 1))) {
      stop(paste("Bsuperior column,", ColumnNames_b[3], "in the source classification table B must contain characters or be blank for records at level 1."))
    }
  }, error = function(e) {
    stop(e)
  })
  
  # Initialize the variable to store the current level
  mostGranularB <- max(b_data$Blevel)
  currentLevelB <- mostGranularB
  
  while (currentLevelB >= 2) {
    # Select rows at the current level and the level below
    Bi <- b_data[b_data$Blevel == currentLevelB, ]
    BiMinus1 <- b_data[b_data$Blevel == (currentLevelB - 1), ]
    
    # Check if all values of Bsuperior (in Bi) correspond to values of Bcode (in BiMinus1)
    error_occurence <- which(!(Bi$Bsuperior %in% BiMinus1$Bcode))
    error_rows <- Bi[error_occurence,]
    if (length(error_occurence) > 0) {
      cat("Hierarchy error in B_data at level:", currentLevelB, "\n")
      cat("For the specified level, error at occurence:", error_occurence, "\n")
      cat("Offending row:\n" )
      print(error_rows)
      stop("Hierarchy error detected in B_data.")
    } 
    
    # Check if all values of Bcode (in BiMinus1) correspond to values of Bsuperior (in Bi)
    error_occurence <- which(!(BiMinus1$Bcode %in% Bi$Bsuperior))
    error_rows <- BiMinus1[error_occurence,]
    if (length(error_occurence) > 0) {
      cat("Hierarchy error in B_data at level:", currentLevelB - 1, "\n")
      cat("For the specified level, error at occurence:", error_occurence, "\n")
      cat("Offending row:\n" )
      print(error_rows)
      stop("Hierarchy error detected in B_data.")
    }
    
    # Move to the next level
    currentLevelB <- currentLevelB - 1
  }
  
  # Uniqueness Check if Acode and Bcode in AB exist in A and B respectively
  if (!all(ab_data$Acode %in% a_data$Acode)) {
    offending_Acodes <- ab_data$Acode[!ab_data$Acode %in% a_data$Acode]
    tryCatch(
      stop(paste("Acode in the input correspondence table does not exist in source classification table. Offending Acodes:", paste(offending_Acodes, collapse = ", "))),
      error = function(e) {
        cat("Error:", e$message, "\n")
        stop(e)
      }
    )
  } else if (!all(ab_data$Bcode %in% b_data$Bcode)) {
    offending_Bcodes <- ab_data$Bcode[!ab_data$Bcode %in% b_data$Bcode]
    tryCatch(
      stop(paste("Bcode in the input correspondence table does not exist in target classification table. Offending Bcodes:", paste(offending_Bcodes, collapse = ", "))),
      error = function(e) {
        cat("Error:", e$message, "\n")
        stop(e)
      }
    )
  }
  
  
  
  
  ###3.4 Correct and complete correspondences 
  
  ###add additional the column because here you just add the code you need all the column
  AmostGranular <- subset(a_data, Alevel == max(Alevel), select = c(Acode, Asuperior))
  BmostGranular <- subset(b_data, Blevel == max(Blevel), select = c(Bcode,Bsuperior))
  
  AB_mostGranular <- merge(AmostGranular, BmostGranular, by.x = "Acode", by.y = "Bcode")
  
  if (!(all(AB_mostGranular$Acode %in% ab_data$Acode))) {
    offending_Acodes <- AB_mostGranular$Acode[!AB_mostGranular$Acode %in% ab_data$Acode]
    stop(paste("Acode in the most granular correspondence table does not exist in the input correspondence table. Offending Acodes:", paste(offending_Acodes, collapse = ", ")))
  }
  if (!(all(AB_mostGranular$Bcode %in% ab_data$Bcode))) {
    offending_Bcodes <- AB_mostGranular$Bcode[!AB_mostGranular$Bcode %in% ab_data$Bcode]
    stop(paste("Bcode in the most granular correspondence table does not exist in the input correspondence table. Offending Bcodes:", paste(offending_Bcodes, collapse = ", ")))
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
  resultA$granular <- resultA[[paste0("Acode", mostGranularA)]]
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
  resultB$granular <- resultB[[paste0("Bcode", mostGranularB)]]
  resultB$Bsuperior <- NULL
  
  
  # Merge resultA and resultB using the 'granular' column as the key
  Merged_resultA_AB <- merge(resultA, ab_data, by.x = "granular", by.y= "Acode", all= F)
  Merged_AB <- merge(Merged_resultA_AB, resultB, by.x= "Bcode", by.y= "granular", all=F)
  names(Merged_AB)[names(Merged_AB) == "Acode"] <- "Acode1"
  names(Merged_AB)[names(Merged_AB) == "Bcode.y"] <- "Bcode1"
  Merged_AB$Bcode <- NULL
  Merged_AB$granular <- NULL
  
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
  
  results_df <- suppressWarnings(as.data.frame(do.call(rbind, results_matrices)))
 
  
  # Extract levels from Acode and Bcode columns
  acode_levels <- gsub("^Acode(\\d+)$", "\\1", acode_columns)
  bcode_levels <- gsub("^Bcode(\\d+)$", "\\1", bcode_columns)
  max_levels <- max(length(acode_levels), length(bcode_levels))
  acode_levels <- rep(acode_levels, length.out = max_levels)
  bcode_levels <- rep(bcode_levels, length.out = max_levels)
  # Define the new column names
  new_colnames <- c(paste0(ColumnNames_ab[1]," level"),
                    paste0(ColumnNames_ab[2]," level"),
                    ColumnNames_ab[1],
                    ColumnNames_ab[2],
                    paste("N of", ColumnNames_ab[1], "level", acode_levels,  "values"),
                    paste("N of", ColumnNames_ab[2] , "level", bcode_levels , "values"))
  
  # Update column names in results_df
  colnames(results_df) <- new_colnames
  
  # Display Results
  
    # Using the testCsvParameter function to validate CSVout
    testCsvParameter("CSV", CSVout)
      
    CsvFileSave(CSVout, results_df)
    
return(results_df)  
}
