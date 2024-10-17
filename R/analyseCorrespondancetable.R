#' @title analyseCorrespondenceTable performs analysis on correspondence tables
#' @description The `analyseCorrespondenceTable` function takes input correspondence tables (AB) and related data (A and B) to perform analysis and generate various statistics.
#' It checks the validity of the input data, identifies components, calculates correspondence types, and creates summary tables.
#' @param AB a mandatory argument containing a CSV file provide by the user contains the correspondence table data with columns "Acode" and "Bcode".
#' @param A  a path to a CSV file containing source classification data with "Acode" column.
#' @param longestAcodeOnly A Boolean argument to filter source classification data based on "Acode" retaining only the maximum length, thus the lowest level Acode .
#' @param B a path to a CSV file containing target classification data with "Bcode" column.
#' @param longestBcodeOnly A Boolean argument to filter source classification data based on "Bcode" retaining only the maximum length, thus the lowest level Bcode.
#' @param CSVcorrespondenceInventory The valid values are not NULL if the user put a path with a empty csv file it will return it with the correspondeceInventory or just a path with a csv file . By default no CSV is produce
#' @param CSVcorrespondenceAnalysis  Provide an output containing the correpondenceAnalysis. the user put a path a empty file it will return with correpondenceAnalysis. by default no CSV is produce
#' @importFrom igraph graph.data.frame decompose.graph
#' @import igraph
#'
#' @return A list containing two data frames: Inventory and Analysis.
#' The `CSVcorrespondenceInventory` contains statistics related to components, correspondence types, and source/target positions.
#' The `CSVcorrespondenceAnalysis` contains statistics for each class in the correspondence table.
#'
#' @export
#' @examples 
#' # Use data from the folder extdata
#' 
#'
#'   
#' 
#' 
#'
#'
#'
#' # Perform analysis
#' result <- analyseCorrespondenceTable(AB =system.file("extdata", "ExempleAnnexe.csv", package = "correspondenceTables"),A = NULL, longestAcodeOnly = FALSE, B = NULL, longestBcodeOnly = FALSE, CSVcorrespondenceInventory = NULL, CSVcorrespondenceAnalysis = NULL) 
#' print(result$Inventory)
#' print(result$Analysis)



analyseCorrespondenceTable <- function(AB, A = NULL, longestAcodeOnly = FALSE, B = NULL, longestBcodeOnly = FALSE,
                                       CSVcorrespondenceInventory = NULL, CSVcorrespondenceAnalysis = NULL) {
  
  # if (class(AB) == "character") {
  #   # If AB is a character string, assume it's a path to a CSV file
  #   input_file_path <- AB
  #   
  #   AB <- read.csv2(input_file_path, header = TRUE, sep = ",")
  # } else if (class(AB) == "data.frame") {} else {
  #   stop("Parameter AB must be a path to a CSV file")
  # }
    
  ab_data <- testInputTable("Correspondence table (AB)", AB)
  #ab_data[] <- lapply(ab_data, as.character)
  
  #Check if required number of columns are present in input
  #check_n_columns(ab_data,"Correspondence table (AB)", 2)
  
  ColumnNames_ab <- colnames(ab_data)[1:2]
  colnames(ab_data)[1:2] = c("Acode", "Bcode")
  unused_data_ab <- ab_data
  
  # # Check if AB file has required columns
  # if (!("Acode" %in% colnames(AB)) || !("Bcode" %in% colnames(AB))) {
  #   stop("The AB file must contain 'Acode' and 'Bcode' columns.")
  # }
  # # Filter out records with missing Acode or Bcode
  # AB <- AB[complete.cases(AB[c("Acode", "Bcode")]), ]
  
  # Check if there are any records
  tryCatch(
    {
      if (nrow(ab_data) == 0) {
        stop("No valid records found in the input correspondence table AB.")
      }
    }, error = function(e) {
      message("Error in analyseCorrespondenceTable: ",conditionMessage(e))
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
      message("Error in analyseCorrespondenceTable: ",conditionMessage(e))
      print(missing_code_rows)
    })
  
  # # Check if there are any valid records after filtering
  # if (nrow(AB) == 0) {
  #   stop("No valid records found in the AB file.")
  # }
  
  # Find duplicated combinations of Acode and Bcode in AB
  duplicate_pairs <- ab_data[duplicated(ab_data[c("Acode", "Bcode")]), c("Acode", "Bcode")]
  tryCatch(
    {
      # Check for duplicate combinations of Acode and Bcode
      if (nrow(duplicate_pairs) > 0) {
        stop("Please remove duplicate(s) combinations of Acode and Bcode found in AB file.")
      }
    }, error = function(e) {
      message("Error in analyseCorrespondenceTable:",conditionMessage(e))
      print(duplicate_pairs)
      stop(e)
    })
  
  # Filter AB data based on longestAcodeOnly and longestBcodeOnly, if specified
  
  if (longestAcodeOnly == TRUE | longestBcodeOnly == TRUE) {
    if (longestAcodeOnly == TRUE) {
      # Calculate the maximum length of Acode
      maxLengthA <- max(nchar(ab_data$Acode, type = "width"))
      # Filter rows where Acode has the maximum length
      longest_Acode <- ab_data$Acode[nchar(ab_data$Acode, type = "width") == maxLengthA ]
      if (length(longest_Acode) == nrow(ab_data)) {
        ab_data$Acode <- longest_Acode
      } else {
        ab_data$Acode <- c(longest_Acode, rep("", nrow(ab_data) - length(longest_Acode)))
      }
    }
    if (longestBcodeOnly == TRUE){
      # Calculate the maximum length of Bcode
      maxLengthB <- max(nchar(ab_data$Bcode, type = "width"))
      # Filter rows where Bcode has the maximum length
      longest_Bcode <- ab_data$Bcode[nchar(ab_data$Bcode, type = "width") == maxLengthB ]
      if (length(longest_Bcode) == nrow(ab_data)) {
        ab_data$Bcode <- longest_Bcode
      } else {
        ab_data$Bcode <- c(longest_Bcode, rep("", nrow(ab_data) - length(longest_Bcode)))
      }
    }
    
    # Check if there are any valid records after filtering
    if (nrow(ab_data) == 0) {
      stop("No valid records found in the AB file after applying longestAcodeOnly and/or longestBcodeOnly filters.")
    }
    if (length(ab_data$Acode) != length(ab_data$Bcode)) {
      stop("Invalid records found in the AB file after applying the longestAcodeOnly and/or longestBcodeOnly filters. Acode and Bcode have different number of rows")
    }
  }
  
  # Read A file if provided
  if (!is.null(A)) {
    # a_data <- read.csv(A, header = TRUE)
    
    a_data <- testInputTable("Source classification table (A)", A)
    
    ColumnNames_a <- colnames(A)[1:1]
    colnames(a_data)[1:1] = c("Acode")
    unused_data_a <- a_data
    
    # Check if there are any records
    tryCatch(
      {
        if (nrow(a_data) == 0) {
          stop("No valid records found in the input table ource classification table (A).")
        }
      }, error = function(e) {
        message("Error in analyseCorrespondenceTable: ",conditionMessage(e))
      })
    
    # Check uniqueness of A codes
    duplicate_a_codes <- duplicated(a_data$Acode) | duplicated(a_data$Acode, fromLast = TRUE)
    tryCatch(
      {
        if (nrow(duplicate_a_codes) > 0) {
          stop("Duplicate Acode(s) found in A file.")
        }
      },error = function(e) {
        message("Error in analyseCorrespondenceTable: ",conditionMessage(e))
        print(duplicate_a_codes)
      })
    
    # Filter out A records based on longestAcodeOnly, if specified
    if (longestAcodeOnly == TRUE) {
      # Calculate the maximum length of Acode
      maxLengthA <- max(nchar(a_data$Acode, type = "width"))
      
      # Filter rows where Acode has the maximum length
      longest_Acode <- a_data$Acode[nchar(a_data$Acode, type = "width") == maxLengthA ]
      if (length(longest_Acode) == nrow(a_data)) {
        a_data$Acode <- longest_Acode
      } else {
        a_data$Acode <- c(longest_Acode, rep("", nrow(a_data) - length(longest_Acode)))
      }
      
      # Check if there are any valid records after end position filtering
      tryCatch(
        {
          if (nrow(a_data) == 0) {
            stop("No valid records found in the A file after applying end position filter.")
          }
        },error = function(e) {
          message("Error in analyseCorrespondenceTable: ",conditionMessage(e))
          stop(e)
        })
    }
    
    # Find unmatched source classification codes for A
    unmatched_codes_A <- setdiff(a_data$Acode, ab_data$Acode)
    noCorrespondenceA <- a_data[a_data$Acode %in% unmatched_codes_A, ]
    noClassificationA <- ab_data[ab_data$Acode %in% unmatched_codes_A, ]
    
    # Check if all codes in A are covered
    if (nrow(noCorrespondenceA) > 0) {
      message("Warning:Number of unmatched source classification codes in A:\n")
      print(noCorrespondenceA)
    } 
    
    # Check if all codes in the correspondence table are covered
    if (nrow(noClassificationA) > 0) {
      message("Warning:Number of source classification codes in AB not found in A:\n")
      print(noClassificationA)
    }
  }
  
  # Read B file if provided
  if (!is.null(B)) {
    
    #b_data <- read.csv(B, header = TRUE)
    b_data <- testInputTable("Target classification table (B)", B)
    ColumnNames_b <- colnames(B)[1:1]
    colnames(b_data)[1:1] = c("Bcode")
    unused_data_b <- b_data
    
    # Check if there are any records
    tryCatch(
      {
        if (nrow(b_data) == 0) {
          stop("No valid records found in the input table ource classification table (B).")
        }
      }, error = function(e) {
        message("Error in analyseCorrespondenceTable: ",conditionMessage(e))
      })
    
    # Check uniqueness of B codes
    duplicate_b_codes <- duplicated(b_data$Bcode) | duplicated(b_data$Bcode, fromLast = TRUE)
    tryCatch(
      {
        if (nrow(duplicate_b_codes) > 0) {
          stop("Duplicate Bcode(s) found in B file.")
        }
      },error = function(e) {
        message("Error in analyseCorrespondenceTable: ",conditionMessage(e))
        print(duplicate_b_codes)
      })
    
    # Filter out B records based on longestBcodeOnly, if specified
    if (longestBcodeOnly == TRUE) {
      # Calculate the maximum length of Bcode
      maxLengthB <- max(nchar(b_data$Bcode, type = "width"))
      
      # Filter rows where Bcode has the maximum length
      longest_Bcode <- b_data$Bcode[nchar(b_data$Bcode, type = "width") == maxLengthB ]
      if (length(longest_Bcode) == nrow(b_data)) {
        b_data$Bcode <- longest_Bcode
      } else {
        b_data$Bcode <- c(longest_Bcode, rep("", nrow(b_data) - length(longest_Bcode)))
      }
      
      # Check if there are any valid records after end position filtering
      tryCatch(
        {
          if (nrow(b_data) == 0) {
            stop("No valid records found in the B file after applying end position filter.")
          }
        },error = function(e) {
          message("Error in analyseCorrespondenceTable: ",conditionMessage(e))
          stop(e)
        })
    }
    
    # Find unmatched source classification codes for B
    unmatched_codes_B <- setdiff(b_data$Bcode, ab_data$Bcode)
    noCorrespondenceB <- b_data[b_data$Bcode %in% unmatched_codes_B, ]
    noClassificationB <- ab_data[ab_data$Bcode %in% unmatched_codes_B, ]
    
    ## Check if all codes in B are covered
    if (nrow(noCorrespondenceB) > 0) {
      message("Warning:Number of unmatched source classification codes in B:\n")
      print(noCorrespondenceB)
    } 
    
    # Check if all codes in the correspondence table are covered
    if (nrow(noClassificationB) > 0) {
      message("Warning: Number of source classification codes in AB not found in B:\n")
      print(noClassificationB)
    } 
  }
  

    
  #bipartitePart
  # create the bipartite graph 
  g <- graph.data.frame(ab_data, directed = FALSE)
  
  # all composant
  components <- decompose.graph(g)
  
  # list of composant by code 
  component_codes <- lapply(components, function(comp) V(comp)$name)
  
  ab_data$component <- NA
  
  for (i in seq_along(component_codes)) {
    component <- component_codes[[i]]
    ab_data$component[ab_data$Acode %in% component] <- paste("Component", i)
  }
  ### Print AB to see the component column for the correspondenceTable between Source & Target.
  
  component_codes <- unlist(component_codes)
  
  # Creation of the table for each component 
  component_index <- 0
  component_stats <- lapply(components, function(comp) {
    component <- V(comp)$name
    n_unique_targets <- length(unique(ab_data[ab_data$Acode %in% component, "Bcode"]))
    
    correspondence_type <- if (n_unique_targets == 1) {
      if (length(unique(ab_data[ab_data$Bcode %in% component, "Acode"])) == 1) {
        "1:1"
      } else {
        "M:1"
      }
    } else {
      if (length(unique(ab_data[ab_data$Bcode %in% component, "Acode"])) == 1) {
        "1:M"
      } else {
        "M:M"
      }
    }
    
    source_positions <- unique(ab_data[ab_data$Acode %in% component, "Acode"])
    target_positions <- unique(ab_data[ab_data$Acode %in% component, "Bcode"])
    n_source_positions <- length(source_positions)
    n_target_positions <- length(target_positions)
    
    component_index <- component_index + 1
    component_name <- unique(ab_data[ab_data$Acode %in% component, "component"])
    
    list(
      Component = component_name,
      CorrespondenceType = correspondence_type,
      SourcePositions = source_positions,
      TargetPositions = target_positions,
      nSourcePositions = n_source_positions,
      nTargetPositions = n_target_positions
    )
  })
  
  # Creation of the new dataFrame
  result <- do.call(rbind, component_stats)
  
  # Conversion into a data frame
  Inventory <- as.data.frame(result)
  
  ## Creation of Annex B (Analysis)
  Analysis <- data.frame(
    ClassC = ab_data$Acode,
    ClassD = ab_data$Bcode,
    nTargetClasses = NA,
    SourceToTargetMapping = NA,
    nSourceClasses = NA,
    TargetToSourceMapping = NA
  )
  # Update nTargetClasses column
  Analysis$nTargetClasses <- sapply(Analysis$ClassC, function(c_code) {
    length(unique(Analysis[Analysis$ClassC == c_code, "ClassD"]))
  })
  
  # Update SourceToTargetMapping column
  Analysis$SourceToTargetMapping <- sapply(Analysis$ClassC, function(c_code) {
    paste(unique(Analysis[Analysis$ClassC == c_code, "ClassD"]), collapse = ", ")
  })
  
  # Update nSourceClasses column
  Analysis$nSourceClasses <- sapply(Analysis$ClassD, function(d_code) {
    length(unique(Analysis[Analysis$ClassD == d_code, "ClassC"]))
  })
  
  # Update TargetToSourceMapping column
  Analysis$TargetToSourceMapping <- sapply(Analysis$ClassD, function(d_code) {
    paste(unique(Analysis[Analysis$ClassD == d_code, "ClassC"]), collapse = ", ")
  })
  
  
  
  colnames(Analysis)[1:2] = c("Acode", "Bcode")
  # store annex on variable to make table 
  output_Inventory <- Inventory
  output_Analysis <- Analysis
 
  
  Inventory_df <- as.data.frame(output_Inventory)
  
  Inventory_df$Component <- as.character(Inventory_df$Component)
  Inventory_df$CorrespondenceType <- as.character(Inventory_df$CorrespondenceType)
  #Inventory_df$SourcePositions <- as.character(Inventory_df$SourcePositions)
  Inventory_df$SourcePositions <- sapply(Inventory_df$SourcePositions, function(x) paste(x, collapse = ", "))
  #Inventory_df$TargetPositions <- as.character(Inventory_df$TargetPositions)
  Inventory_df$TargetPositions <- sapply(Inventory_df$TargetPositions, function(x) paste(x, collapse = ", "))
  Inventory_df$nSourcePositions <- as.numeric(Inventory_df$nSourcePositions)
  Inventory_df$nTargetPositions <- as.numeric(Inventory_df$nTargetPositions)
  
  Analysis_df <- as.data.frame(output_Analysis)
  Analysis_df <- merge(Analysis_df, unused_data_ab, by = c("Acode", "Bcode"), all = F)
  
  if (!is.null(B)) {
    Analysis_df <- merge(Analysis_df, unused_data_b, by = "Bcode", all = F)
  }
 
  if (!is.null(A)) {
    Analysis_df <- merge(Analysis_df, unused_data_a, by = "Acode", all = F)
  }
 
  Analysis_df <- Analysis_df[, c("Acode", "Bcode", setdiff(names(Analysis_df), c("Acode", "Bcode")))]
  
   colnames(Analysis_df)[1:2] = ColumnNames_ab[1:2]
   base_file_name <- paste0("correspondence_analysis_", format(Sys.time(), "%Y%m%d%H%M%S"))

   CsvFileSave(CSVcorrespondenceInventory, Inventory_df)
   CsvFileSave(CSVcorrespondenceAnalysis, Analysis_df)

  # Output list of the two dataframes.
  output <- list(Inventory = Inventory_df, Analysis =  Analysis_df)
  
  return(output)
}




