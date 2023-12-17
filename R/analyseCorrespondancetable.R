#' @title analyseCorrespondenceTable performs analysis on correspondence tables
#' @description The `analyseCorrespondenceTable` function takes input correspondence tables (AB) and related data (A and B) to perform analysis and generate various statistics.
#' It checks the validity of the input data, identifies components, calculates correspondence types, and creates summary tables.
#' @param AB a mandatory argument containing a CSV file provide by the user contains the correspondence table data with columns "Acode" and "Bcode".
#' @param A  a path to a CSV file containing source classification data with "Acode" column.
#' @param formatA A regular expression pattern to filter source classification data based on "Acode" should contains start & end position.
#' @param B a path to a CSV file containing target classification data with "Bcode" column.
#' @param formatB A regular expression pattern to filter target classification data based on "Bcode" should contains start & end position.
#' @param CSVcorrespondenceInventory The valid values are not NULL if the user put a path with a empty csv file it will return it with the correspondeceInventory or just a path with a csv file . By default no CSV is produce
#' @param CSVcorrespondenceAnalysis  Provide an output containing the correpondenceAnalysis. the user put a path a empty file it will return with correpondenceAnalysis. by default no CSV is produce
#' @importFrom igraph graph.data.frame decompose.graph
#' @import igraph
#'
#' @return A list containing two data frames: Annexe_A and Annexe_B.
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
#' result <- analyseCorrespondenceTable(AB =system.file("extdata", "ExempleAnnexe.csv", package = "correspondenceTables"),A = NULL, formatA = NULL, B = NULL, formatB = NULL, CSVcorrespondenceInventory = NULL, CSVcorrespondenceAnalysis = NULL) 
#' print(result$Annexe_A)
#' print(result$Annexe_B)



analyseCorrespondenceTable <- function(AB, A = NULL, formatA = NULL, B = NULL, formatB = NULL,
                                       CSVcorrespondenceInventory = NULL, CSVcorrespondenceAnalysis = NULL) {
  
  if (class(AB) == "character") {
    # If AB is a character string, assume it's a path to a CSV file
    input_file_path <- AB
    
    AB <- read.csv2(input_file_path, header = TRUE, sep = ",")
  } else if (class(AB) == "data.frame") {} else {
    stop("Parameter AB must be a path to a CSV file")
  }
  ColumnNames <- colnames(AB)[1:2]
  colnames(AB)[1:2] = c("Acode", "Bcode")
  # Check if AB file has required columns
  if (!("Acode" %in% colnames(AB)) || !("Bcode" %in% colnames(AB))) {
    stop("The AB file must contain 'Acode' and 'Bcode' columns.")
  }
  
  # Filter out records with missing Acode or Bcode
  AB <- AB[complete.cases(AB[c("Acode", "Bcode")]), ]
  
  # Check if there are any valid records after filtering
  if (nrow(AB) == 0) {
    stop("No valid records found in the AB file.")
  }
  
  # Check uniqueness of code pairs in AB file
  duplicate_pairs <- duplicated(AB[c("Acode", "Bcode")]) | duplicated(AB[c("Acode", "Bcode")], fromLast = TRUE)
  if (any(duplicate_pairs)) {
    first_duplicate <- AB[duplicate_pairs, c("Acode", "Bcode")][1, ]
    stop(paste("Duplicate code pair found in AB file:", first_duplicate$Acode, "-", first_duplicate$Bcode))
  }
  
  # Read A file if provided
  if (!is.null(A)) {
    a_data <- read.csv(A, header = TRUE)
    colnames(a_data)[1:1] = c("Acode")
    
    # Check if A file has the required column
    if (!("Acode" %in% colnames(a_data))) {
      stop("The A file must contain 'Acode' column.")
    }
    
    # Filter out A records based on formatA, if specified
    if (!is.null(formatA)) {# Check if formatA is numeric vector with two elements
      if (!is.numeric(formatA) || length(formatA) != 2) {
        stop("formatA must be a numeric vector with two elements.")
      }
      
      # Convert Acode to character and filter based on end position
      a_data$Acode <- as.character(a_data$Acode)
      a_data <- a_data[nchar(a_data$Acode) == formatA[2], ]
      
      # Check if there are any valid records after end position filtering
      if (nrow(a_data) == 0) {
        stop("No valid records found in the A file after applying end position filter.")
      }
    }
    
    # Check uniqueness of A codes
    duplicate_a_codes <- duplicated(a_data$Acode) | duplicated(a_data$Acode, fromLast = TRUE)
    if (any(duplicate_a_codes)) {
      first_duplicate <- a_data[duplicate_a_codes, "Acode"][1]
      stop(paste("Duplicate Acode found in A file:", first_duplicate))
    }
    
    # Find unmatched source classification codes
    unmatched_codes_A <- setdiff(a_data$Acode, AB$Acode)
    noCorrespondenceA <- a_data[a_data$Acode %in% unmatched_codes_A, ]
    noClassificationA <- AB[AB$Acode %in% unmatched_codes_A, ]
    
    # Print the length of noCorrespondenceA or a message indicating all codes in A are covered
    if (nrow(noCorrespondenceA) > 0) {
      cat("Number of unmatched source classification codes in A:", nrow(noCorrespondenceA), "\n")
    } else {
      cat("All source classification codes in A are covered in the correspondence table.\n")
    }
    
    # Print the length of noClassificationA or a message indicating all codes in the correspondence table are covered
    if (nrow(noClassificationA) > 0) {
      cat("Number of source classification codes in AB not found in A:", nrow(noClassificationA), "\n")
    } else {
      cat("All source classification codes in the correspondence table are covered by A.\n")
    }
  }
  
  # Read B file if provided
  if (!is.null(B)) {
    b_data <- read.csv(B, header = TRUE)
    colnames(b_data)[1:1] = c("Bcode")
    
    # Check if B file has the required column
    if (!("Bcode" %in% colnames(b_data))) {
      stop("The B file must contain 'Bcode' column.")
    }
    
    # Filter out B records based on formatB, if specified
    if (!is.null(formatB)) {
      # Check if formatB is numeric vector with two elements
      if (!is.numeric(formatB) || length(formatB) != 2) {
        stop("formatB must be a numeric vector with two elements.")
      }
      
      # Convert Bcode to character and filter based on end position
      b_data$Bcode <- as.character(b_data$Bcode)
      b_data <- b_data[nchar(b_data$Bcode) == formatB[2], ]
      
      # Check if there are any valid records after end position filtering
      if (nrow(b_data) == 0) {
        stop("No valid records found in the B file after applying end position filter.")
      }
    }
    
    # Check uniqueness of B codes
    duplicate_b_codes <- duplicated(b_data$Bcode) | duplicated(b_data$Bcode, fromLast = TRUE)
    if (any(duplicate_b_codes)) {
      first_duplicate <- b_data[duplicate_b_codes, "Bcode"][1]
      stop(paste("Duplicate Bcode found in B file:", first_duplicate))
    }
    
    # Find unmatched source classification codes for B
    unmatched_codes_B <- setdiff(b_data$Bcode, AB$Bcode)
    noCorrespondenceB <- b_data[b_data$Bcode %in% unmatched_codes_B, ]
    noClassificationB <- AB[AB$Bcode %in% unmatched_codes_B, ]
    
    # Print the length of noCorrespondenceB or a message indicating all codes in B are covered
    if (nrow(noCorrespondenceB) > 0) {
      cat("Number of unmatched source classification codes in B:", nrow(noCorrespondenceB), "\n")
    } else {
      cat("All source classification codes in B are covered in the correspondence table.\n")
    }
    
    # Print the length of noClassificationB or a message indicating all codes in the correspondence table are covered
    if (nrow(noClassificationB) > 0) {
      cat("Number of source classification codes in AB not found in B:", nrow(noClassificationB), "\n")
    } else {
      cat("All source classification codes in the correspondence table are covered by B.\n")
    }
  }
  
  # Filter AB data based on formatA and formatB, if specified
  if (!is.null(formatA) && !is.null(formatB)) {
    AB$Acode <- as.character(AB$Acode)
    AB$Bcode <- as.character(AB$Bcode)
    AB <- AB[nchar(AB$Acode) == formatA & nchar(AB$Bcode) == formatB, ]
    
    # Check if there are any valid records after filtering
    if (nrow(AB) == 0) {
      stop("No valid records found in the AB file after applying formatA and formatB filters.")
    }
  }
  #bipartitePart
  # create the bipartite graph 
  g <- graph.data.frame(AB, directed = FALSE)
  
  # all composant
  components <- decompose.graph(g)
  
  # list of composant by code 
  component_codes <- lapply(components, function(comp) V(comp)$name)
  
  AB$component <- NA
  
  for (i in seq_along(component_codes)) {
    component <- component_codes[[i]]
    AB$component[AB$Acode %in% component] <- paste("Component", i)
  }
  ### Print AB to see the component column for the correspondenceTable between Source & Target.
  
  component_codes <- unlist(component_codes)
  
  # Creation of the table for each component 
  component_index <- 0
  component_stats <- lapply(components, function(comp) {
    component <- V(comp)$name
    n_unique_targets <- length(unique(AB[AB$Acode %in% component, "Bcode"]))
    
    correspondence_type <- if (n_unique_targets == 1) {
      if (length(unique(AB[AB$Bcode %in% component, "Acode"])) == 1) {
        "1:1"
      } else {
        "M:1"
      }
    } else {
      if (length(unique(AB[AB$Bcode %in% component, "Acode"])) == 1) {
        "1:M"
      } else {
        "M:M"
      }
    }
    
    source_positions <- unique(AB[AB$Acode %in% component, "Acode"])
    target_positions <- unique(AB[AB$Acode %in% component, "Bcode"])
    n_source_positions <- length(source_positions)
    n_target_positions <- length(target_positions)
    
    component_index <- component_index + 1
    component_name <- unique(AB[AB$Acode %in% component, "component"])
    
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
  Annexe_A <- as.data.frame(result)
  
  ## Creation of Annex B
  Annexe_B <- data.frame(
    ClassC = AB$Acode,
    ClassD = AB$Bcode,
    nTargetClasses = NA,
    SourceToTargetMapping = NA,
    nSourceClasses = NA,
    TargetToSourceMapping = NA
  )
  # Update nTargetClasses column
  Annexe_B$nTargetClasses <- sapply(Annexe_B$ClassC, function(c_code) {
    length(unique(Annexe_B[Annexe_B$ClassC == c_code, "ClassD"]))
  })
  
  # Update SourceToTargetMapping column
  Annexe_B$SourceToTargetMapping <- sapply(Annexe_B$ClassC, function(c_code) {
    paste(unique(Annexe_B[Annexe_B$ClassC == c_code, "ClassD"]), collapse = ", ")
  })
  
  # Update nSourceClasses column
  Annexe_B$nSourceClasses <- sapply(Annexe_B$ClassD, function(d_code) {
    length(unique(Annexe_B[Annexe_B$ClassD == d_code, "ClassC"]))
  })
  
  # Update TargetToSourceMapping column
  Annexe_B$TargetToSourceMapping <- sapply(Annexe_B$ClassD, function(d_code) {
    paste(unique(Annexe_B[Annexe_B$ClassD == d_code, "ClassC"]), collapse = ", ")
  })
  
  colnames(Annexe_B)[1:2] = ColumnNames[1:2]
  # store annex on variable to make table 
  output_annex_A <- Annexe_A
  output_annex_B <- Annexe_B
  
  annex_A_df <- as.data.frame(output_annex_A)
  annex_A_df$Component <- as.character(annex_A_df$Component)
  annex_A_df$CorrespondenceType <- as.character(annex_A_df$CorrespondenceType)
  annex_A_df$SourcePositions <- as.character(annex_A_df$SourcePositions)
  annex_A_df$TargetPositions <- as.character(annex_A_df$TargetPositions)
  annex_A_df$nSourcePositions <- as.numeric(annex_A_df$nSourcePositions)
  annex_A_df$nTargetPositions <- as.numeric(annex_A_df$nTargetPositions)
  annex_B_df <- as.data.frame(output_annex_B)
  
  # Take the user's CSV file name to create CSV files
  if (!is.null(input_file_path)) {
    base_file_name <- tools::file_path_sans_ext(tools::file_path_sans_ext(basename(input_file_path)))
  } else {
    # Generate a unique base file name (e.g., based on the timestamp)
    base_file_name <- paste0("correspondence_analysis_", format(Sys.time(), "%Y%m%d%H%M%S"))
  }
 
  
  if (!is.null(CSVcorrespondenceInventory)) {
    if (is.character(CSVcorrespondenceInventory)) {
      chemin_inventaire <- CSVcorrespondenceInventory
    } else {
      # Generate a file name based on the name of the first column, "correspondence", and the date
      chemin_inventaire <- paste0("Correspondence_inventory_",ColumnNames[1], "_", ColumnNames[2],  ".csv")
    }
    write.csv(annex_A_df, chemin_inventaire, row.names = FALSE)
    message(paste0("The table was saved in ", getwd(), chemin_inventaire))
  }
  
  if (!is.null(CSVcorrespondenceAnalysis)) {
    if (is.character(CSVcorrespondenceAnalysis)) {
      # If it's a valid file path, use it
      chemin_analyse <- CSVcorrespondenceAnalysis
    } else {
      # Generate a file name based on the name of the first column, "correspondence", and the date
      chemin_analyse <- paste0("Correspondence_analysis_", ColumnNames[1], "_", ColumnNames[2],  ".csv")
    }
    write.csv(annex_B_df, chemin_analyse, row.names = FALSE)
    message(paste0("The table was saved in ", getwd(), chemin_analyse))
  }
  
  # Output list of the two dataframes.
  output <- list(Annexe_A = output_annex_A, Annexe_B = output_annex_B)
  
  return(output)
  
}




