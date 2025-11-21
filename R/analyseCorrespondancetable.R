#' @title Perform analysis on correspondence tables
#' @description Perform analysis on a correspondence table using its associated classifications to generate various statistics.
#' It checks the validity of the input data, identifies components, calculates correspondence types, and creates summary tables.
#' @param AB A mandatory argument containing a CSV file provide by the user contains the correspondence table data with columns "Acode" and "Bcode".
#' @param A  A path to a CSV file containing source classification data with "Acode" column.
#' @param longestAcodeOnly A Boolean argument to filter source classification data based on "Acode" retaining only the maximum length, thus the lowest level Acode .
#' @param B A path to a CSV file containing target classification data with "Bcode" column.
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
#' result <- analyseCorrespondenceTable(
#'   AB = system.file("extdata", "ExempleAnnexe.csv", package = "correspondenceTables"),
#'   A = NULL,
#'   longestAcodeOnly = FALSE,
#'   B = NULL,
#'   longestBcodeOnly = FALSE,
#'   CSVcorrespondenceInventory = NULL,
#'   CSVcorrespondenceAnalysis = NULL
#' )
#' print(result$Inventory)
#' print(result$Analysis)




analyseCorrespondenceTable <- function(AB, A = NULL, longestAcodeOnly = FALSE, B = NULL, longestBcodeOnly = FALSE,
                                       CSVcorrespondenceInventory = NULL, CSVcorrespondenceAnalysis = NULL) {
  
  if (!is.logical(longestAcodeOnly)) {
    stop("Argument 'longestAcodeOnly' must be TRUE or FALSE (logical).")
  }
  if (!is.logical(longestBcodeOnly)) {
    stop("Argument 'longestBcodeOnly' must be TRUE or FALSE (logical).")
  }
  # If AB is provided as a path to a file, check that the file exists
  if (is.character(AB) && !file.exists(AB)) {
    stop(paste0("File not found: ", AB, ". Please check the path and filename of the correspondence table."))
  }
  ab_data <- testInputTable("Correspondence table (AB)", AB)
  
  ColumnNames_ab <- colnames(ab_data)[1:2]
  colnames(ab_data)[1:2] <- c("Acode", "Bcode")
  unused_data_ab <- ab_data
  
  # Nrows > 0
  if (nrow(ab_data) == 0) {
    stop("No valid records found in the input correspondence table AB")
  }  
  # Missing value
  missing_code_rows <- ab_data[is.na(ab_data$Acode) | ab_data$Acode == "" | is.na(ab_data$Bcode) | ab_data$Bcode == "", ]
  if (nrow(missing_code_rows) > 0) stop(paste("Rows with missing values in the", ColumnNames_ab[1], "or", ColumnNames_ab[2], "column of the AB data:"))
  
  # Duplicates  Acode-Bcode check
  duplicate_pairs <- ab_data[duplicated(ab_data[c("Acode", "Bcode")]), c("Acode", "Bcode")]
  if (nrow(duplicate_pairs) > 0) {
    stop(paste0(
      "Duplicate row(s) found in AB file: the following Acode-Bcode pairs appear more than once:\n",
      paste(capture.output(print(duplicate_pairs)), collapse = "\n"),
      "\nPlease remove duplicate rows from your correspondence table."
    ))
  }
  
  
  # Filter rows in AB table based on longest Acode and/or Bcode
  if (longestAcodeOnly) {
    maxLengthA <- max(nchar(ab_data$Acode, type = "width"))
    ab_data <- ab_data[nchar(ab_data$Acode, type = "width") == maxLengthA, ]
  }
  
  if (longestBcodeOnly) {
    maxLengthB <- max(nchar(ab_data$Bcode, type = "width"))
    ab_data <- ab_data[nchar(ab_data$Bcode, type = "width") == maxLengthB, ]
  }
  
  # Final safety check
  if (nrow(ab_data) == 0) stop("No valid records after filtering longest codes.")
  
  
  if (!is.null(A) && is.character(A) && !file.exists(A)) {
    stop(paste0("File not found: ", A, ". Please check the path and filename."))
  }
  
  if (!is.null(A)) {
    a_data <- testInputTable("Source classification table (A)", A)
    
    
    colnames(a_data)[1] <- "Acode"
    unused_data_a <- a_data
    
    if (nrow(a_data) == 0) stop("No valid records found in source classification table (A).")
    dups_A <- a_data$Acode[duplicated(a_data$Acode)]
    if (length(dups_A) > 0) {
      stop(paste0(
        "Duplicate Acode(s) found in A file:\n",
        paste(unique(dups_A), collapse = ", "),
        "\nEach Acode must be unique in the source classification."
      ))
    }
    
    unmatched_codes_A <- setdiff(a_data$Acode, ab_data$Acode)
    extra_codes_in_AB_A <- setdiff(ab_data$Acode, a_data$Acode)
    
    if (length(unmatched_codes_A) > 0) {
      warning(paste0(
        "Unmatched source classification codes in A: ",
        paste(unmatched_codes_A, collapse = ", ")
      ))
    }
    
    if (length(extra_codes_in_AB_A) > 0) {
      warning(paste0(
        "Source classification codes in AB not found in A: ",
        paste(extra_codes_in_AB_A, collapse = ", ")
      ))
    }
  }
  
  if (!is.null(B) && is.character(B) && !file.exists(B)) {
    stop(paste0("File not found: ", B, ". Please check the path and filename."))
  }
  
  if (!is.null(B)) {
    b_data <- testInputTable("Target classification table (B)", B)
    colnames(b_data)[1] <- "Bcode"
    unused_data_b <- b_data
    
    if (nrow(b_data) == 0) stop("No valid records found in target classification table (B).")
    dups_B <- b_data$Bcode[duplicated(b_data$Bcode)]
    if (length(dups_B) > 0) {
      stop(paste0(
        "Duplicate Bcode(s) found in B file:\n",
        paste(unique(dups_B), collapse = ", "),
        "\nEach Bcode must be unique in the target classification."
      ))
    }
    
    unmatched_codes_B <- setdiff(b_data$Bcode, ab_data$Bcode)
    extra_codes_in_AB_B <- setdiff(ab_data$Bcode, b_data$Bcode)

    # Warnings explicites
    if (length(unmatched_codes_B) > 0) {
      warning(paste0(
        "Unmatched target classification codes in B: ",
        paste(unmatched_codes_B, collapse = ", ")
      ))
    }
    
    if (length(extra_codes_in_AB_B) > 0) {
      warning(paste0(
        "Target classification codes in AB not found in B: ",
        paste(extra_codes_in_AB_B, collapse = ", ")
      ))
    }
  }
  
  #graph biparti
  g <- igraph::graph_from_data_frame(ab_data, directed = FALSE)
  components <- igraph::decompose(g)
  component_codes <- lapply(components, function(comp) V(comp)$name)
  
  ab_data$component <- NA
  for (i in seq_along(component_codes)) {
    component <- component_codes[[i]]
    ab_data$component[ab_data$Acode %in% component | ab_data$Bcode %in% component] <- paste("Component", i)
  }
  
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
    target_positions <- unique(ab_data[ab_data$Bcode %in% component, "Bcode"])
    n_source_positions <- length(source_positions)
    n_target_positions <- length(target_positions)
    
    component_name <- unique(ab_data$component[ab_data$Acode %in% component | ab_data$Bcode %in% component])
    
    list(
      Component = component_name,
      CorrespondenceType = correspondence_type,
      SourcePositions = source_positions,
      TargetPositions = target_positions,
      nSourcePositions = n_source_positions,
      nTargetPositions = n_target_positions
    )
  })
  
  Inventory <- do.call(rbind, lapply(component_stats, function(x) {
    data.frame(
      Component = x$Component,
      CorrespondenceType = x$CorrespondenceType,
      SourcePositions = I(list(x$SourcePositions)),
      TargetPositions = I(list(x$TargetPositions)),
      nSourcePositions = as.integer(x$nSourcePositions),
      nTargetPositions = as.integer(x$nTargetPositions),
      stringsAsFactors = FALSE
    )
  }))
  # Table Analysis
  Analysis <- data.frame(
    ClassC = ab_data$Acode,
    ClassD = ab_data$Bcode,
    nTargetClasses = NA,
    SourceToTargetMapping = NA,
    nSourceClasses = NA,
    TargetToSourceMapping = NA
  )
  
  Analysis$nTargetClasses <- sapply(Analysis$ClassC, function(c_code) length(unique(Analysis[Analysis$ClassC == c_code, "ClassD"])))
  Analysis$SourceToTargetMapping <- sapply(Analysis$ClassC, function(c_code) paste(unique(Analysis[Analysis$ClassC == c_code, "ClassD"]), collapse = ", "))
  Analysis$nSourceClasses <- sapply(Analysis$ClassD, function(d_code) length(unique(Analysis[Analysis$ClassD == d_code, "ClassC"])))
  Analysis$TargetToSourceMapping <- sapply(Analysis$ClassD, function(d_code) paste(unique(Analysis[Analysis$ClassD == d_code, "ClassC"]), collapse = ", "))
  
  colnames(Analysis)[1:2] <- c("Acode", "Bcode")
  
  Inventory_df <- Inventory
  Inventory_df$Component <- as.character(Inventory_df$Component)
  Inventory_df$CorrespondenceType <- as.character(Inventory_df$CorrespondenceType)
  Inventory_df$SourcePositions <- sapply(Inventory_df$SourcePositions, function(x) paste(x, collapse = ", "))
  Inventory_df$TargetPositions <- sapply(Inventory_df$TargetPositions, function(x) paste(x, collapse = ", "))
  

  Analysis_df <- merge(Analysis, unused_data_ab, by = c("Acode", "Bcode"), all = FALSE)
  
  if (!is.null(B)) {
    Analysis_df <- merge(Analysis_df, unused_data_b, by = "Bcode", all = FALSE)
  }
  
  if (!is.null(A)) {
    Analysis_df <- merge(Analysis_df, unused_data_a, by = "Acode", all = FALSE)
  }
  force_integer_if_whole <- function(df) {
    for (col in names(df)) {
      if (is.numeric(df[[col]]) && all(df[[col]] %% 1 == 0, na.rm = TRUE)) {
        df[[col]] <- as.integer(df[[col]])
      }
    }
    df
  }
  Analysis_df <- Analysis_df[, c("Acode", "Bcode", setdiff(names(Analysis_df), c("Acode", "Bcode")))]
  colnames(Analysis_df)[1:2] <- ColumnNames_ab[1:2]
  
  CsvFileSave(CSVcorrespondenceInventory, Inventory_df)
  CsvFileSave(CSVcorrespondenceAnalysis, Analysis_df)
  
  list(Inventory = Inventory_df, Analysis = Analysis_df)
}




