#' @title Perform analysis on correspondence tables
#' @description Perform analysis on a correspondence table using its associated
#' classifications to generate various statistics. It checks the validity of the
#' input data, identifies components, calculates correspondence types, and
#' creates summary tables.
#'
#' @param AB A mandatory argument containing a data frame provided by the user
#'   with the correspondence table data. The table must contain at least two
#'   columns containing the two classifications \code{A code} and \code{B code}. The name of the two columns is irrelevant.
#' @param Aname Optional. A character string specifying the name of the column
#'   in `AB` that should be used as the source classification (`Acode`). If
#'   `NULL`, the function assumes that the first column of `AB` represents the
#'   source classification.
#'
#' @param Bname Optional. A character string specifying the name of the column
#'   in `AB` that should be used as the target classification (`Bcode`). If
#'   `NULL`, the function assumes that the second column of `AB` represents the
#'   target classification.
#'    
#' @param A A data frame containing the source classification data with
#'   an \code{"Acode"} column. Can be \code{NULL} if no source classification
#'   table is provided.
#' @param B A data frame containing the target classification data with
#'   a \code{"Bcode"} column. Can be \code{NULL} if no target classification
#'   table is provided.
#' @param longestAcodeOnly Logical. If \code{TRUE}, source classification data
#'   are filtered based on \code{"Acode"} retaining only the maximum length,
#'   i.e. the lowest-level Acodes in the correspondence table.
#' @param longestBcodeOnly Logical. If \code{TRUE}, target classification data
#'   are filtered based on \code{"Bcode"} retaining only the maximum length,
#'   i.e. the lowest-level Bcodes in the correspondence table.
#'
#' @importFrom igraph graph.data.frame decompose.graph
#' @import igraph
#'
#' @return A list containing two data frames: \code{Inventory} and \code{Analysis}.
#'   \code{Inventory} contains statistics related to components, correspondence
#'   types, and source/target positions. \code{Analysis} contains statistics for
#'   each class in the correspondence table.
#'
#' @export
#'
#' @examples
#' # Use data from the folder extdata
#' 
#' AB = read.csv(system.file("extdata", "ExempleAnnexe.csv", package = "correspondenceTables"))
#' head(AB)
#' 
#' result <- analyseCorrespondenceTable(
#'   AB = AB,
#'   Aname = NULL,
#'   Bname = NULL,
#'   A  = NULL,
#'   B  = NULL,
#'   longestAcodeOnly          = FALSE,
#'   longestBcodeOnly          = FALSE
#' )
#' 
#' print(result$Inventory)
#' print(result$Analysis)
#' 
#' # the name of the columns does not matter:
#' AB_mod = AB
#' colnames(AB_mod) = c("a", "b")
#' 
#' result_mod <- analyseCorrespondenceTable(
#'   AB = AB_mod,
#'   Aname = NULL,
#'   Bname = NULL,
#'   A  = NULL,
#'   B  = NULL,
#'   longestAcodeOnly          = FALSE,
#'   longestBcodeOnly          = FALSE
#' )
#' 
#' print(result_mod$Inventory)
#' print(result_mod$Analysis)
#' 
#' 
#' # examples with using Aname and Bname
#' 
#' # fist we add two more column to the same dataset
#' set.seed(123)
#' AB_mod2 = AB_mod
#' 
#' AB_mod2 <- cbind(new_first_col = sample(1:100, nrow(AB_mod2), replace = TRUE), AB_mod2)
#' AB_mod2 <- cbind(
#'   AB_mod2[1:2], 
#'   new_middle_col = sample(1:100, nrow(AB_mod2), replace = TRUE),
#'   AB_mod2[3:ncol(AB_mod2)]
#' )
#' colnames(AB_mod2)
#' 
#' result_mod2 <- analyseCorrespondenceTable(
#'   AB = AB_mod2,
#'   Aname = "a",
#'   Bname = "b",
#'   A  = NULL,
#'   B  = NULL,
#'   longestAcodeOnly          = FALSE,
#'   longestBcodeOnly          = FALSE
#' )
#' 
#' print(result_mod2$Inventory)
#' print(result_mod2$Analysis)
#' 
#' # check if we obtained the same results in all the 3 examples
#' 
#' first = identical(result$Inventory, result_mod$Inventory) 
#' second = identical(result_mod$Inventory, result_mod2$Inventory)
#' first && second
#' # TRUE
#' 
#' # the following won't work as we changed column names in AB_mod, 
#' # and added new columns in AB_mod2 but the results are the same.
#' first = identical(result$Analysis, result_mod$Analysis)
#' second = identical(result_mod$Analysis, result_mod2$Analysis)
#' first && second



analyseCorrespondenceTable <- function(AB,
                                       Aname = NULL,
                                       Bname = NULL,
                                       A = NULL,
                                       B = NULL,
                                       longestAcodeOnly = FALSE,
                                       longestBcodeOnly = FALSE) {
  # checks  
  
  if (!inherits(AB, "data.frame")) {
    stop("AB must be a data.frame.")
  }
  
  if (!is.null(Aname) && !(is.character(Aname) && length(Aname) == 1)) {
    stop("Aname must be a character or NULL.")
  }
  
  if (!is.null(Bname) && !(is.character(Bname) && length(Bname) == 1)) {
    stop("Bname must be a character or NULL.")
  }
  
  if (!(is.null(Aname) == is.null(Bname))) {
    stop("Aname and Bname must be both NULL or both non-NULL.")
  }
  
  
  if (!is.null(Aname) && !is.null(Bname)) {
    
    # Check they exist in AB
    if (!all(c(Aname, Bname) %in% colnames(AB))) {
      stop("Both Aname and Bname must correspond to existing columns in AB.")
    }
    
    if (Aname == Bname) {
      stop("Aname and Bname must refer to two different columns.")
    }
  }
  
  
  if (!is.null(A) && !inherits(A, "data.frame")) {
    stop("A must be a data.frame or NULL.")
  }
  
  if (!is.null(B) && !inherits(B, "data.frame")) {
    stop("B must be a data.frame or NULL.")
  }
  
  if (!is.logical(longestAcodeOnly)) {
    stop("Argument 'longestAcodeOnly' must be TRUE or FALSE (logical).")
  }
  if (!is.logical(longestBcodeOnly)) {
    stop("Argument 'longestBcodeOnly' must be TRUE or FALSE (logical).")
  }
  
  # function
  
  ab_data = AB
  
  if (!is.null(Aname) && !is.null(Bname)){
    ColumnNames_ab = c(Aname, Bname)
    colnames(ab_data)[which(colnames(ab_data)==Aname)] = "Acode"
    colnames(ab_data)[which(colnames(ab_data)==Bname)] = "Bcode"
    unused_data_ab <- ab_data
    ab_data <- ab_data[,c("Acode","Bcode")]
    
  } else {
    ColumnNames_ab <- colnames(ab_data)[1:2]
    colnames(ab_data)[1:2] <- c("Acode", "Bcode")
    unused_data_ab <- ab_data
  }
  
  
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
  
  
  if (!is.null(A)) {
    a_data <- A
    
    
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
  
  
  if (!is.null(B)) {
    b_data <- B
    
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
  component_codes <- lapply(components, function(comp) igraph::V(comp)$name)
  
  ab_data$component <- NA
  for (i in seq_along(component_codes)) {
    component <- component_codes[[i]]
    ab_data$component[ab_data$Acode %in% component | ab_data$Bcode %in% component] <- paste("Component", i)
  }
  
  component_stats <- lapply(components, function(comp) {
    component <- igraph::V(comp)$name
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
  
  list(Inventory = Inventory_df, Analysis = Analysis_df)
}