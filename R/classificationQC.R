#' @title Perform quality control on a classification
#'
#' @description
#' This function performs a series of structural and logical quality-control
#' checks on a hierarchical classification. It verifies the consistency of the
#' hierarchical levels, the presence of parent–child relationships, the
#' uniqueness of labels, the compliance with admissible coding rules, and the
#' sequencing of children within each level.  
#' The output is a list of data frames flagging potential issues.
#'
#' @param classification Either:
#'   \itemize{
#'     \item a \code{data.frame} containing (at least) two columns: the codes
#'           and labels of the classification to be checked, or
#'     \item a character string giving the path to a CSV file containing the
#'           same information.
#'   }
#'   This argument is mandatory.
#'
#' @param lengths Either:
#'   \itemize{
#'     \item a \code{data.frame} with one row per hierarchical level giving the
#'           initial and final positions of the segment of the code referring
#'           to that level, or
#'     \item a character string giving the path to a CSV file containing such
#'           information.
#'   }  
#'   The number of rows implicitly defines the number of hierarchical levels
#'   (\eqn{k}). This argument is mandatory.
#'
#' @param fullHierarchy Logical.  
#'   If \code{FALSE}, the function checks that all positions at levels greater
#'   than 1 have a parent at the level immediately above (no orphans).  
#'   If \code{TRUE}, it additionally checks that positions at levels strictly
#'   lower than \eqn{k} have children in the next level (no childless nodes).
#'
#' @param labelUniqueness Logical.  
#'   When \code{TRUE}, the function checks whether labels are unique at each
#'   hierarchical level. Duplicates are listed in the
#'   \code{QC_duplicatesLabel} table.
#'
#' @param labelHierarchy Logical.  
#'   When \code{TRUE}, the function checks that:
#'   \itemize{
#'     \item single children share the same label as their parent;
#'     \item if a parent shares a label with one of its children, it must be a
#'           single-child parent.
#'   }
#'
#' @param singleChildCode Either:
#'   \itemize{
#'     \item a \code{data.frame} specifying admissible codes for single
#'           children at each hierarchical level, or
#'     \item a character string giving the path to such a CSV file.
#'   }
#'   If not \code{NULL}, the function checks compliance with these codes.
#'
#' @param sequencing Either:
#'   \itemize{
#'     \item a \code{data.frame} defining admissible codes and their expected
#'           order for multiple-child situations at each level, or
#'     \item a character string giving the path to a CSV file containing these
#'           rules.
#'   }
#'   If not \code{NULL}, the function checks sequencing consistency.
#'
#' @param CSVout Either:
#'   \itemize{
#'     \item \code{NULL} (default), meaning that the function only returns a
#'           list of data.frames in R, or
#'     \item a character string giving the path where the resulting
#'           \code{QC_output} table should be saved as a CSV file.
#'   }
#'
#' @return
#' A list of data frames flagging potential issues in the classification:
#' \itemize{
#'   \item \code{QC_output}: full augmented table with computed levels,
#'         code segments, and all compliance flags;
#'   \item \code{QC_noLevels}: rows for which hierarchical level could not be
#'         determined;
#'   \item \code{QC_orphan}: positions that lack an expected parent;
#'   \item \code{QC_childless}: positions expected to have children but do not;
#'   \item \code{QC_duplicatesLabel}: positions with duplicate labels within
#'         the same level;
#'   \item \code{QC_duplicatesCode}: duplicated codes;
#'   \item \code{QC_singleChildMismatch}: label inconsistencies for
#'         single-children relationships;
#'   \item \code{QC_singleCodeError}: single children with invalid codes (if
#'         \code{singleChildCode} is provided);
#'   \item \code{QC_multipleCodeError}: non-single children with invalid codes
#'         (if \code{sequencing} is provided);
#'   \item \code{QC_gapBefore}: missing expected codes in sequence;
#'   \item \code{QC_lastSibling}: flag for last sibling in sequencing rules.
#' }
#'
#' @examples
#'
#'
#' classification <- read.csv(system.file("extdata", "Nace2.csv",
#'                                       package = "correspondenceTables"))
#'                                       
#' lengths <- data.frame(charb = c(1,2,3,5), chare = c(1,2,4,5))
#' 
#' CSVout <- system.file("extdata", "QC_Output.csv",
#'                      package = "correspondenceTables")
#'                      
#' Output <- classificationQC(
#'  classification = classification,
#'  lengths = lengths,
#'  fullHierarchy = TRUE,
#'  labelUniqueness = TRUE,
#'  labelHierarchy = TRUE,
#'  singleChildCode = NULL,
#'  sequencing = NULL,
#'  CSVout = CSVout
#')
#'
#' print(Output$QC_output)
#' print(Output$QC_orphan)
#' print(Output$QC_childless)
#' print(Output$QC_duplicatesLabel)
#' print(Output$QC_duplicatesCode)


classificationQC = function(classification, lengths, fullHierarchy = TRUE, labelUniqueness  = TRUE, labelHierarchy = TRUE, singleChildCode = NULL, sequencing = NULL, CSVout = NULL) {
  
  if (is.character(classification)) {
    if (tolower(tools::file_ext(classification)) != "csv") {
      stop("The classification should be provided as a csv file")
    }
    if (!file.exists(classification)) stop("The classification file does not exist.")
    classification <- tryCatch({
      read.csv(classification, header = TRUE)
    }, error = function(e) {
      stop("Error reading the classification file: ", e$message)
    })
  } else if (!is.data.frame(classification)) {
    stop("The classification must be a data frame or a valid CSV file path.")
  }
  colnames(classification)[1:2] <- c("Code", "Label")
  classificationName <- colnames(classification)[1]
  
  # #check that classification has only two columns
  # if(ncol(classification) != 2){
  #   stop("The classification must have only two colums corresponding to code and label.")
  # }
  #
  colnames(classification)[1:2] = c("Code", "Label")
  
  # Accept lengths as data.frame or CSV path
  if (is.character(lengths)) {
    
    if (tolower(tools::file_ext(lengths)) != "csv") {
      stop("The provided file does not have a CSV extension.")
    }
    if (!file.exists(lengths)) stop("The lengths file does not exist.")
    lengths <- tryCatch({
      read.csv(lengths, header = TRUE)
    }, error = function(e) {
      stop("Error reading the lengths file: ", e$message)
    })
    
    # Rename columns if not already correct
    expected_headers <- c("charb", "chare")
    if (!all(expected_headers %in% colnames(lengths))) {
      warning("Variable names do not match expected headers. Renaming.")
      colnames(lengths)[1:2] <- expected_headers
    }
    
  } else if (!is.data.frame(lengths)) {
    stop("The 'lengths' argument must be a data frame or a path to a CSV file.")
  }
  
  # Check formatting requirements
  if (nrow(lengths) == 0) {
    stop("Lengths file must have at least one row.")
  }
  
  negative_lengths <- which(lengths[,1] < 1 | lengths[,2] < 1)
  if (length(negative_lengths) > 0) {
    stop(paste("Lengths must be strictly positive. Error at row:", negative_lengths))
  }
  
  na_lengths <- which(is.na(lengths[,1]) | is.na(lengths[,2]))
  if (length(na_lengths) > 0) {
    stop(paste("Lengths cannot be missing. Error at row:", na_lengths))
  }
  
  for (i in 1:(nrow(lengths)-1)) {
    if (lengths[i,2] >= lengths[i+1,1]) {
      stop(paste("Sequences should not overlap in the lengths definition. Error at row:", i+1))
    }
  }
  
  ### RULE 1 - Correctness of formatting requirements (lengths file)
  #check that char file has at least one row
  if(nrow(lengths) == 0){
    stop("Lengths file must have at least one row")
  }
  
  #check if value are strictly positive
  negative_lengths <- which(lengths[,1] < 1 | lengths[,2] < 1)
  if (length(negative_lengths) > 0) {
    stop(paste("lengths must be strictly positive. Error at row:", negative_lengths))
  }
  
  #check if value are NA
  na_lengths <- which(is.na(lengths[,1]) | is.na(lengths[,2]))
  if (length(na_lengths) > 0) {
    stop(paste("lengths must be missing. Error at row:", na_lengths))
  }
  
  #check the sequences rendered by these numbers should not overlap
  for (i in 1:(nrow(lengths)-1)) {
    if (lengths[i,2] >= lengths[i+1,1]) {
      stop(paste("Sequences should not overlap in case of formatting errors. Error at row:", i+1))
    }
  }
  
  #check that char file has valid character ranges -- NEEDED?!
  # if(!all(sapply(lengths$charb, function(x) grepl("[A-Za-z0-9]", x)) &
  #         sapply(lengths$chare, function(x) grepl("[A-Za-z0-9]", x)) &
  #         lengths$charb <= lengths$chare)){
  #   stop("Char file must contain only numbers")
  # }
  #
  
  ## Create QC_output
  QC_output = classification
  
  # Add hierarchical level column
  QC_output$level <- sapply(classification$Code, function(x) {
    lvl <- which(nchar(x) == lengths$chare)
    if (length(lvl) == 0) return(NA_integer_)
    else return(lvl)
  })
  QC_output$level <- as.integer(QC_output$level)
  
  # Initialize the superior column
  QC_output$Parent = NA
  
  # Add Code and segment columns
  for (i in 1:nrow(lengths)) {
    charb = lengths$charb[i]
    chare = lengths$chare[i]
    
    segment_col_name = paste0("segment", i)
    code_col_name = paste0("Code", i)
    
    segments = character(nrow(QC_output))
    codes = character(nrow(QC_output))
    
    for (j in 1:nrow(QC_output)) {
      is_level_present <- !is.null(QC_output$level[j]) && !is.na(QC_output$level[j])
      is_code_present <- !is.null(QC_output$Code[j]) && !is.na(QC_output$Code[j])
      is_code_long_enough <- is_code_present && nchar(QC_output$Code[j]) >= chare
      is_level_eligible <- is_level_present && QC_output$level[j] >= i
      
      if (is_level_present && is_code_present && is_code_long_enough && is_level_eligible) {
        segments[j] <- substr(QC_output$Code[j], charb, chare)
        codes[j] <- substr(QC_output$Code[j], 1, chare)
      } else {
        segments[j] <- NA
        codes[j] <- NA
      }
    }
    
    
    QC_output[, segment_col_name] = segments
    QC_output[, code_col_name] = codes
  }
  
  # Add the 'superior' column based on the current level
  for (i in 1:nrow(QC_output)) {
    if (!is.na(QC_output$level[i]) && QC_output$level[i] > 1) {
      superior_col_name = paste0("Code", (QC_output$level[i] - 1))
      QC_output$Parent[i] = QC_output[i, superior_col_name]
    } else {
      QC_output$Parent[i] = NA
    }
  }
  
  ## RULE 2 - Compliance with formatting requirements (lengths file)
  na_level = which(is.na(QC_output$level))
  if (length(na_level) > 0) {
    stop("Some codes have no specified level. There might be possible errors in the classification or in the length file (see 'QC_noLevels').")
  }
  
  QC_noLevels = QC_output[na_level, ]
  
  ## RULE 3 - Uniqueness of codes
  dup =  duplicated(classification[,1])
  QC_output$duplicateCode = as.numeric(dup)
  if (any(dup)) {
    class_dup <- classification[dup,]
    errow_row <- which(dup)
    stop("Codes in classification file must be unique (see 'QC_duplicatesCode').")
  }
  QC_duplicatesCode = QC_output[which(QC_output$duplicateCode == 1), ]
  
  ## RULE 4 -	Fullness of hierarchy
  if (!is.null(fullHierarchy) && fullHierarchy != FALSE){
    QC_output$orphan = rep(NA, nrow(QC_output))
    
    for (k in nrow(lengths):2) {
      QC_output$exp_parents = rep(NA, nrow(QC_output))
      QC_output$exp_parents[which(QC_output$level == k)] = substr(QC_output[which(QC_output$level == k), paste0("Code", k)], 1, lengths[k-1,2])
      o_code = which(QC_output$exp_parents %in% QC_output[which(QC_output$level == k-1), paste0("Code", k-1)])
      QC_output$orphan[intersect(which(QC_output$level == k), o_code)] = 0
      noo_code = which(!QC_output$exp_parents %in% QC_output[which(QC_output$level == k-1), paste0("Code", k-1)])
      QC_output$orphan[intersect(which(QC_output$level == k), noo_code)] = 1
      #QC_output$orphan[which(is.na(QC_output$exp_parents))] = NA
    }
    QC_output = QC_output[, -which(colnames(QC_output) == "exp_parents")]
    
    #identify orphans
    orphan = which(QC_output$orphan == 1)
    if (length(orphan) > 0) {
      warning("Some codes at a lower level than 1 have no parents at higher levels ('see QC_orphan').")
    }
    
    QC_orphan = QC_output[orphan, ]
    
    #childless - if (fullHierarchy == TRUE)
    
    QC_output$childless = rep(NA, nrow(QC_output))
    
    for (k in 2:nrow(lengths)) {
      exp_parents = substr(QC_output[which(QC_output$level == k), paste0("Code", k)], 1, lengths[k-1,2])
      QC_output$childless[which(QC_output[, paste0("Code", k-1)] %in% exp_parents & QC_output$level == k-1)] = 0
      QC_output$childless[which(!QC_output[, paste0("Code", k-1)] %in% exp_parents & QC_output$level == k-1)] = 1
    }
    
    #identify childless
    childless = which(QC_output$childless == 1)
    if (length(childless) > 0) {
      warning(paste("Some codes at a higher level than ", nrow(lengths) ," have no children at lower levels ('see QC_childless')."))
    }
    QC_childless = QC_output[childless, ]
  }else{
    cat("FullHierarchy is NULL so no treatment")
  }
  ###  RULE 5 - Uniqueness of labels
  if (!is.null(labelUniqueness) && labelUniqueness != FALSE) {
    QC_output$duplicateLabel = 0
    
    # Get unique levels
    unique_levels <- unique(QC_output$level)
    
    # Iterate over each unique level
    for (le in unique_levels) {
      level_data = QC_output[QC_output$level == le,]
      
      if (nrow(level_data) != length(unique(level_data$Label))) {
        # There are duplicates, mark them in the QC output column
        # The outcome of the test should be reported in a new ‘QC output’ column (duplicateLabel) assuming the value 1 for positions involved in duplicates (0 otherwise).
        duplicate_labels = level_data$Label[duplicated(level_data$Label)]
        ## Here we add 1 if we have the same label.
        QC_output$duplicateLabel[QC_output$level == le & QC_output$Label %in% duplicate_labels] = 1
      }
    }
    
    # Identify duplicates
    duplicatesLabel = which(QC_output$duplicateLabel == 1)
    if (length(duplicatesLabel) > 0) {
      warning(paste("Some codes at the same hierarchical level have the same labels (see 'QC_duplicatesLabel')."))
    }
    QC_duplicatesLabel = QC_output[duplicatesLabel,]
  }
  
  ## Prepare label_nocode globally for reuse (labelHierarchy, singleChildCode, etc.)
  label_nocode <- gsub("(\\d)|\\.", "", QC_output$Label)
  label_nocode <- mapply(function(x, y) sub(x, "", y), QC_output$Code, label_nocode)
  label_nocode <- tolower(stringr::str_squish(label_nocode))
  ## RULE 6 - Hierarchical label dependencies
  if (!is.null(labelHierarchy) && labelHierarchy != FALSE){
    QC_output$singleChildMismatch = 0
    
    for (k in 1:(nrow(lengths)-1)) {
      
      #select only parents with children
      parents_k = QC_output$Code[which(QC_output$level == k)]
      #list the no. of child for each parent then select parents with single child / or multiple children
      child_ls = sapply(unique(parents_k), function(x) length(unique(na.omit(QC_output[which(QC_output[[paste0("Code", k)]] == x),  paste0("Code", k+1)]))))
      code_singlechild = QC_output[which(QC_output[[paste0("Code", k)]] %in% names(which(child_ls == 1)) & !is.na(QC_output[[paste0("Code", k+1)]]) & QC_output$level == k +1), c(paste0("Code", k), paste0("Code", k+1))]
      code_multichild = QC_output[which(QC_output[[paste0("Code", k)]] %in% names(which(child_ls > 1)) & !is.na(QC_output[[paste0("Code", k+1)]]) & QC_output$level == k +1), c(paste0("Code", k), paste0("Code", k+1))]
      
      #check if single child have different labels from their parents (=1)
      if (nrow(code_singlechild) != 0){
        for (c in 1:nrow(code_singlechild)){
          row_parent = which(QC_output$Code == as.character(code_singlechild[c,1]))
          label_parent = label_nocode[row_parent]
          row_child = which(QC_output$Code == as.character(code_singlechild[c,2]))
          label_child = label_nocode[row_child]
          if (label_parent != label_child) {
            QC_output$singleChildMismatch[row_child] = 1
          } else if (length(row_child) > 1) {
            QC_output$singleChildMismatch[row_child] = 9
          } else {
            QC_output$singleChildMismatch[row_child] = 0
          }
          
          
        }
      }
    }
    #identify mismatches -
    singleChildMismatch = which(QC_output$singleChildMismatch != 0)
    if (length(singleChildMismatch) > 0) {
      warning(paste("Some single child has a different label from their parent or some multiple child has the same label as their parent (see 'QC_singleChildMismatch')."))
    }
    QC_singleChildMismatch = QC_output[singleChildMismatch,]
    
  }else{
    cat("labelHierarchy is NULL so no treatment")
  }
  
  ## RULE 7 - Single child code compliance
  if (!is.null(singleChildCode)) {
    if (!is.null(singleChildCode)) {
      if (is.character(singleChildCode)) {
        if (!file.exists(singleChildCode)) stop("The singleChildCode file does not exist.")
        
        singleChildCode <- tryCatch({
          read.csv(singleChildCode, header = TRUE)
        }, error = function(e) {
          stop("Error reading the singleChildCode file: ", e$message)
        })
        
      } else if (!is.data.frame(singleChildCode)) {
        stop("The singleChildCode argument must be a data frame or a valid CSV path.")
      }
      
      # Vérifier et corriger les noms de colonnes si nécessaire
      expected_headers <- c("level", "singleCode", "multipleCode")
      if (!all(expected_headers %in% colnames(singleChildCode))) {
        warning("Variable names do not match expected headers. Renaming to 'level', 'singleCode', 'multipleCode'.")
        colnames(singleChildCode)[1:3] <- expected_headers
      }
    }
    QC_output$singleCodeError <- 0
    QC_output$multipleCodeError <- 0
    
    for (k in 1:(nrow(lengths) - 1)) {
      
      segment_kplus1 <- na.omit(QC_output[[paste0("segment", k + 1)]])
      
      if (length(segment_kplus1) == 0) {
        next
      }
      
      if (length(unique(nchar(segment_kplus1))) > 1) {
        QC_output$singleCodeError[which(nchar(QC_output[[paste0("segment", k + 1)]]) > 1)] <- NA
        QC_output$multipleCodeError[which(nchar(QC_output[[paste0("segment", k + 1)]]) > 1)] <- NA
        next
      } else {
        parents_k <- QC_output$Code[which(QC_output$level == k)]
        
        child_ls <- sapply(unique(parents_k), function(x)
          length(unique(na.omit(QC_output[which(QC_output[[paste0("Code", k)]] == x), paste0("Code", k + 1)]))))
        
        code_singlechild <- QC_output[
          which(QC_output[[paste0("Code", k)]] %in% names(which(child_ls == 1)) &
                  !is.na(QC_output[[paste0("Code", k + 1)]]) &
                  QC_output$level == k + 1),
          c(paste0("Code", k), paste0("Code", k + 1))
        ]
        
        code_multichild <- QC_output[
          which(QC_output[[paste0("Code", k)]] %in% names(which(child_ls > 1)) &
                  !is.na(QC_output[[paste0("Code", k + 1)]]) &
                  QC_output$level == k + 1),
          c(paste0("Code", k), paste0("Code", k + 1))
        ]
        
        level <- singleChildCode[k, 1]
        single <- singleChildCode[which(singleChildCode[, 1] == level), 2]
        multi <- as.character(singleChildCode[which(singleChildCode[, 1] == level), 3])
        
        if (nrow(code_singlechild) != 0) {
          for (c in 1:nrow(code_singlechild)) {
            row_child <- which(QC_output$Code == as.character(code_singlechild[c, 2]))
            end_code <- stringr::str_sub(QC_output$Code[row_child], nchar(QC_output$Code[row_child]), nchar(QC_output$Code[row_child]))
            if (!end_code %in% single) {
              QC_output$singleCodeError[row_child] <- 1
            }
          }
        }
        
        if (nrow(code_multichild) != 0) {
          for (m in 1:nrow(code_multichild)) {
            row_child <- which(QC_output$Code == as.character(code_multichild[m, 2]))
            end_code <- stringr::str_sub(QC_output$Code[row_child], nchar(QC_output$Code[row_child]), nchar(QC_output$Code[row_child]))
            if (!end_code %in% unlist(strsplit(multi, ""))) {
              QC_output$multipleCodeError[row_child] <- 1
            }
          }
        }
      }
    }
    
    singleCodeError <- which(QC_output$singleCodeError != 0)
    if (length(singleCodeError) > 0) {
      warning("Some single children been wrongly coded (see 'QC_singleCodeError').")
    }
    QC_singleCodeError <- QC_output[singleCodeError, ]
    
    multipleCodeError <- which(QC_output$multipleCodeError != 0)
    if (length(multipleCodeError) > 0) {
      warning("Some multiple children been wrongly coded (see 'QC_multipleCodeError').")
    }
    QC_multipleCodeError <- QC_output[multipleCodeError, ]
  }
  ## RULE 8 - Sequencing of codes
  if (!is.null(sequencing)) {
    if (is.character(sequencing)) {
      if (!file.exists(sequencing)) stop("The sequencing file does not exist.")
      
      sequencing <- tryCatch({
        read.csv(sequencing, header = TRUE)
      }, error = function(e) {
        stop("Error reading the sequencing file: ", e$message)
      })
      
    } else if (!is.data.frame(sequencing)) {
      stop("The sequencing argument must be a data frame or a valid CSV path.")
    }
    
    # Vérification des colonnes
    expected_headers <- c("level", "multipleCode")
    if (!all(expected_headers %in% colnames(sequencing))) {
      warning("Variable names do not match expected headers. Renaming to 'level', 'multipleCode'.")
      colnames(sequencing)[1:2] <- expected_headers
    }
    
    QC_output$gapBefore <- 0
    QC_output$lastSibling <- 0
    lengths$level <- seq_len(nrow(lengths))
    levels_to_filter <- unique(sequencing$level)
    lengths2 <- lengths[lengths$level %in% levels_to_filter, ]
    lengths2$level <- NULL
    
    for (k in 1:nrow(lengths2)) {
      segment_kplus1 <- na.omit(QC_output[[paste0("segment", k + 1)]])
      
      if (length(unique(nchar(segment_kplus1))) > 1) {
        warning(paste0("Sequencing of codes cannot be checked at level ", k + 1, " as segments of code have more than one character."))
        QC_output$gapBefore[which(nchar(QC_output[[paste0("segment", k + 1)]]) > 1)] <- NA
        QC_output$lastSibling[which(nchar(QC_output[[paste0("segment", k + 1)]]) > 1)] <- NA
        next
      }
      
      # Bloc de sécurisation des segments
      segments <- rep(NA, nrow(QC_output))
      codes <- rep(NA, nrow(QC_output))
      for (j in 1:nrow(QC_output)) {
        if (!is.null(QC_output$level[j]) &&
            !is.na(QC_output$level[j]) &&
            !is.na(QC_output$Code[j]) &&
            nchar(QC_output$Code[j]) >= lengths2[k, "chare"] &&
            QC_output$level[j] >= k + 1) {
          
          segments[j] <- substr(QC_output$Code[j], lengths2[k, "charb"], lengths2[k, "chare"])
          codes[j] <- substr(QC_output$Code[j], 1, lengths2[k, "chare"])
        }
      }
      
      parents_k <- QC_output$Code[QC_output$level == k]
      child_ls <- sapply(unique(parents_k), function(x)
        length(unique(na.omit(QC_output[QC_output[[paste0("Code", k)]] == x, paste0("Code", k + 1)]))))
      
      code_multichild <- QC_output[QC_output[[paste0("Code", k)]] %in% names(which(child_ls > 1)) &
                                     !is.na(QC_output[[paste0("Code", k + 1)]]) &
                                     QC_output$level == k + 1,
                                   c(paste0("Code", k), paste0("Code", k + 1))]
      
      level <- sequencing$level[k]
      multi <- as.character(sequencing[sequencing[, 1] == level, 2])
      multi <- if (length(multi) > 0) strsplit(multi, "")[[1]] else NULL
      
      multi_code <- stringr::str_sub(code_multichild[, 2], nchar(code_multichild[, 2]), nchar(code_multichild[, 2]))
      
      mcode_ls <- sapply(unique(code_multichild[, 1]), function(x)
        code_multichild[, 2][code_multichild[, 1] == x])
      ecode_ls <- lapply(mcode_ls, function(x)
        stringr::str_sub(x, nchar(x), nchar(x)))
      
      last_dig <- unlist(lapply(ecode_ls, function(x)
        which(x == max(x, na.rm = TRUE))[1]))
      last_code <- as.vector(mapply(function(x, y) x[y], mcode_ls, last_dig))
      
      gap_find <- lapply(ecode_ls, function(x) match(multi, x))
      code_gap <- lapply(gap_find, function(x) which(is.na(x)) + 1)
      gapbefore_dig <- mapply(function(x, y) na.omit(x[y]), gap_find, code_gap)
      gapbefore_code <- as.vector(unlist(mapply(function(x, y) x[y], mcode_ls, gapbefore_dig)))
      
      QC_output$lastSibling[QC_output$Code %in% last_code] <- 1
      QC_output$gapBefore[QC_output$Code %in% gapbefore_code] <- 1
      
      row_child <- which(QC_output$level == k & QC_output$multipleCodeError == 1)
      QC_output$gapBefore[row_child] <- 9
    }
    
    gap <- which(QC_output$gapBefore == 1)
    if (length(gap) > 0) {
      warning("There are gaps in the sequencing of multiple children coding (see 'QC_gapBefore').")
    }
    QC_gapBefore <- QC_output[gap, ]
    
    lastSibling <- which(QC_output$lastSibling == 1)
    QC_lastSibling <- QC_output[lastSibling, ]
  }
  
  ## RESULTS
  colnames(QC_output)[1:1] <- classificationName
  
  # Always start with QC_output
  return_ls <- list("QC_output" = QC_output)
  
  # Add optional data frames only if they exist
  if (exists("QC_noLevels")) {
    return_ls[["QC_noLevels"]] <- QC_noLevels
  }
  if (exists("QC_duplicatesCode")) {
    return_ls[["QC_duplicatesCode"]] <- QC_duplicatesCode
  }
  if (exists("QC_duplicatesLabel")) {
    return_ls[["QC_duplicatesLabel"]] <- QC_duplicatesLabel
  }
  if (exists("QC_orphan")) {
    return_ls[["QC_orphan"]] <- QC_orphan
  }
  if (exists("QC_childless")) {
    return_ls[["QC_childless"]] <- QC_childless
  }
  if (exists("QC_singleChildMismatch")) {
    return_ls[["QC_singleChildMismatch"]] <- QC_singleChildMismatch
  }
  if (exists("QC_singleCodeError")) {
    return_ls[["QC_singleCodeError"]] <- QC_singleCodeError
  }
  if (exists("QC_multipleCodeError")) {
    return_ls[["QC_multipleCodeError"]] <- QC_multipleCodeError
  }
  if (exists("QC_gapBefore")) {
    return_ls[["QC_gapBefore"]] <- QC_gapBefore
  }
  if (exists("QC_lastSibling")) {
    return_ls[["QC_lastSibling"]] <- QC_lastSibling
  }
  
  # Write CSV output if requested
  if (!is.null(CSVout)) {
    if (is.logical(CSVout) && CSVout == TRUE) {
      name <- names(QC_output)[1]
      file_name <- paste0("QC_output_", classificationName, ".csv")
      path_file <- file.path(getwd(), file_name)
      write.csv(QC_output, path_file, row.names = FALSE)
      message(paste0("The table was saved in ", getwd(), "/", file_name))
    } else if (is.character(CSVout)) {
      write.csv(QC_output, CSVout, row.names = FALSE)
    }
  }
  
  return(return_ls)
  
}
