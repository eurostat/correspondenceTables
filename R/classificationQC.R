#' @title ClassificationQC performs quality control checks on statistical classifications
#' @description The `classificationQC()` function performs quality control checks on statistical classifications, ensuring their integrity and accuracy. It checks compliance with various structural rules and provides informative error messages for violations. This function requires input files containing code and label information for each classification position and verifies the following criteria:
#' - Formatting requirements
#' - Uniqueness of codes
#' - Fullness of hierarchy
#' - Uniqueness of labels
#' - Hierarchical label dependencies
#' - Single child code compliance
#' - Sequencing of codes
#'
#' The `classificationQC()` function generates a QC output data frame with the classification data, hierarchical levels, code segments, and test outcomes. Additionally, it allows exporting the output to a CSV file.
#'
#' @param classification Refers to a classification in a CSV file structured with two columns: "codes" and "labels." If the classification is provided as a CSV file. This argument is mandatory.
#' @param lengthsfile Refers to a CSV file (one record per hierarchical level) containing the initial and last positions of the segment of the code specific to that level. The number of lines in this CSV file implicitly defines the number of hierarchical levels of the classification. This argument is mandatory.
#' @param fullHierarchy It is used to test the fullness of hierarchy. If the parameter \code{fullHierarchy} is set to \code{FALSE}, the function will check that every position at a lower level than 1 should have parents all the way up to level 1. If set to \code{TRUE}, it will additionally check that any position at a higher level than k should have children all the way down to level k.
#' @param labelUniqueness It is used to test that positions at the same hierarchical level have unique labels. If set to \code{TRUE}, the compliance is checked, and positions with duplicate labels are marked as 1 in the "duplicateLabel" column, while positions with unique labels are marked as 0.
#' @param labelHierarchy It is used to ensure that the hierarchical structure of labels is respected. When set to \code{TRUE}, the function will check that single children have a label identical to the label of their parent and that if a position has a label identical to the label of one of its children, then that position should only have a single child.
#' @param singleChildCode It refers to a CSV file with specific formatting to define valid codes for each level. If this parameter is not \code{NULL}, then it checks compliance with coding rules for single children and non-single children, as provided in the CSV file.
#' @param sequencing It refers to a CSV file to define the admissible codes for multiple children at each level. If this parameter is not \code{NULL}, the function checks the sequencing of multiple children codes within each level, as provided in the CSV file.
#' @param CSVout The valid values are \code{NULL} means that the user don't need to write the parameters  \code{TRUE}. In both cases, the output will be returned as an R list. If the output should be saved as an CSV file, the argument should be set to \code{TRUE}. By default, no CSV file is produced.
#' @importFrom  stringr str_squish 
#' @importFrom  stringr str_sub
#' @import writexl
#' @export
#' @return
#'
#' \code{classificationQC()} returns a list of data frames identifying possible cases violating the formatting requirements. The databases returned depend on the rules checked. The databases produced are:
#' - \code{QC_output}: The dataset includes all the original records in the classification. The "Level" column refers to the hierarchical levels of each position. Each code will be parsed into segment_k (column "Segmentk") and code_k (column "Codek"), corresponding to the code and segment at hierarchical level k, respectively. Additional columns are included to flag the behavior in each position:
#'   - \code{Orphan}: If \code{fullHierarchy} is set to \code{FALSE}, an "orphan" is a position at a hierarchical level (j) greater than 1 that lacks a parent at the hierarchical level (j-1) immediately above it. Orphan positions are marked with a value of 1 in the "Orphan" column, indicating their orphan status; otherwise, they are assigned a value of 0.
#'   - \code{Childless}: If \code{fullHierarchy} is set to \code{TRUE}, a "childless" position is one at a hierarchical level (j) less than k that lacks a child at the hierarchical level (j+1) immediately below it. Childless positions are marked with a value of 1 in the "Childless" column, indicating their childless status; otherwise, they are assigned a value of 0.
#'   - \code{DuplicateLabel}: A new column in the output that flags positions involved in duplicate label situations (where multiple positions share the same label at the same hierarchical level) by assigning them a value of 1, while positions with unique labels are assigned a value of 0.
#'   - \code{SingleChildMismatch}: A column in the output provides information about label hierarchy consistency in a hierarchical classification system. It indicates:
#'     - Value 1: Mismatched labels between a parent and its single child.
#'     - Value 9: Parent-child pairs with matching labels, but the parent has multiple children.
#'     - Value 0: Compliance with the label hierarchy rule, indicating no mismatches or violations.
#'   - \code{SingleCodeError}: A column serves as a flag indicating whether a position is a single child and whether the corresponding "singleCode" contains the level j segment. A value of 1 signifies a mismatch, while a value of 0 indicates compliance with the coding rules.
#'   - \code{MultipleCodeError}: A column serves as a flag indicating whether a position is not a single child and whether the corresponding "multipleCodej" contains the level j segment. A value of 1 signifies a mismatch, while a value of 0 indicates compliance with the coding rules.
#'   - \code{GapBefore}: The column is a flag that indicates whether there is a gap before a specific code within its level in the hierarchy. A gap refers to the absence of a sibling code that should logically precede the given code. In other words, it checks if there is a missing code in the sequence before a particular code.
#'   - \code{LastSibling}: The "LastSibling" column is a flag that identifies whether a code is the last sibling code at its level in the hierarchy. It is only relevant for codes with multiple children, meaning there are other codes at the same level with the same parent.
#'
#' - \code{QC_noLevels}: A subset of the \code{QC_output} dataframe including only records for which levels are not defined. If this dataframe is not empty, it suggests that either the classification or the length file is not correctly specified.
#'
#' - \code{QC_orphan}: A subset of the \code{QC_output} dataframe including only records that have no parents at the higher hierarchical level.
#'
#' - \code{QC_childless}: A subset of the \code{QC_output} dataframe including only records that have no children at the lower hierarchical level.
#'
#' - \code{QC_duplicatesLabel}: A subset of the \code{QC_output} dataframe including only records that have duplicated labels in the same hierarchical level.
#'
#' - \code{QC_duplicatesCode}: A subset of the \code{QC_output} dataframe including only records that have the same codes.
#'
#' - \code{QC_singleChildMismatch}: A subset of the \code{QC_output} dataframe including only records that are single children and have different labels from their parents or that are multiple children and have the same labels as their parents.
#'
#' - \code{QC_singleCodeError}: A subset of the \code{QC_output} dataframe including only records that are single children and have been wrongly coded (not following the rule provided in the 'SingleChildMismatch' CSV file).
#'
#' - \code{QC_multipleCodeError}: A subset of the \code{QC_output} dataframe including only records that are multiple children and have been wrongly coded (not following the rule provided in the 'SingleChildMismatch' CSV file).
#'
#' - \code{QC_gapBefore}: A subset of the \code{QC_output} dataframe including only records that are multiple children and have a gap in the sequencing provided in the 'sequencing' CSV file.
#'
#' - \code{QC_lastSibling}: A subset of the \code{QC_output} dataframe including only records that are multiple children and are the last sibling following the sequencing provided in the 'sequencing' CSV file.
#'
#' @examples 
#' {
#' 
#'   classification_path <- system.file("extdata", "Nace2.csv", package = "correspondenceTables")
#'   
#'   lengthsFile_path <- system.file("extdata", "lenghtsNace.csv", package = "correspondenceTables")
#'   
#'   Output <- classificationQC(classification = system.file("extdata", "Nace2.csv", package = "correspondenceTables") , lengthsFile = system.file("extdata", "lenghtsNace.csv", package = "correspondenceTables"), fullHierarchy = TRUE, labelUniqueness  = TRUE, labelHierarchy = TRUE, singleChildCode = NULL, sequencing = NULL, CSVout = system.file("extdata", "QC_Output.csv", package = "correspondenceTables")) 
#'   print(Output$QC_output)
#'   print(Output$QC_noLevels)
#'   print(Output$QC_orphan)
#'   print(Output$QC_childless)
#'   print(Output$QC_duplicatesLabel)
#'   print(Output$QC_duplicatesCode)
#'   print(Output$QC_singleChildMismatch)
#'   print(Output$QC_singleCodeError)
#'   print(Output$QC_multipleCodeError
#'   
#'  )
#'   print(Output$QC_gapBefore)
#'   print(Output$QC_lastSibling)
#' }

  

classificationQC = function(classification, lengthsFile, fullHierarchy = TRUE, labelUniqueness  = TRUE, labelHierarchy = TRUE, singleChildCode = NULL, sequencing = NULL, CSVout = NULL) {
  
  
  if ((length(grep("csv", classification)) == 0) ){
    stop("The classification should be provided as a csv file")
  }
  
  if (length(grep("csv", classification)) > 0){
    classification = read.csv(classification, header = TRUE)
  }
 
  #check that classification has only two columns
  if(ncol(classification) != 2){
    stop("The classification must have only two colums corresponding to code and label.")
  }
  
  colnames(classification)[1:2] = c("Code", "Label")
  
  ## Length table 
  if ((length(grep("csv", lengthsFile))) == 0) {
    stop("The lengthsFile should be provided as a csv file")
  }   
  
  if (length(grep("csv", lengthsFile)) > 0){
    lengths = read.csv(lengthsFile, header = TRUE) 
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
  if(!all(sapply(lengths$charb, function(x) grepl("[A-Za-z0-9]", x)) &
          sapply(lengths$chare, function(x) grepl("[A-Za-z0-9]", x)) &
          lengths$charb <= lengths$chare)){
    stop("Char file must contain only numbers")
  }
  
  
  ## Create QC_output
  QC_output = classification
  
  # Add hierarchical level column
  QC_output$level = sapply(classification$Code, function(x) which(nchar(x) == lengths$chare))
  
  # Initialize the superior column
  QC_output$superior = NA
  
  # Add Code and segment columns
  for (i in 1:nrow(lengths)) {
    charb = lengths$charb[i]
    chare = lengths$chare[i]
    
    segment_col_name = paste0("segment", i)
    code_col_name = paste0("Code", i)
    
    segments = character(nrow(QC_output))
    codes = character(nrow(QC_output))
    
    for (j in 1:nrow(QC_output)) {
      if (QC_output$level[j] >= i) {
        segments[j] = ifelse(nchar(QC_output$Code[j]) >= chare, substr(QC_output$Code[j], charb, chare), NA)
        codes[j] = ifelse(nchar(QC_output$Code[j]) >= chare, substr(QC_output$Code[j], 1, chare), NA)
      } else {
        segments[j] = NA
        codes[j] = NA
      }
    }
    
    QC_output[, segment_col_name] = segments
    QC_output[, code_col_name] = codes
  }
  
  # Add the 'superior' column based on the current level
  for (i in 1:nrow(QC_output)) {
    if (QC_output$level[i] > 1) {
      superior_col_name = paste0("Code", (QC_output$level[i] - 1))
      QC_output$superior[i] = QC_output[i, superior_col_name]
    } else {
      QC_output$superior[i] = NA
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
 if (!is.null(labelUniqueness) && labelUniqueness != FALSE){
    QC_output$duplicateLabel = 0
    
    # Check for duplicate labels at each hierarchical level
    for (le in unique(QC_output$level)) {
      level_data = QC_output[QC_output$level == le,]
      level_data$Label = substr(level_data$Label, lengths[le,2] + 1, nchar(level_data$Label)) 
      if (nrow(level_data) != length(unique(level_data$Label))) {
        # There are duplicates, mark them in the QC output column
        # The outcome of the test should be reported in a new ‘QC output’ column (duplicateLabel) assuming the value 1 for positions involved in duplicates (0 otherwise).
        duplicate_labels = level_data$Label[duplicated(level_data$Label)]
        ##Here we add 1 if we have the same label.
        QC_output$duplicateLabel[QC_output$level == le & QC_output$Label %in% duplicate_labels] = 1
      }
    }
    
    #identify duplicates
    duplicatesLabel = which(QC_output$duplicateLabel == 1)
    if (length(duplicatesLabel) > 0) {
      warning(paste("Some codes at the same hierarchical level have the same labels (see 'QC_duplicatesLabel')."))
    }
    QC_duplicatesLabel = QC_output[duplicatesLabel,]
  }else{
    cat("labelUniqueness is NULL so no treatment")
  }
  
  
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
      
      #only label
      label_nocode = gsub("(\\d)|\\.", "", QC_output$Label)
      label_nocode = mapply(function(x,y) sub(x, "", y), QC_output$Code, label_nocode)
      label_nocode = tolower(str_squish(label_nocode))
      
      #check if single child have different labels from their parents (=1)
      if (nrow(code_singlechild) != 0){
        for (c in 1:nrow(code_singlechild)){
          row_parent = which(QC_output$Code == as.character(code_singlechild[c,1]))
          label_parent = label_nocode[row_parent]
          row_child = which(QC_output$Code == as.character(code_singlechild[c,2]))
          label_child = label_nocode[row_child]
          if (label_parent != label_child) {
            QC_output$singleChildMismatch[row_child] = 1
          }
        }
      }
      #identify mismatches - 
      singleChildMismatch = which(QC_output$singleChildMismatch != 0)
      if (length(singleChildMismatch) > 0) {
        warning(paste("Some single child have different labels from their parents or some multiple children have same labels to their parents (see 'QC_singleChildMismatch')."))
      }
      QC_singleChildMismatch = QC_output[singleChildMismatch,]
    }
  }else{
    cat("labelHierarchy is NULL so no treatment")
  }
  
  ## RULE 7 -	Single child code compliance 
  if (!is.null(singleChildCode) && file.exists(singleChildCode)){
 
    
      singleChildCode <- read.csv(singleChildCode)
  
    
    
    QC_output$singleCodeError = 0
    QC_output$multipleCodeError = 0
    
    for (k in 1:(nrow(lengths)-1)) {
      
      if (unique(nchar(na.omit(QC_output[[paste0("segment", k+1)]]))) > 1) {
        warning(paste0("Single child code compliance cannot be checked at level ", k+1, " as segments of code have more than one character."))   
        QC_output$singleCodeError[which(nchar(na.omit(QC_output[[paste0("segment", k+1)]])) > 1)] = NA
        QC_output$multipleCodeError[which(nchar(na.omit(QC_output[[paste0("segment", k+1)]])) > 1)] = NA
      } 
      
      if (unique(nchar(na.omit(QC_output[[paste0("segment", k+1)]]))) == 1) {
        
        #select only parents with children
        parents_k = QC_output$Code[which(QC_output$level == k)]
        #list the no. of child for each parent then select parents with single child / or multiple children
        child_ls = sapply(unique(parents_k), function(x) length(unique(na.omit(QC_output[which(QC_output[[paste0("Code", k)]] == x),  paste0("Code", k+1)]))))
        code_singlechild = QC_output[which(QC_output[[paste0("Code", k)]] %in% names(which(child_ls == 1)) & !is.na(QC_output[[paste0("Code", k+1)]]) & QC_output$level == k +1), c(paste0("Code", k), paste0("Code", k+1))]
        code_multichild = QC_output[which(QC_output[[paste0("Code", k)]] %in% names(which(child_ls > 1)) & !is.na(QC_output[[paste0("Code", k+1)]]) & QC_output$level == k +1), c(paste0("Code", k), paste0("Code", k+1))]
        
        level = singleChildCode[k, 1]
        ##singleCode take all the code end by a "0"
        single = singleChildCode[which(singleChildCode[,1] == level),2]
        ## MultipleCode take all the code end as in the csv file #COULD BE LETTER AS WELL
        multi = as.character(singleChildCode[which(singleChildCode[,1] == level),3])
        
        #Determine the observed code end for single and multi children
        single_code = str_sub(code_singlechild[,2], nchar(code_singlechild[,2]), nchar(code_singlechild[,2]))
        multi_code = str_sub(code_multichild[,2], nchar(code_multichild[,2]), nchar(code_multichild[,2]))
        
        #Check if they are correct (single)
        if (nrow(code_singlechild) != 0){
          for (c in 1:nrow(code_singlechild)){
            row_parent = which(QC_output$Code == as.character(code_singlechild[c,1]))
            label_parent = label_nocode[row_parent]
            row_child = which(QC_output$Code == as.character(code_singlechild[c,2]))
            label_child = label_nocode[row_child]
            
            #check if single child have correct end code
            end_code = str_sub(QC_output$Code[row_child], nchar(QC_output$Code[row_child]), nchar(QC_output$Code[row_child])) 
            if (!end_code %in% single) {
              QC_output$singleCodeError[row_child] = 1
            }
          }
        }
        
        
        #Check if they are correct (multi)
        if (nrow(code_multichild) != 0){
          for (m in 1:nrow(code_multichild)){
            row_parent = which(QC_output$Code == as.character(code_multichild[m,1]))
            label_parent = label_nocode[row_parent]
            row_child = which(QC_output$Code == as.character(code_multichild[m,2]))
            label_child = label_nocode[row_child]
            #check if multiple child have same labels to their parents (=9)
            if (label_parent == label_child) {
              QC_output$singleChildMismatch[row_child] = 9
            }
            
            #check if multi child have correct end code
            end_code = str_sub(QC_output$Code[row_child], nchar(QC_output$Code[row_child]), nchar(QC_output$Code[row_child])) 
            if (!end_code %in% multi) {
              QC_output$multipleCodeError[row_child] = 1
            }
          }
        }
      }
    }
    
    
    #identify mismatches
    singleCodeError = which(QC_output$singleCodeError != 0)
    if (length(singleCodeError) > 0) {
      warning(paste("Some single children been wrongly coded (see 'QC_singleCodeError'."))
    }
    QC_singleCodeError = QC_output[singleCodeError,]
    
    multipleCodeError = which(QC_output$multipleCodeError != 0)
    if (length(multipleCodeError) > 0) {
      warning(paste("Some multiple children been wrongly coded (see 'QC_multipleCodeError'."))
    }
    QC_multipleCodeError = QC_output[multipleCodeError,] 
  }     
  
  ## RULE 8 - Sequencing of codes
  if (!is.null(sequencing) && file.exists(sequencing)) {
    cat("The sequencing file exists and is not NULL.\n")
    
    # Read the CSV file
    sequencing <- read.csv(sequencing)  # Replace with the actual read function and file parsing logic

    QC_output$gapBefore = 0
    QC_output$lastSibling = 0
    
    for (k in 1:(nrow(lengths)-1)) {
      
      if (unique(nchar(na.omit(QC_output[[paste0("segment", k+1)]]))) > 1) {
        warning(paste0("Sequencing of codes cannot be checked at level ", k+1, " as segments of code have more than one character.")) 
        QC_output$gapBefore[which(nchar(na.omit(QC_output[[paste0("segment", k+1)]])) > 1)] = NA
        QC_output$lastSibling[which(nchar(na.omit(QC_output[[paste0("segment", k+1)]])) > 1)] = NA
      }
      
      if (unique(nchar(na.omit(QC_output[[paste0("segment", k+1)]]))) == 1) {
        
        #select only parents with children
        parents_k = QC_output$Code[which(QC_output$level == k)]
        #list the no. of child for each parent then select parents with single child / or multiple children
        child_ls = sapply(unique(parents_k), function(x) length(unique(na.omit(QC_output[which(QC_output[[paste0("Code", k)]] == x),  paste0("Code", k+1)]))))
        code_multichild = QC_output[which(QC_output[[paste0("Code", k)]] %in% names(which(child_ls > 1)) & !is.na(QC_output[[paste0("Code", k+1)]]) & QC_output$level == k +1), c(paste0("Code", k), paste0("Code", k+1))]
        
        level = sequencing[k, 1]
        
        ## MultipleCode take all the code end as in the csv file #COULD BE LETTER AS WELL
        multi = as.character(sequencing[which(sequencing[,1] == level),2])
        
        #Determine the observed code end for single and multi children
        multi_code = str_sub(code_multichild[,2], nchar(code_multichild[,2]), nchar(code_multichild[,2]))
        
        #identify last code for multi children
        mcode_ls = sapply(unique(code_multichild[,1]), function(x) code_multichild[,2][which(code_multichild[,1] == x)])
        ecode_ls = lapply(mcode_ls, function(x) str_sub(x, nchar(x), nchar(x)))
        
        ## to avoid confusions I have added to take the first element only, but needs to be changed
        last_dig = unlist(lapply(ecode_ls, function(x) which(x == max(x))[1]))
        last_code = as.vector(mapply(function(x, y) x[y], mcode_ls, last_dig))
        
        #identify code with gap before
        gap_find = lapply(ecode_ls, function(x) match(multi, x))
        code_gap = lapply(gap_find, function(x) which(is.na(x)) + 1)
        gapbefore_dig = mapply(function(x, y) na.omit(x[y]), gap_find, code_gap)
        gapbefore_code = as.vector(unlist(mapply(function(x, y) x[y], mcode_ls, gapbefore_dig)))
        
        #flag in the QC_output
        QC_output$lastSibling[which(QC_output$Code %in% last_code)] = 1
        QC_output$gapBefore[which(QC_output$Code %in% gapbefore_code)] = 1
      }
      
      #identify gab before 
      gap = which(QC_output$gapBefore == 1)
      if (length(gap) > 0) {
        warning(paste("There are gab in the sequencing of multiple children coding (see 'QC_gapBefore')."))
      }
      QC_gapBefore = QC_output[gap,]
      
      #identify last sibling -
      lastSibling = which(QC_output$lastSibling == 1)
      QC_lastSibling = QC_output[lastSibling,] 
      
    }
  }

## RESULTS

return_ls <- list("QC_output" = QC_output)

# Add the result in QC_output

# Add the dataframe from swhat the user use as parameters
if (!is.null(fullHierarchy)) {
  if (!fullHierarchy) {
    # Add  codes chidless
    QC_output$childless <- QC_childless
  }
}

if (!is.null(labelUniqueness)) {
  if (!labelUniqueness) {
    # Add duplicate label
    QC_output$duplicateLabel <- QC_duplicatesLabel
  }
}

if (!is.null(labelHierarchy)) {
  if (!labelHierarchy) {
    # add singleChildMismatch
    QC_output$singleChildMismatch <- QC_singleChildMismatch
  }
}

if (!is.null(singleChildCode)) {
  # Add Single & multiple code error 
  QC_output$singleCodeError <- QC_singleCodeError
  QC_output$multipleCodeError <- QC_multipleCodeError
}

if (!is.null(sequencing)) {
  if (!missing(sequencing)) {
    # Add  sequencing 
    QC_output$lastSibling <- QC_lastSibling
  }
}

  
if (!is.null(CSVout)) {
  if (is.logical(CSVout) && CSVout == TRUE) {
    name <- names(return_ls)[1]
    date <- format(Sys.time(), "%Y%m%d%H%M%S")
    file_name <- paste0("QC_output_", "_", date, ".csv")
    path_file <- file.path(getwd(), file_name)
    write.csv(return_ls, path_file, row.names = FALSE)
  } else if (is.character(CSVout)) {
    write.csv(return_ls, CSVout, row.names = FALSE)
  }
}


   return(return_ls)
}


