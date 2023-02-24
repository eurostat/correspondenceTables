#' @title Update the correspondence table between statistical classifications A and B when A has been updated to version A*
#' @description Update the correspondence table between statistical classifications A and B when A has been updated to version A*.
#' @param A A string of the type \code{character} containing the name of a csv file that contains the original classification A.
#' @param B A string of the type \code{character} containing the name of a csv file that contains classification B.
#' @param AStar A string of the type \code{character} containing the name of a csv file that contains the updated version A*.
#' @param AB A string of the type \code{character} containing the name of a csv file that contains the previous correspondence table A:B.
#' @param AAStar A string of the type character containing the name of a csv file that contains the \emph{concordance table} A:A*,
#' which contains the mapping between the codes of the two versions of the classification.
#' @param CSVout The preferred name for the \emph{output csv files} that will contain the updated correspondence table and
#' information about the classifications involved. The valid values are \code{NULL} or strings of type \code{character}. If
#' the selected value is \code{NULL}, the default, no output file is produced. If the value is a string, then the output is
#' exported into two csv files whose names contain the provided name (see "Value" below).
#' @param Reference The reference classification among A and B. If a classification is the reference to the other, and
#' hence \emph{hierarchically superior} to it, each code of the other classification is expected to be mapped to at most one
#' code of the reference classification. The valid values are \code{"none"}, \code{"A"}, and \code{"B"}. If the selected
#' value is \code{"A"} or \code{"B"}, a "Review" flag column is included in the output (see "Explanation of the flags" below).
#' @param MismatchToleranceB The maximum acceptable proportion of rows in the updated correspondence table which contain no
#' code of the target classification B, among those which contain a code of A, of A*, or of both. The default value
#' is \code{0.2}. The valid values are real numbers in the interval [0, 1].
#' @param MismatchToleranceAStar The maximum acceptable proportion of rows in the updated correspondence table which contain
#' no code of the updated classification A*, among those which contain a code of A, of B, or of both. The default value
#' is \code{0.2}. The valid values are real numbers in the interval [0, 1].
#' @param Redundancy_trim An argument used to facilitate the trimming of the redundant records. The valid logical values are \code{TRUE} or  \code{FALSE}.
#' The default value is \code{TRUE}, which removes all redundant records, replacing the values of Acode Alabel and Asupp with the value ‘Multiple’ (to indicate that multiple A records are involved). If the multiple A records are the same, their value will not be replaced. 
#' The other values is \code{FALSE}, which shows redundant records together with the redundancy flag.
#' @export
#' @details
#' File and file name requirements:
#'     \itemize{
#'         \item The files that correspond to arguments \code{A},  \code{B}, \code{AStar}, \code{AB}, \code{AAStar} must be
#'         in \emph{csv format with comma as delimiter}. If full paths are not provided, then these files must be available
#'         in the working directory. No two filenames provided must be identical.
#'         \item If any of the two files where the output will be stored is read protected (for instance because it is open
#'         elsewhere) an error message will be reported and execution will be halted.
#'     }
#' Classification table requirements:
#'     \itemize{
#'         \item The files that correspond to arguments \code{A},  \code{B} and \code{AStar} must contain at least one column
#'         and at least two rows. The first column contains the codes of the respective classification. The first row contains
#'         column headers. The name of the first column is the name of the respective classification (e.g., "CN 2021").
#'         \item The classification codes contained in a classification file (expected in its first column as mentioned above)
#'         must be unique. No two identical codes are allowed in the column.
#'         \item If any of the files that correspond to arguments \code{A},  \code{B} and \code{AStar} has additional columns
#'         the first one of them is considered as containing the labels of the respective classification codes.
#'     }
#' Correspondence and concordance table requirements:
#'     \itemize{
#'         \item The files that correspond to arguments \code{AB} and \code{AAStar} must contain at least two columns and at least
#'         two rows. The first column of the file that corresponds to \code{AB} contains the codes of classification A. The second
#'         column contains the codes of classification B. Similar requirements apply to the file that corresponds to \code{AAStar}.
#'         The first row of each of these files contains column headers. The names of the first two columns are the names of the
#'         respective classifications.
#'         \item The pairs of classification codes contained in the concordance and the correspondence table files (expected in
#'         their first two columns as mentioned above) must be unique. No two identical pairs of codes are allowed in the first
#'         two columns.
#'     }
#' Interdependency requirements:
#'     \itemize{
#'         \item At least one code of classification A must appear in both the file of concordance table A:A* and the file of
#'         correspondence table A:B.
#'         \item	At least one code of classification A* must appear in both the file of
#'         classification A* and the file of
#'         concordance table A:A*.
#'         \item	At least one code of classification B must appear in both the file of classification B and the file of
#'         correspondence table A:B.
#'     }
#' Mismatch tolerance:
#'     \itemize{
#'         \item The ratio that is compared with \code{MismatchToleranceB} has as numerator the number of rows of the updated
#'         correspondence table which contain a code for A, for A*, or for both, but no code for B and as denominator the number of
#'         rows which contain a code for A, for A*, or for both (regardless of whether there is a code for B or not). If the ratio
#'         exceeds \code{MismatchToleranceB} the execution of the function is halted.
#'         \item The ratio that is compared with \code{MismatchToleranceAStar} has as numerator the number of rows of the updated
#'         correspondence table which contain a code for A, for B, or for both, but no code for A* and as denominator the number of
#'         rows which contain a code for A, for B*, or for both (regardless of whether there is a code
#'         for A* or not). If the ratio exceeds \code{MismatchToleranceAStar} the execution of the function is halted.
#'     }
#'
#'
#' @return
#' \code{updateCorrespondenceTable()} returns a list with two elements, both of which are data frames.
#' \itemize{
#'     \item The first element is the updated correspondence table A*:B augmented with flags "CodeChange", "Review" (if
#'     applicable), "Redundancy", "NoMatchToAStar", "NoMatchToB", "NoMatchFromAStar", "NoMatchFromB", "LabelChange", and
#'     with all the additional columns of the \code{A},  \code{B}, \code{AStar}, \code{AB} and \code{AAStar} files.
#'     \item The second element contains the names of the original classification A, the target classification B, and the
#'     updated version A*, as read from the top left-hand side cell of the respective input files.
#'     \item If the value of argument \code{CSVout} is a string of type \code{character}, the elements of the list are
#'     exported into files of csv format. The name of the file for the first element is the value of argument \code{CSVout}
#'     and the name of the file for the second element is classificationNames_\code{CSVout}. For example, if
#'     \code{CSVout} = "updateCorrespondenceTable.csv", the elements of the list are exported into
#'     "updateCorrespondenceTable.csv" and "classificationNames_updateCorrespondenceTable.csv", respectively.
#' }
#'
#' @section Explanation of the flags:
#'
#' \itemize{
#'     \item For each row of the updated correspondence table, the value of "CodeChange" is equal to \code{1} if the code of A (or A*)
#'      contained in this row maps -in this or any other row of the table- to a different code of A* (or A), otherwise the 
#'      "CodeChange" is equal to \code{0}. The value of "CodeChange" is empty if either the code of A, or the code of A*, or both are missing.
#'     \item The "Review" flag is produced only if argument \code{Reference} has been set equal to "\code{A}" or "\code{B}".
#'     For each row of the updated correspondence table, if \code{Reference} = "\code{A}" the value of "Review" is equal to
#'     \code{1} if the code of B maps to more than one code of A*, and \code{0} otherwise. If \code{Reference} = "\code{B}" the
#'     value of "Review" is equal to \code{1} if the code of A* maps to more than one code of B, and \code{0} otherwise. The value
#'     of the flag is empty if either the code of A*, or the code of B, or both are missing.
#'     \item For each row of the updated correspondence table, the value of "Redundancy" is equal to \code{1} if the row contains
#'     a combination of codes of A* and B that also appears in at least one other row of the updated correspondence table. The
#'     value of the flag is empty if both the code of A* and the code of B are missing.
#'     \item When "Redundancy_Trim" is equal to \code{FALSE} the "Redundancy_keep" flag is created to identify with value \code{1}
#'     the records that will be kept if trimming is performed.
#'     \item For each row of the updated correspondence table, the value of "NoMatchToAStar" is equal to \code{1} if there is a
#'     code for A,  for B, or for both, but no code for A*. The value of the flag is \code{0} if there are codes for both A and
#'     A* (regardless of whether there is a code for B or not). Finally, the value of "NoMatchToAStar" is empty if neither A nor B
#'     have a code in this row.
#'     \item For each row of the updated correspondence table, the value of "NoMatchToB" is equal to \code{1} if there is a code
#'     for A,  for A*, or for both, but no code for B. The value of the flag is \code{0} if there are codes for both A and B
#'     (regardless of whether there is a code for A* or not). Finally, the value of "NoMatchToB" is empty if neither A nor
#'     A* have a code in this row.
#'     \item For each row of the updated correspondence table, the value of "NoMatchFromAStar" is equal to \code{1} if the row
#'     contains a code of A* that appears in the table of classification A* but not in the concordance table A:A*. The value of
#'     the flag is \code{0} if the row contains a code of A* that appears in both the table of classification
#'     A* and the concordance table A:A*. Finally, the value of the flag is empty if the row contains no code of A* or if it
#'     contains a code of A* that appears in the concordance table A:A* but not in the table of classification A*.
#'     \item For each row of the updated correspondence table, the value of "NoMatchFromB" is equal to \code{1} if the row
#'     contains a code of B that appears in the table of classification B but not in the correspondence table A:B. The value of
#'     the flag is \code{0} if the row contains a code of B that appears in both the table of classification B and the
#'     correspondence table A:B. Finally, the value of the flag is empty if the row contains no code of B or if it contains a code
#'     of B that appears in the correspondence table A:B but not in the table of classification B.
#'     \item For each row of the updated correspondence table, the value of "LabelChange" is equal to \code{1} if the labels of
#'     the codes of A and A* are different, and \code{0} if they are the same. Finally, the value of "LabelChange" is empty if
#'     either of the labels, or both labels, are missing. Lower and upper case are considered the same, and punctuation characters
#'     are ignored when comparing code labels.
#'     \item The argument "Redundancy_trim" is used to delete all the redundancies which are mapping correctly. If the analysis 
#'    concludes that the A*code / Bcode mapping is correct for all cases involving redundancies, then an action is needed to remove 
#'    the redundancies. If the selected value is \code{TRUE}, all redundant records are removed and kept only one record for each unique 
#'    combination. For this record retained, the Acodes, the Alabel and the Asupp information is replaced with ‘multiple’. If the multiple 
#'    A records are the same, their value will not be replaced. If the selected value is \code{FALSE}, no trimming is executed so redundant 
#'    records are shown, together with the redundancy flag.
#' }
#' @section Sample datasets included in the package:
#'
#'     Running \code{browseVignettes("correspondenceTables")} in the console opens an html page in the user's default browser.
#'     Selecting HTML from the menu, users can read information about the use of the sample datasets that are included in the
#'     package.
#' If they wish to access the csv files with the sample data, users have two options:
#' \itemize{
#' \item Option 1: Unpack into any folder of their choice the tar.gz file into which the package has arrived. All sample datasets
#' may be found in the "inst/extdata" subfolder of this folder.
#' \item Option 2: Go to the "extdata" subfolder of the folder in which the package has been installed in their PC's \code{R}
#' library. All sample datasets may be found there.
#' }
#' @examples
#'  {
#'  ## Application of function updateCorrespondenceTable() with NAICS 2017 being the
#'  ## original classification A, NACE being the target classification B, NAICS 2022
#'  ## being the updated version A*, NAICS 2017:NACE being the previous correspondence
#'  ## table A:B, and NAICS 2017:NAICS 2022 being the A:A* concordance table. The desired
#'  ## name for the csv file that will contain the updated correspondence table is
#'  ## "updateCorrespondenceTable.csv", there is no reference classification, and the
#'  ## maximum acceptable proportions of unmatched codes between the original
#'  ## classification A and the target classification B, and between the original
#'  ## classification A and the updated classification A* are 0.5 and 0.3, respectively.
#'
#'  tmp_dir<-tempdir()
#'  A <- system.file("extdata", "NAICS2017.csv", package = "correspondenceTables")
#'  AStar <- system.file("extdata", "NAICS2022.csv", package = "correspondenceTables")
#'  B <- system.file("extdata", "NACE.csv", package = "correspondenceTables")
#'  AB <- system.file("extdata", "NAICS2017_NACE.csv", package = "correspondenceTables")
#'  AAStar <- system.file("extdata", "NAICS2017_NAICS2022.csv", package = "correspondenceTables")
#'
#'  UPC <- updateCorrespondenceTable(A,
#'                                   B,
#'                                   AStar,
#'                                   AB,
#'                                   AAStar,
#'                                   file.path(tmp_dir,"updateCorrespondenceTable.csv"),
#'                                   "none",
#'                                   0.5,
#'                                   0.3,
#'                                   FALSE)
#'
#'  summary(UPC)
#'  head(UPC$updateCorrespondenceTable)
#'  UPC$classificationNames
#'  csv_files<-list.files(tmp_dir, pattern = ".csv")
#'  if (length(csv_files)>0) unlink(csv_files)
#'     }


updateCorrespondenceTable <- function(A, B, AStar, AB, AAStar, CSVout = NULL, Reference = "none", MismatchToleranceB = 0.2, MismatchToleranceAStar = 0.2, Redundancy_trim = TRUE) {
  
  # Check if files exist in working directory
  test.names <- as.character(c(A, B, AStar, AB, AAStar))
  if (!all(file.exists(test.names))) {
    for (i in which(file.exists(test.names) == FALSE)) {
      stop(simpleError(paste("There is no file with name", test.names[i], "in your working directory.")))
    }
  }
  
  if (length(unique(test.names)) != 5) {
    stop(simpleError("At least two of your filenames are the same."))
  }
  
  # Check CSVout
  if (!is.null(CSVout)) {
    while (file.exists(CSVout)) {
      message(paste("Your working directory contains already a file with the name that you selected for the output file: ",
                    CSVout))
      answer <- utils::menu(c("Yes", "No"), title = "Do you want to overwrite it?")
      if (answer == 2) {
        CSVout <- readline(prompt = "Please enter a new name for the output file: ")
      }
      if (answer == 1) {
        break
      }
    }
  }
  
  # Check Redundancy (MP)
  #if (!identical(Redundancy_trim, FALSE)){
  #  stop(simpleError("You entered a non-allowed value for Redundancy_trim. The allowed values are \"TRUE\" (for trim) or \"FALSE\" (to show)."))
  #}
  
  # Check Reference
  if (!(Reference %in% c("A", "B", "none"))) {
    stop(simpleError("You entered a non-allowed value for Reference. The allowed values are \"A\", \"B\" and \"none\"."))
  }
  
  # Check MismatchToleranceB
  if (is.character(MismatchToleranceB) || MismatchToleranceB < 0 || MismatchToleranceB >
      1) {
    stop(simpleError("You entered a non-allowed value for MismatchToleranceB. The allowed values are numbers in the interval [0, 1]."))
  }
  
  # Check MismatchToleranceAStar
  if (is.character(MismatchToleranceAStar) || MismatchToleranceAStar < 0 || MismatchToleranceAStar >
      1) {
    stop(simpleError("You entered a non-allowed value for MismatchToleranceAStar. The allowed values are numbers in the interval [0, 1]."))
  }
  
  removeBOM <- function(headers) {
    gsub("\\xef\\xbb\\xbf", "", headers, useBytes = T)
  }
  # The following code lines read the classifications A, AStar, and B.
  
  classA <- utils::read.csv(A, sep = ",", header = TRUE, check.names = FALSE, colClasses = c("character"),
                            encoding = "UTF-8")
  colnames(classA) <- removeBOM(colnames(classA))
  
  classAStar <- utils::read.csv(AStar, sep = ",", header = TRUE, check.names = FALSE,
                                colClasses = c("character"), encoding = "UTF-8")
  colnames(classAStar) <- removeBOM(colnames(classAStar))
  
  classB <- utils::read.csv(B, sep = ",", header = TRUE, check.names = FALSE, colClasses = c("character"),
                            encoding = "UTF-8")
  colnames(classB) <- removeBOM(colnames(classB))
  
  # The following code lines read the correspondence tables AAStar and AB.
  
  corrAAStar <- utils::read.csv(AAStar, sep = ",", header = TRUE, check.names = FALSE,
                                colClasses = c("character"), encoding = "UTF-8")
  colnames(corrAAStar) <- removeBOM(colnames(corrAAStar))
  
  corrAB <- utils::read.csv(AB, sep = ",", header = TRUE, check.names = FALSE, colClasses = c("character"),
                            encoding = "UTF-8")
  colnames(corrAB) <- removeBOM(colnames(corrAB))
  
  # The correspondence tables without codes in A
  if (length(which(corrAAStar[, 1] == "" & corrAAStar[, 2] != "")) >= 1) {
    NoCorrAAStar <- corrAAStar[-which(corrAAStar[, 1] == "" & corrAAStar[, 2] !=
                                        ""), ]
  } else {
    NoCorrAAStar <- corrAAStar
  }
  
  if (length(which(corrAB[, 1] == "" & corrAB[, 2] != "")) >= 1) {
    NoCorrAB <- corrAB[-which(corrAB[, 1] == "" & corrAB[, 2] != ""), ]
  } else {
    NoCorrAB <- corrAB
  }
  
  # Check the dimensions of the files
  test.dimClass <- list()
  test.dimClass[[1]] = classA
  test.dimClass[[2]] = classB
  test.dimClass[[3]] = classAStar
  
  for (i in 1:3) {
    if (ncol(test.dimClass[[i]]) < 1 || nrow(test.dimClass[[i]]) < 1) {
      stop(simpleError(paste("File", test.names[i], "should have at least one column and two rows (including the row of headers).")))
    }
  }
  
  test.dimCorr <- list()
  test.dimCorr[[1]] = corrAB
  test.dimCorr[[2]] = corrAAStar
  
  for (i in 1:2) {
    if (ncol(test.dimCorr[[i]]) <= 1 || nrow(test.dimCorr[[i]]) < 1) {
      stop(simpleError(paste("File", test.names[i + 3], "should have at least two columns and two rows (including the row of headers).")))
    }
  }
  
  # Check for unique entries in classifications.
  for (i in 1:3) {
    if (sum(duplicated(test.dimClass[[i]][, 1])) >= 1) {
      stop(simpleError(paste("At least one code of ", colnames(test.dimClass[[i]])[1],
                             " appears more than once in file ", test.names[i], ". This is an error. Each code must appear only once in the file.",
                             sep = "")))
    }
  }
  
  # Check for unique entries in correspondence tables.
  for (i in 1:2) {
    if (nrow(test.dimCorr[[i]][, 1:2]) != nrow(unique(test.dimCorr[[i]][, 1:2]))) {
      stop(simpleError(paste("At least one pair of codes of ", colnames(test.dimCorr[[i]])[1],
                             " and ", colnames(test.dimCorr[[i]])[2], " appears more than once in file ",
                             test.names[i + 3], ". This is an error. Each pair of codes must appear only once in the file.",
                             sep = "")))
    }
  }
  
  # Check for at least one match in classifications and correspondence
  # tables.
  if (sum(!is.na(match(classAStar[, 1], corrAAStar[, 2]))) == 0) {
    stop(simpleError(paste("There is no code of ", colnames(classAStar)[1], " that appears in both ",
                           test.names[3], " and ", test.names[5], ". This is an error. The files should have at least one code of ",
                           colnames(classAStar)[1], " in common to allow the generation of the candidate correspondence table.",
                           sep = "")))
  }
  
  if (sum(!is.na(match(corrAAStar[, 1], corrAB[, 1]))) == 0) {
    stop(simpleError(paste("There is no code of ", colnames(corrAAStar)[1], " that appears in both ",
                           test.names[4], " and ", test.names[5], ". This is an error. The files should have at least one code of ",
                           colnames(corrAAStar)[1], " in common to allow the generation of the candidate correspondence table.",
                           sep = "")))
  }
  
  if (sum(!is.na(match(classB[, 1], corrAB[, 2]))) == 0) {
    stop(simpleError(paste("There is no code of ", colnames(classB)[1], " that appears in both ",
                           test.names[2], " and ", test.names[4], ". This is an error. The files should have at least one code of ",
                           colnames(classB)[1], " in common to allow the generation of the candidate correspondence table.",
                           sep = "")))
  }
  
  if (sum(!is.na(match(classA[, 1], corrAAStar[, 1]))) == 0) {
    message(paste("WARNING: there is no code of ", colnames(classA)[1], " that appears in both ",
                  test.names[1], " and ", test.names[5], ". When the execution of the function is over, please check the files to ensure that this is not the result of a mistake in their preparation or declaration.",
                  sep = ""))
  }
  
  if (sum(!is.na(match(classA[, 1], corrAB[, 1]))) == 0) {
    message(paste("WARNING: there is no code of ", colnames(classA)[1], " that appears in both ",
                  test.names[1], " and ", test.names[4], ". When the execution of the function is over, please check the files to ensure that this is not the result of a mistake in their preparation or declaration.",
                  sep = ""))
  }
  
  # The following if statement checks which classifications is the reference
  # one (if any). Based on which classification is the reference one (if
  # any), idx and idx.thres are created, in order to be used for the creation
  # of the 'review' flag and 'label change' flag.
  tryCatch({
    
    if (Reference == "A") {
      idx <- 2
      idx.thres <- 7
    } else if (Reference == "B") {
      idx <- 1
      idx.thres <- 7
    } else if (Reference == "none") {
      idx <- 3
      idx.thres <- 6
    }
    
    # The codeChange function constructs the 'code change' flag.
    codeChange <- function(X) {
      
      diffX <- as.matrix(X[, 1:2])
      Diff <- rep(0, nrow(diffX))
      Diff[which(diffX[, 1] != diffX[, 2])] <- 1
      difference <- data.frame(diffX, Diff)
      
      dif1 <- difference[order(difference[, 1]), ]
      dif1.values <- lapply(split(seq_along(dif1[, 1]), dif1[, 1]), function(x) {
        dif1[x, 3]
      })
      dif1 <- cbind(dif1, unlist(mapply(rep, lapply(dif1.values, max), lapply(dif1.values,
                                                                              length))))
      
      dif2 <- difference[order(difference[, 2]), ]
      dif2.values <- lapply(split(seq_along(dif2[, 2]), dif2[, 2]), function(x) {
        dif2[x, 3]
      })
      dif2 <- cbind(dif2, unlist(mapply(rep, lapply(dif2.values, max), lapply(dif2.values,
                                                                              length))))
      
      dif2.new <- dif2[order(dif2[, 1]), ]
      final.diff <- cbind(dif1, dif2.new[, 4])
      final.diff <- cbind(dif1, dif2.new[, 4], apply(final.diff[, 4:5], 1,
                                                     max))
      
      return(final.diff)
    }
    
    # The review function constructs the 'review' flag.
    review <- function(X, Y) {
      
      X1 <- X[!is.na(match(X[, 1], Y[, 1])), ]
      Y1 <- Y[!is.na(match(Y[, 1], X[, 1])), ]
      
      X1 <- X1[order(X1[, 1]), ]
      Y1 <- Y1[order(Y1[, 1]), ]
      
      x1 <- unlist(lapply(split(seq_along(X1[, 1]), X1[, 1]), length))
      y1 <- unlist(lapply(split(seq_along(Y1[, 1]), Y1[, 1]), length))
      Review <- data.frame(X1[rep(1:nrow(X1), rep(y1, x1)), 1:2], Y1[unlist(rep(split(seq_along(Y1[,
                                                                                                   1]), Y1[, 1]), x1)), 2], 0)
      colnames(Review) <- c("R1", "R2", "R3", "R4")
      
      q1 <- 0
      if (length(which(is.na(match(Y[, 1], X[, 1])) == TRUE)) >= 1) {
        Y2 <- matrix(unlist(Y[is.na(match(Y[, 1], X[, 1])), 1:2]), ncol = 2)
        Y2 <- cbind(Y2[, 1], rep("", nrow(Y2)), Y2[, 2], 0)
        colnames(Y2) <- c("R1", "R2", "R3", "R4")
        q1 <- 1
      }
      
      q2 <- 0
      if (length(which(is.na(match(X[, 1], Y[, 1])) == TRUE)) >= 1) {
        YY2 <- matrix(unlist(X[is.na(match(X[, 1], Y[, 1])), 1:2]), ncol = 2)
        YY2 <- cbind(YY2, rep("", nrow(YY2)), 0)
        colnames(YY2) <- c("R1", "R2", "R3", "R4")
        q2 <- 1
      }
      
      if (q1 == 1) {
        Review <- rbind(Review, Y2)
      }
      
      if (q2 == 1) {
        Review <- rbind(Review, YY2)
      }
      
      Review <- Review[!duplicated(Review[, c(1:3)]), ]
      
      F1 <- Review[apply(Review, 1, function(x) {
        length(which(x == ""))
      }) == 0, ]
      F2 <- Review[apply(Review, 1, function(x) {
        length(which(x == ""))
      }) >= 1, ]
      f <- stats::aggregate(unique(F1[, 2:3])[, idx], list(num = unique(F1[, 2:3])[,
                                                                                   idx]), length)[which(stats::aggregate(unique(F1[, 2:3])[, idx], list(num = unique(F1[,
                                                                                                                                                                        2:3])[, idx]), length)[, 2] > 1), 1]
      F1[which(F1[, idx + 1] %in% f), 4] <- 1
      Review <- rbind(F1, F2)
      ChangeReview <- data.frame(Review[, 1:3], 0, Review[, 4])
      ChangeReview[which(ChangeReview[, 1] %in% unique(codeChange(X)[which(codeChange(X)[,
                                                                                         6] == 1), 1])), 4] <- 1
      ChangeReview[which(ChangeReview[, 2] == ""), 4] <- ""
      
      return(ChangeReview)
    }
    
    # From this point, the 'redundancy' flag starts to be constructed.
    
    X1 <- NoCorrAAStar[!is.na(match(NoCorrAAStar[, 1], NoCorrAB[, 1])), ]
    Y1 <- NoCorrAB[!is.na(match(NoCorrAB[, 1], NoCorrAAStar[, 1])), ]
    
    X1 <- X1[order(X1[, 1]), ]
    Y1 <- Y1[order(Y1[, 1]), ]
    
    x1 <- unlist(lapply(split(seq_along(X1[, 1]), X1[, 1]), length))
    y1 <- unlist(lapply(split(seq_along(Y1[, 1]), Y1[, 1]), length))
    Redun <- data.frame(X1[rep(1:nrow(X1), rep(y1, x1)), 1:2], Y1[unlist(rep(split(seq_along(Y1[,
                                                                                                1]), Y1[, 1]), x1)), 2], 0)
    colnames(Redun) <- c("R1", "R2", "R3", "R4")
    
    q1 <- 0
    if (length(which(is.na(match(NoCorrAB[, 1], NoCorrAAStar[, 1])) == TRUE)) >=
        1) {
      Y2 <- matrix(unlist(NoCorrAB[is.na(match(NoCorrAB[, 1], NoCorrAAStar[,
                                                                           1])), 1:2]), ncol = 2)
      Y2 <- cbind(Y2[, 1], rep("", nrow(Y2)), Y2[, 2], 0)
      colnames(Y2) <- c("R1", "R2", "R3", "R4")
      q1 <- 1
    }
    
    q2 <- 0
    if (length(which(is.na(match(NoCorrAAStar[, 1], NoCorrAB[, 1])) == TRUE)) >=
        1) {
      YY2 <- matrix(unlist(NoCorrAAStar[is.na(match(NoCorrAAStar[, 1], NoCorrAB[,
                                                                                1])), 1:2]), ncol = 2)
      YY2 <- cbind(YY2, rep("", nrow(YY2)), 0)
      colnames(YY2) <- c("R1", "R2", "R3", "R4")
      q2 <- 1
    }
    
    if (q1 == 1) {
      Redun <- rbind(Redun, Y2)
    }
    
    if (q2 == 1) {
      Redun <- rbind(Redun, YY2)
    }
    
    Redun <- Redun[!duplicated(Redun[, c(1:3)]), ]
    
    F1 <- Redun[apply(Redun, 1, function(x) {
      length(which(x == ""))
    }) == 0, ]
    F2 <- Redun[apply(Redun, 1, function(x) {
      length(which(x == ""))
    }) >= 1, ]
    
    if (idx != 3) {
      
      # The 'redundancy' flag is constructed if the reference
      # classification is either A or B.
      
      f1 <- stats::aggregate(F1[, 2:3], by = F1[, 2:3], length)[1:(ncol(F1[, 2:3]) +
                                                                     1)][which(stats::aggregate(F1[, 2:3], by = F1[, 2:3], length)[1:(ncol(F1[,
                                                                                                                                              2:3]) + 1)][, 3] >= 2), 1:2]
      F1[which(apply(F1[, 2:3], 1, paste, collapse = " ") %in% apply(f1, 1,
                                                                     paste, collapse = " ")), 4] <- 1
      Redundancy <- rbind(F1, F2)
      ChangeReviewRedundancy <- data.frame(review(NoCorrAAStar, NoCorrAB),
                                           Redundancy[, 4])
      ChangeReviewRedundancy <- ChangeReviewRedundancy[order(ChangeReviewRedundancy[,
                                                                                    2], ChangeReviewRedundancy[, 3], decreasing = FALSE), ]
      List <- cbind(ChangeReviewRedundancy, 0, 0)
      List[which(ChangeReviewRedundancy[, 2] == ""), 7] <- 1
      List[which(ChangeReviewRedundancy[, 3] == ""), 8] <- 1
      colnames(List) <- c(colnames(classA[1]), colnames(classAStar[1]), colnames(classB[1]),
                          "CodeChange", "Review", "Redundancy", "NoMatchToAStar", "NoMatchToB")
      
    } else {
      
      # The 'redundancy' flag is constructed if none of classifications A
      # and B is the reference one.
      
      if (nrow(F2) >= 1) {
        F2 <- cbind(F2, 0)
        colnames(F2) <- c("R1", "R2", "R3", "R4", "R5")
      }
      f1 <- stats::aggregate(F1[, 2:3], by = F1[, 2:3], length)[1:(ncol(F1[, 2:3]) +
                                                                     1)][which(stats::aggregate(F1[, 2:3], by = F1[, 2:3], length)[1:(ncol(F1[,
                                                                                                                                              2:3]) + 1)][, 3] >= 2), 1:2]
      F1 <- data.frame(F1[, 1:3], 0, 0)
      F1[which(apply(F1[, 2:3], 1, paste, collapse = " ") %in% apply(f1, 1,
                                                                     paste, collapse = " ")), 5] <- 1
      colnames(F1) <- c("R1", "R2", "R3", "R4", "R5")
      ChangeRedundancy <- rbind(F1, F2)
      ChangeRedundancy[which(ChangeRedundancy[, 1] %in% unique(codeChange(NoCorrAAStar)[which(codeChange(NoCorrAAStar)[,
                                                                                                                       6] == 1), 1])), 4] <- 1
      ChangeRedundancy[which(ChangeRedundancy[, 2] == ""), 4] <- ""
      ChangeRedundancy <- ChangeRedundancy[order(ChangeRedundancy[, 2], ChangeRedundancy[,
                                                                                         3], decreasing = FALSE), ]
      List <- cbind(ChangeRedundancy, 0, 0)
      List[which(ChangeRedundancy[, 2] == ""), 6] <- 1
      List[which(ChangeRedundancy[, 3] == ""), 7] <- 1
      colnames(List) <- c(colnames(classA[1]), colnames(classAStar[1]), colnames(classB[1]),
                          "CodeChange", "Redundancy", "NoMatchToAStar", "NoMatchToB")
      
    }
    
    # Final table and final flags
    NoMatchFromAStar <- rep("", nrow(List))
    NoMatchFromB <- rep("", nrow(List))
    List <- cbind(List, NoMatchFromAStar, NoMatchFromB)
    
    inA1 <- which(is.na(match(classA[, 1], corrAAStar[, 1])) == TRUE)
    inA2 <- which(is.na(match(classA[, 1], corrAB[, 1])) == TRUE)
    inA <- intersect(inA1, inA2)
    if (length(inA) >= 1) {
      InA <- cbind(matrix(classA[inA, 1], length(inA), 1), matrix("", length(inA),
                                                                  2), matrix("", length(inA), idx.thres - 2))
      InA <- cbind(InA, matrix("", length(inA), 2))
      InA <- data.frame(InA)
      colnames(InA) <- colnames(List)
      List <- rbind(List, InA)
    }
    
    inAStar <- which(is.na(match(classAStar[, 1], corrAAStar[, 2])) == TRUE)
    if (length(inAStar) >= 1) {
      InAStar <- cbind(matrix("", length(inAStar), 1), matrix(classAStar[inAStar,
                                                                         1], length(inAStar), 1), matrix("", length(inAStar), idx.thres -
                                                                                                           1))
      InAStar <- cbind(InAStar, matrix("", length(inAStar), 2))
      InAStar <- data.frame(InAStar)
      colnames(InAStar) <- colnames(List)
      List <- rbind(List, InAStar)
    }
    
    noInA <- which(corrAAStar[, 1] == "" & corrAAStar[, 2] != "")
    if (length(noInA) >= 1) {
      NoInA <- cbind(matrix("", length(noInA), 1), matrix(corrAAStar[noInA,
                                                                     2], length(noInA), 1), matrix("", length(noInA), idx.thres - 1))
      NoInA <- cbind(NoInA, matrix("", length(noInA), 2))
      NoInA <- data.frame(NoInA)
      colnames(NoInA) <- colnames(List)
      List <- rbind(List, NoInA)
    }
    
    inB <- which(is.na(match(classB[, 1], corrAB[, 2])) == TRUE)
    if (length(inB) >= 1) {
      InB <- cbind(matrix("", length(inB), 2), matrix(classB[inB, 1], length(inB),
                                                      1), matrix("", length(inB), idx.thres - 2))
      InB <- cbind(InB, matrix("", length(inB), 2))
      InB <- data.frame(InB)
      colnames(InB) <- colnames(List)
      List <- rbind(List, InB)
    }
    
    noInB <- which(corrAB[, 1] == "" & corrAB[, 2] != "")
    if (length(noInB) >= 1) {
      NoInB <- cbind(matrix("", length(noInB), 2), matrix(corrAB[noInB, 2],
                                                          length(noInB), 1), matrix("", length(noInB), idx.thres - 2))
      NoInB <- cbind(NoInB, matrix("", length(noInB), 2))
      NoInB <- data.frame(NoInB)
      colnames(NoInB) <- colnames(List)
      List <- rbind(List, NoInB)
    }
    
    # The final NoMatchFrom and NoMatchTo flags are created NoMatchFrom
    yesAstarClass <- which(!is.na(match(List[, 2], classAStar[, 1])) == TRUE)
    yesAstarCorr <- which(!is.na(match(List[, 2], corrAAStar[, 2])) == TRUE)
    noAstarCorr <- which(is.na(match(List[, 2], corrAAStar[, 2])) == TRUE)
    
    List$NoMatchFromAStar[intersect(yesAstarClass, yesAstarCorr)] <- 0
    List$NoMatchFromAStar[intersect(yesAstarClass, noAstarCorr)] <- 1
    
    yesBClass <- which(!is.na(match(List[, 3], classB[, 1])) == TRUE)
    yesBCorr <- which(!is.na(match(List[, 3], corrAB[, 2])) == TRUE)
    noBCorr <- which(is.na(match(List[, 3], corrAB[, 2])) == TRUE)
    
    List$NoMatchFromB[intersect(yesBClass, yesBCorr)] <- 0
    List$NoMatchFromB[intersect(yesBClass, noBCorr)] <- 1
    
    # NoMatchTo
    noA <- which(List[, 1] == "")
    yesA <- which(List[, 1] != "")
    noAstar <- which(List[, 2] == "")
    yesAstar <- which(List[, 2] != "")
    noB <- which(List[, 3] == "")
    yesB <- which(List[, 3] != "")
    
    List$NoMatchToAStar <- 1
    List$NoMatchToAStar[intersect(intersect(noA, noB), yesAstar)] <- ""
    List$NoMatchToAStar[intersect(intersect(yesA, yesAstar), noB)] <- 0
    List$NoMatchToAStar[intersect(intersect(yesA, yesAstar), yesB)] <- 0
    
    List$NoMatchToB <- 1
    List$NoMatchToB[intersect(intersect(noA, noAstar), yesB)] <- ""
    List$NoMatchToB[intersect(intersect(yesA, yesB), noAstar)] <- 0
    List$NoMatchToB[intersect(intersect(yesA, yesAstar), yesB)] <- 0
    
    # Final review flag
    if ((Reference %in% c("A", "B"))) {
      List$Review[which(List[, 2] == "")] <- ""
      List$Review[which(List[, 3] == "")] <- ""
    }
    
    # Final redundancy flag
    List$Redundancy <- 0
    f1 <- stats::aggregate(List[, 2:3], by = List[, 2:3], length)[1:(ncol(List[, 2:3]) +
                                                                       1)][which(stats::aggregate(List[, 2:3], by = List[, 2:3], length)[1:(ncol(List[,
                                                                                                                                                      2:3]) + 1)][, 3] >= 2), 1:2]
    List$Redundancy[which(apply(List[, 2:3], 1, paste, collapse = " ") %in% apply(f1,
                                                                                  1, paste, collapse = " "))] <- 1
    List$Redundancy[intersect(which(List[, 2] == ""), which(List[, 3] == ""))] <- ""
    
  }, error = function(e) {
    stop(simpleError("An error has occurred and execution needs to stop. Please check the input data."))
  })
  
  # The following if statement checks for the maximum acceptable proportion
  # of unmatched codes between A and B, and between A and AAStar.
  
  if (length(which(List[, idx.thres] == 1))/length(which(List[, idx.thres] != "")) >
      MismatchToleranceAStar) {
    StopAAStar <- 1
  } else {
    StopAAStar <- 0
  }
  
  if (length(which(List[, idx.thres + 1] == 1))/length(which(List[, idx.thres +
                                                                  1] != "")) > MismatchToleranceB) {
    StopAB <- 1
  } else {
    StopAB <- 0
  }
  
  if (StopAAStar == 1 && StopAB == 0) {
    stop("The updated correspondence table (resulting from the joining of the concordance table A:A* with the correspondence table A:B) contains too many missing values for A*. \n Please review your input data or adjust the MismatchToleranceAStar parameter.")
  } else if (StopAAStar == 0 && StopAB == 1) {
    stop("The updated correspondence table (resulting from the joining of the concordance table A:A* with the correspondence table A:B) contains too many missing values for B. \n Please review your input data or adjust the MismatchToleranceB parameter.")
  } else if (StopAAStar == 1 && StopAB == 1) {
    stop("The updated correspondence table (resulting from the joining of the concordance table A:A* with the correspondence table A:B) contains too many missing values for A* and for B. \n Please review your input data or adjust the MismatchToleranceAStar and MismatchToleranceB parameters.")
  } else if (StopAAStar == 0 && StopAB == 0) {
    
    # The following if statements checks if there are any label as well as
    # supplementary columns in the classifications A, AStar, B, and in the
    # correspondence tables AAStar and AB, in order to include them in the
    # final table.
    tryCatch({
      
      if (ncol(classA) >= 2) {
        A1 <- as.matrix(classA[match(List[, 1], unlist(classA[, 1])), 2:ncol(classA)])
        A1[is.na(A1)] <- ""
        colnames(A1) <- paste(colnames(classA)[1], colnames(classA)[2:ncol(classA)],
                              sep = "_")
        List <- cbind(List, A1)
      }
      
      if (ncol(classAStar) >= 2) {
        A2 <- as.matrix(classAStar[match(List[, 2], unlist(classAStar[, 1])),
                                   2:ncol(classAStar)])
        A2[is.na(A2)] <- ""
        colnames(A2) <- paste(colnames(classAStar)[1], colnames(classAStar)[2:ncol(classAStar)],
                              sep = "_")
        List <- cbind(List, A2)
      }
      
      if (ncol(classB) >= 2) {
        B1 <- as.matrix(classB[match(List[, 3], unlist(classB[, 1])), 2:ncol(classB)])
        B1[is.na(B1)] <- ""
        colnames(B1) <- paste(colnames(classB)[1], colnames(classB)[2:ncol(classB)],
                              sep = "_")
        List <- cbind(List, B1)
      }
      
      
      if (ncol(corrAAStar) >= 3) {
        AA1 <- as.matrix(corrAAStar[match(data.frame(t(List[, c(1, 2)])),
                                          data.frame(t(corrAAStar[, 1:2]))), 3:ncol(corrAAStar)])
        AA1[is.na(AA1)] <- ""
        colnames(AA1) = paste(paste(colnames(corrAAStar)[1], colnames(corrAAStar)[2],
                                    sep = " - "), colnames(corrAAStar)[3:ncol(corrAAStar)], sep = "_")
        List <- cbind(List, AA1)
      }
      
      if (ncol(corrAB) >= 3) {
        AB1 <- as.matrix(corrAB[match(data.frame(t(List[, c(1, 3)])), data.frame(t(corrAB[,
                                                                                          1:2]))), 3:ncol(corrAB)])
        AB1[is.na(AB1)] <- ""
        colnames(AB1) = paste(paste(colnames(corrAB)[1], colnames(corrAB)[2],
                                    sep = " - "), colnames(corrAB)[3:ncol(corrAB)], sep = "_")
        List <- cbind(List, AB1)
      }
      
    }, error = function(e) {
      stop(simpleError("An error has occurred and execution needs to stop. Please check the input data."))
    })
    
  }
  
  
  # The 'label change' flag is constructed.
  tryCatch({
    
    if (ncol(classA) >= 2 && ncol(classAStar) >= 2) {
      LabelChange <- rep(1, nrow(List))
      LabelChange[which(tolower(gsub("[[:punct:] ]+", " ", List[, idx.thres +
                                                                  4])) == tolower(gsub("[[:punct:] ]+", " ", List[, idx.thres + 4 +
                                                                                                                    ncol(classA) - 1])))] <- 0
      LabelChange[which(List[, idx.thres + 4] == "")] <- ""
      LabelChange[which(List[, idx.thres + 4 + ncol(classA) - 1] == "")] <- ""
      List <- cbind(List[, 1:(idx.thres + 3)], LabelChange, List[, (idx.thres +
                                                                      4):ncol(List)])
    }
  })
  
  # Redundancy_trim parameter (MP)
  if (Redundancy_trim == TRUE){
    # Redundancy_trim parameter (MP)
    # Find the columns which are related to dataset A (col 1) which values need to be recorded as "Multiple"
    col_multiple = c(grep(colnames(List)[1], colnames(List), value = T),  grep("LabelChange", colnames(List), value = T))
    
    ############################################################
    # Find unique combination of A and B and identify them with a number
    uniqueAstarB = unique(List[which(List$Redundancy == 1),c(2, 3)])
    uniqueAstarB$id_to_use = 1:nrow(uniqueAstarB)
    
    List = merge(List, uniqueAstarB, by = colnames(List)[c(2,3)], sort = FALSE, all.x = TRUE)[, union(names(List), names(uniqueAstarB))]
    col_link = grep("id_to_use", colnames(List), value = T)
    
    x_temp = split(List[which(List$Redundancy == 1), col_multiple], List[which(List$Redundancy == 1),col_link])
    
    for (i in 1:nrow(uniqueAstarB)){
      multiple_values = apply(x_temp[[i]], 2, function(x) length(unique(x)))
      x_change = which(multiple_values != 1)
      #replace with multiple
      col_multiple_temp = c(col_multiple)[x_change]
      List[which(List$Redundancy == 1 & List[,col_link] == i), col_multiple_temp] = "Multiple"
    }
    
    List = List[,!names(List) %in% "id_to_use"]
    
    ############################################################
    ### old but probably faster  
    ##replace with multiple
    #List[which(List$Redundancy == 1), col_multiple] = "Multiple"
    #List[which(List$Redundancy == 1), grep("LabelChange", colnames(List), value = T)] = 1
    
    #eliminate duplicates
    dup = as.numeric(duplicated(List[,1:3]))
    List = List[which(dup == 0), ]
    
    #change LabelChange to 1 if multiple
    List[which(List$LabelChange == "Multiple"), grep("LabelChange", colnames(List), value = T)] = 1
  }
  
  if (Redundancy_trim == FALSE){
    #add a redundancy keep flag to indicate which row will be kept
    dup = as.numeric(duplicated(List[,c(2,3)]))
    red_col = which(colnames(List) == "Redundancy")
    List$Redundancy_keep = rep(0, nrow(List))
    List$Redundancy_keep[which(dup == "0" & List$Redundancy == "1")] = 1
    List = List[,c(1:red_col, ncol(List), (red_col+1):(ncol(List)-1))]
  }
  
  tryCatch({
    # A data frame that contains the names of the classifications A, AStar,
    # and B is constructed.
    
    CsvNames <- data.frame(matrix(0, 3, 1))
    
    CsvNames[1, 1] <- paste("A:", colnames(List)[1], sep = " ")
    
    CsvNames[2, 1] <- paste("B:", colnames(List)[3], sep = " ")
    
    CsvNames[3, 1] <- paste("AStar:", colnames(List)[2], sep = " ")
    
    CsvNames <- data.frame(CsvNames)
    
    
    ##Added condition when CSVout is null (MP)
    if (!is.null(CSVout)) {
      
      pos <- regexpr("\\/[^\\/]*$", CSVout)
      Name1 <- substr(CSVout, 1, pos[[1]])
      Name2 <- substr(CSVout, pos[[1]] + 1, nchar(CSVout))
      
      pos <- regexpr("\\.[^\\.]*$", Name2)
      if (pos[[1]] == -1) {
        Name <- substr(Name2, pos[[1]] + 1, nchar(Name2))
      } else {
        Name <- substr(Name2, 1, pos[[1]] - 1)
      }
      
      colnames(CsvNames) <- paste("Classification:", "Name", sep = " ")
    }
    
    Final <- apply(List, 2, function(x) {
      gsub(" ", " ", x)
    })
    
    if (is.null(dim(Final))) {
      Final <- t(data.frame(Final))
      rownames(Final) <- 1
    }
    
  }, error = function(e) {
    stop(simpleError("An error has occurred and execution needs to stop. Please check the input data."))
  })
  
  
  # The following if statement check if the user wants a .csv file of the
  # final table.  If yes, a .csv file that contains the names of the
  # classifications A, AStar, and B is created as well.
  
  tryCatch({
    
    if (!is.null(CSVout)) {
      data.table::fwrite(data.frame(Final, check.names = FALSE), file = CSVout, quote = TRUE)
      utils::write.csv(CsvNames, file = paste0(Name1, "classificationNames_", Name2),
                       row.names = FALSE)
    }
    
  }, error = function(e) {
    stop(simpleError("An error occurred while trying to write the output to the specified files. Please check the respective input parameters."))
  })
  
  # The following list contains the final table as well as the data frame
  # that contains the names of the classifications A, AStar, and B in a list.
  # This list is the output of the updateCorrespondenceTable function.
  
  tryCatch({
    
    FinalResults <- list()
    FinalResults[[1]] <- data.frame(Final, check.names = FALSE, row.names = NULL)
    FinalResults[[2]] <- CsvNames
    names(FinalResults) <- c("updateCorrespondenceTable", "classificationNames")
    
    return(FinalResults)
    
  }, error = function(e) {
    stop(simpleError("An error has occurred and execution needs to stop. Please check the input data."))
  })
  
}
