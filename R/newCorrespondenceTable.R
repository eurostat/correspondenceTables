#' @title Ex novo creation of candidate correspondence tables between two classifications via pivot tables
#' @description Creation of a candidate correspondence table between two classifications, A and B, when there are
#'   correspondence tables leading from the first classification to the second one via \eqn{k} intermediate pivot
#'   classifications \eqn{C_1, \ldots, C_k}. 
#'   The correspondence tables leading from A to B are A:\eqn{C_1}, \{\eqn{C_i}:\eqn{C_{i+1}}: \eqn{1 \le i \le k -1}\}, B:\eqn{C_k}.
#' @param Tables A string of type character containing the name of a csv file which contains the names of the files that
#'   contain the classifications and the intermediate correspondence tables (see "Details" below).
#' @param CSVout The preferred name for the \emph{output csv files} that will contain the candidate correspondence table
#'   and information about the classifications involved. The valid values are \code{NULL} or strings of type \code{character}. 
#'   If the selected value is \code{NULL}, the default, no output file is produced. If the value is a string, then the output 
#'   is exported into two csv files whose names contain the provided name (see "Value" below).
#' @param Reference The reference classification among A and B. If a classification is the reference to the other, and hence
#'  \emph{hierarchically superior} to it, each code of the other classification is expected to be mapped to at most one code
#'   of the reference classification. The valid values are \code{"none"}, \code{"A"}, and \code{"B"}. If the selected value
#'   is \code{"A"} or \code{"B"}, a "Review" flag column (indicating the records violating this expectation) is included
#'   in the output (see "Explanation of the flags" below).
#' @param MismatchTolerance The maximum acceptable proportion of rows in the candidate correspondence table which contain
#'   no code for classification A or no code for classification B. The default value is \code{0.2}. The valid values are
#'   real numbers in the interval [0, 1].
#' @param Redundancy_trim An argument in the function containing the logical values \code{TRUE} or \code{FALSE} 
#' used to facilitate the trimming of the redundant records.
#' The default value is \code{TRUE}, which removes all redundant records. 
#' The other values is \code{FALSE}, which shows redundant records together with the redundancy flag.
#' @export
#' @details
#' File and file name requirements:
#'     \itemize{
#'         \item The file that corresponds to argument \code{Tables} and the files to which the contents of \code{Tables}
#'         lead, must be in \emph{csv format with comma as delimiter}. If full paths are not provided, then these files must
#'          be available in the working directory. No two filenames provided must be identical.
#'         \item The file that corresponds to argument \code{Tables} must contain filenames, \emph{and nothing else}, in
#'         a \eqn{(k+2)} × \eqn{(k+2)} table, where \eqn{k}, a positive integer, is the number of "pivot" classifications.
#'         The cells in the main diagonal of the table provide the filenames of the files which contain, with this order,
#'         the classifications A, \eqn{C_1}, \eqn{\ldots}, \eqn{C_k} and B. The off-diagonal directly above the main
#'         diagonal contains the filenames of the files that contain, with this order, the correspondence tables
#'         A:\eqn{C_1}, \{\eqn{C_i}:\eqn{C_{i+1}}, \eqn{1 \le i \le k-1}\} and B:\eqn{C_k}. All other cells of the table
#'         must be empty.
#'         \item If any of the two files where the output will be stored is read protected (for instance because it is open
#'         elsewhere) an error message will be reported and execution will be halted.
#'     }
#' Classification table requirements:
#'     \itemize{
#'         \item Each of the files that contain classifications must contain at least one column and at least two rows.
#'         The first column contains the codes of the respective classification. The first row contains column headers.
#'         The header of the first column is the name of the respective classification (e.g., "CN 2021").
#'         \item The classification codes contained in a classification file (expected in its first column as mentioned
#'         above) must be unique. No two identical codes are allowed in the column.
#'         \item If any of the files that contain classifications has additional columns the first one of them is assumed
#'         to contain the labels of the respective classification codes.
#'     }
#' Correspondence table requirements:
#'     \itemize{
#'         \item The files that contain correspondence tables must contain at least two columns and at least two rows.
#'         The first column of the file that contains A:\eqn{C_1} contains the codes of classification A. The second column
#'         contains the codes of classification \eqn{C_1}. Similar requirements apply to the files that contain
#'         \eqn{C_i}:\eqn{C_{i+1}}, \eqn{1 \le i \le k-1} and B:\eqn{C_k}. The first row of each of the files that contain
#'         correspondence tables contains column headers. The names of the first two columns are the names of the respective
#'         classifications.
#'         \item The pairs of classification codes contained in a correspondence table file (expected in its first two columns
#'         as mentioned above) must be unique. No two identical pairs of codes are allowed in the first two columns.
#'     }
#' Interdependency requirements:
#'     \itemize{
#'         \item At least one code of classification A must appear in both the file of classification A and the file of
#'         correspondence table A:\eqn{C_1}.
#'         \item At least one code of classification B must appear in both the file of classification B and the file of
#'         correspondence table B:\eqn{C_k}, where \eqn{k}, \eqn{k\ge 1}, is the number of pivot classifications.
#'         \item If there is only one pivot classification, \eqn{C_1}, at least one code of it must appear in both the file of
#'         correspondence table A:\eqn{C_1} and the file of correspondence table B:\eqn{C_1}.
#'         \item If the pivot classifications are \eqn{k} with \eqn{k\ge 2} then at least one code of \eqn{C_1} must appear in
#'         both the file of correspondence table A:\eqn{C_1} and the file of correspondence table \eqn{C_1}:\eqn{C_2}, at least one 
#'         code of each of the \eqn{C_i}, \eqn{i = 2, \ldots, k-1} (if \eqn{k\ge 3}) must appear in both the file of correspondence 
#'         table \eqn{C_{i-1}}:\eqn{C_i} and the file of correspondence table \eqn{C_i}:\eqn{C_{i+1}}, and at least one code of 
#'         \eqn{C_k} must appear in both the file of correspondence table \eqn{C_{k-1}}:\eqn{C_k} and the file of correspondence table 
#'         B:\eqn{C_k}.
#'     }
#' Mismatch tolerance:
#'     \itemize{
#'         \item The ratio that is compared with \code{MismatchTolerance} has as numerator the number of rows in the candidate
#'         correspondence table which contain no code for classification A or no code for classification B and as denominator
#'         the total number of rows of this table. If the ratio exceeds \code{MismatchTolerance} the execution of the function
#'         is halted.
#'     }
#' If any of the conditions required from the arguments is violated an error message is produced and execution is stopped.
#'
#' @section Explanation of the flags:
#'
#' \itemize{
#'     \item  The "Review" flag is produced only if argument Reference has been set equal to "\code{A}" or "\code{B}". For
#'     each row of the candidate correspondence table, if \code{Reference} = "\code{A}" the value of "Review" is equal to
#'     \code{1} if the code of B maps to more than one code of A, and \code{0} otherwise. If \code{Reference} = "\code{B}"
#'     the value of "Review" is equal to \code{1} if the code of A maps to more than one code of B, and \code{0} otherwise.
#'     The value of the flag is empty if the row does not contain a code of A or a code of B.
#'     \item For each row of the candidate correspondence table, the value of "Redundancy" is equal to \code{1} if the row
#'     contains a combination of codes of A and B that also appears in at least one other row of the candidate
#'     correspondence table.
#'     \item When "Redundancy_Trim" is equal to \code{FALSE} the "Redundancy_keep" flag is created to identify with value \code{1}
#'     the records that will be kept if trimming is performed.
#'     \item For each row of the candidate correspondence table, the value of "Unmatched" is equal to \code{1} if the row
#'     contains a code of A but no code of B or if it contains a code of B but no code of A. The value of the flag is
#'     \code{0} if the row contains codes for both A and B.
#'     \item For each row of the candidate correspondence table, the value of "NoMatchFromA" is equal to \code{1} if the row
#'     contains a code of A that appears in the table of classification A but not in correspondence table A:\eqn{C_1}. The
#'     value of the flag is \code{0} if the row contains a code of A that appears in both the table of classification A and
#'     correspondencetable A:\eqn{C_1}. Finally, the value of the flag is empty if the row contains no code of A or if it
#'     contains a code of A that appears in correspondence table A:\eqn{C_1} but not in the table of classification A.
#'     \item For each row of the candidate correspondence table, the value of "NoMatchFromB" is equal to \code{1} if the row
#'     contains a code of B that appears in the table of classification B but not in correspondence table B:\eqn{C_k}. The
#'     value of the flag is \code{0} if the row contains a code of B that appears in both the table of classification B and
#'     correspondence table B:\eqn{C_k}. Finally, the value of the flag is empty if the row contains no code of B or if it
#'     contains a code of B that appears in correspondence table B:\eqn{C_k} but not in the table of classification B.
#'     \item The argument "Redundancy_trim" is used to delete all the redundancies which are mapping correctly. 
#'     The valid logical values for this argument in the candidate correspondence table are \code{TRUE} or \code{FALSE}. 
#'     If the selected value is \code{TRUE}, all redundant records are removed and kept exactly one record for each unique combination. 
#'     For this retained record, the codes, the label and the supplementary information of the pivot classifications are replaced with 
#'     'multiple'. If the multiple infomration of the pivot classifications are the same, their value will not be replaced.
#'     If the selected value is \code{FALSE}, no trimming is executed so redundant records are shown, together with the redundancy flag.  
#'     If the logical values are missing the implementation of the function will stop.
#'      
#' }
#'
#' @section Sample datasets included in the package:
#'
#' Running \code{browseVignettes("correspondenceTables")} in the console opens an html page in the user's default browser. Selecting HTML from the menu, users can read information about the use of the sample datasets that are included in the package.
#' If they wish to access the csv files with the sample data, users have two options:
#' \itemize{
#' \item Option 1: Unpack into any folder of their choice the tar.gz file into which the package has arrived. All sample
#' datasets may be found in the "inst/extdata" subfolder of this folder.
#' \item Option 2: Go to the "extdata" subfolder of the folder in which the package has been installed in their PC's \code{R}
#' library. All sample datasets may be found there.
#' }
#'
#' @return
#'  \code{newCorrespondenceTable()} returns a list with two elements, both of which are data frames.
#' \itemize{
#'     \item The first element is the candidate correspondence table A:B, including the codes of all "pivot" classifications,
#'      augmented with flags "Review" (if applicable), "Redundancy", "Unmatched", "NoMatchFromA", "NoMatchFromB" and with all
#'      the additional columns of the classification and intermediate correspondence table files.
#'     \item The second element contains the names of classification A, the "pivot" classifications and classification B as
#'     read from the top left-hand side cell of the respective input files.
#'     \item If the value of argument \code{CSVout} a string of type \code{character}, the elements of the list are exported
#'     into files of csv format. The name of the file for the first element is the value of argument \code{CSVout} and the
#'     name of the file for the second element is classificationNames_\code{CSVout}. For example, if
#'     \code{CSVout} = "newCorrespondenceTable.csv", the elements of the list are exported into "newCorrespondenceTable.csv"
#'     and "classificationNames_newCorrespondenceTable.csv" respectively.
#' }
#'
#' @examples
#' {
#'    ## Application of function newCorrespondenceTable() with "example.csv" being the file
#'    ## that includes the names the files  and the intermediate tables in a sparse square
#'    ## matrix containing the 100 rows of the classifications (from ISIC v4 to CPA v2.1 through
#'    ## CPC v2.1). The desired name for the csv file that will contain the candidate
#'    ## correspondence table is "newCorrespondenceTable.csv", the reference classification is
#'    ## ISIC v4 ("A") and the maximum acceptable proportion of unmatched codes between
#'    ## ISIC v4 and CPC v2.1 is 0.56 (this is the minimum mismatch tolerance for the first 100 row
#'    ## as 55.5% of the code of ISIC v4 is unmatched).
#'
#'      tmp_dir<-tempdir()
#'      A <- read.csv(system.file("extdata", "example.csv", package = "correspondenceTables"),
#'                    header = FALSE,
#'                    sep = ",")
#'      for (i in 1:nrow(A)) {
#'        for (j in 1:ncol(A)) {
#'          if (A[i,j]!="") {
#'            A[i, j] <- system.file("extdata", A[i, j], package = "correspondenceTables")
#'        }}}
#'      write.table(x = A,
#'                  file = file.path(tmp_dir,"example.csv"),
#'                  row.names = FALSE,
#'                  col.names = FALSE,
#'                  sep = ",")
#'
#'      NCT<-newCorrespondenceTable(file.path(tmp_dir,"example.csv"),
#'                                  file.path(tmp_dir,"newCorrespondenceTable.csv"),
#'                                  "A",
#'                                  0.56, 
#'                                  FALSE)
#'
#'      summary(NCT)
#'      head(NCT$newCorrespondenceTable)
#'      NCT$classificationNames
#'      csv_files<-list.files(tmp_dir, pattern = ".csv")
#'      unlink(csv_files)
#'     }

newCorrespondenceTable <- function(Tables, CSVout = NULL, Reference = "none", MismatchTolerance = 0.2, Redundancy_trim = TRUE) {
  
  # Check if the file that contains the names of both classifications and
  # correspondence tables exists in working directory
  if (!file.exists(Tables)) {
    stop(simpleError(paste("There is no file with name", Tables, "in your working directory.")))
  } else {
    # x <- as.matrix(utils::read.csv(Tables, sep = ",", header = FALSE, colClasses = c("character"),
    #                                 encoding = "UTF-8"))
    x <- as.matrix(data.table::fread(Tables, sep = ",", header = FALSE, colClasses = c("character"),
                                     encoding = "UTF-8"))
    mat.list <- apply(x, 2, function(x) {
      as.character(which(x != ""))
    })
  }
  
  # Check if files exist in working directory
  test.names <- as.vector(x)[which(as.vector(x) != "")]
  if (!all(file.exists(test.names))) {
    for (i in which(file.exists(test.names) == FALSE)) {
      stop(simpleError(paste("The is no file with name", test.names[i], "in your working directory.")))
    }
  }
  
  if (length(which(duplicated(test.names) == TRUE)) >= 1) {
    stop(simpleError(paste("At least two of the filenames in", Tables, "are the same.")))
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
  
  
  # Check Reference
  if (!(Reference %in% c("A", "B", "none"))) {
    stop(simpleError("You entered a non-allowed value for Reference. The allowed values are \"A\", \"B\" and \"none\"."))
  }
  
  # Check MismatchTolerance
  if (is.character(MismatchTolerance) || MismatchTolerance < 0 || MismatchTolerance >
      1) {
    stop(simpleError("You entered a non-allowed value for MismatchTolerance. The allowed values are numbers in the interval [0, 1]."))
  }
  
  
  test.list <- list()
  test.list[[1]] <- "1"
  for (mat.index in 2:ncol(x)) {
    test.list[[mat.index]] <- as.character(c((mat.index - 1):mat.index))
  }
  
  # The following if statement checks if the names of both classifications
  # and correspondence tables in the 'names.csv' file construct a sparse
  # square matrix.
  if (all(unlist(Map(identical, mat.list, test.list)) == TRUE) && nrow(x) >= 3) {
    k <- nrow(x) - 2
    
  } else {
    # Error message in case the names of both classifications and
    # correspondence tables in the 'names.csv' file do not construct a
    # sparse square matrix.
    stop(paste("The filenames in", Tables, "do not construct a sparse square matrix. \n Please verify that the appropriate number of filenames are inserted in the appropriate cells."))
  }
  
  # The list inputs includes the names of both classifications and
  # correspondence tables.
  inputs <- list()
  inputs[[1]] <- diag(x)[1]
  inputs[seq(k) + 1] = as.list(diag(x)[seq(k) + 1])
  inputs[[k + 2]] <- diag(x)[length(diag(x))]
  inputs[(k + 3):(k + 2 + length(as.list(x[upper.tri(x)][x[upper.tri(x)] != ""])))] <- as.list(x[upper.tri(x)][x[upper.tri(x)] !=
                                                                                                                 ""])
  
  # Create a list of the classifications and the known correspondence tables
  # as data frames.
  RRR <- lapply(inputs[1:length(inputs)], function(x) {
    utils::read.csv(x, sep = ",", check.names = FALSE, colClasses = c("character"),
                    encoding = "UTF-8")
    # data.table::fread(x, sep = ",", check.names = FALSE, colClasses = c("character"),
    # encoding = "UTF-8")
  })
  
  removeBOM <- function(headers) {
    gsub("\\xef\\xbb\\xbf", "", headers, useBytes = T)
  }
  
  for (i in 1:length(RRR)) {
    colnames(RRR[[i]]) <- removeBOM(colnames(RRR[[i]]))
  }
  
  # Convert data frames into matrices.
  RR <- lapply(RRR, function(x) {
    matrix(unlist(x), ncol = ncol(x))
  })
  
  # Select the correspondence tables.
  R <- RR[utils::tail(c(1:length(RR)), (length(RR) - 1)/2)]
  
  # Check the dimensions of the files
  for (i in 1:nrow(x)) {
    if (ncol(RRR[[i]]) < 1 || nrow(RRR[[i]]) < 1) {
      stop(simpleError(paste("File", inputs[i], "should have at least one column and two rows (including the row of headers).")))
    }
  }
  
  for (i in 1:length(R)) {
    if (ncol(R[[i]]) <= 1 || nrow(R[[i]]) < 1) {
      stop(simpleError(paste("File", inputs[i + nrow(x)], "should have at least two columns and two rows (including the row of headers).")))
    }
  }
  
  # Check for entries dimensions of the files
  for (i in 1:nrow(x)) {
    if (sum(duplicated(RRR[[i]][, 1])) >= 1) {
      stop(simpleError(paste("At least one code of ", colnames(RRR[[i]])[1],
                             " appears more than once in file ", inputs[i], ". This is an error. Each code must appear only once in the file.",
                             sep = "")))
    }
  }
  
  for (i in 1:length(R)) {
    if (nrow(unique(R[[i]][, 1:2])) != nrow(R[[i]][, 1:2])) {
      stop(simpleError(paste("At least one pair of codes of ", colnames(RRR[[i +
                                                                               nrow(x)]])[1], " and ", colnames(RRR[[i + nrow(x)]])[2], " appears more than once in file ",
                             inputs[i + nrow(x)], ". This is an error. Each pair of codes must appear only once in the file.",
                             sep = "")))
    }
  }
  
  # Check for at least one match in classifications and correspondence
  # tables. In inputs there are the names of both classifications and
  # correspondence tables. Stop with error
  if (k == 1) {
    # A in A appears in A:C1
    if (sum(!is.na(match(unlist(RRR[[1]][, 1]), R[[1]][, 1]))) == 0) {
      stop(simpleError(paste("There is no code of ", colnames(RRR[[1]])[1],
                             " that appears in both ", inputs[1], " and ", inputs[1 + nrow(x)],
                             ". This is an error. The files should have at least one code of ",
                             colnames(RRR[[1]])[1], " in common to allow the generation of the candidate correspondence table.",
                             sep = "")))
    }
    
    # C1 in A:C1 appears in B:C1
    if (sum(!is.na(match(R[[1]][, 2], R[[2]][, 2]))) == 0) {
      stop(simpleError(paste("There is no code of ", colnames(RRR[[1 + nrow(x)]])[2],
                             " that appears in both ", inputs[1 + nrow(x)], " and ", inputs[2 +
                                                                                              nrow(x)], ". This is an error. The files should have at least one code of ",
                             colnames(RRR[[1 + nrow(x)]])[2], " in common to allow the generation of the candidate correspondence table.",
                             sep = "")))
    }
    
    # B in B:C1 appears in B
    if (sum(!is.na(match(R[[length(R)]][, 1], unlist(RRR[[nrow(x)]][, 1])))) == 0) {
      stop(simpleError(paste("There is no code of ", colnames(RRR[[length(R) +
                                                                     nrow(x)]])[1], " that appears in both ", inputs[nrow(x)], " and ",
                             inputs[length(R) + nrow(x)], ". This is an error. The files should have at least one code of ",
                             colnames(RRR[[length(R) + nrow(x)]])[1], " in common to allow the generation of the candidate correspondence table.",
                             sep = "")))
    }
    
  }
  
  if (k >= 2) {
    
    # A in A appears in A:C1
    if (sum(!is.na(match(unlist(RRR[[1]][, 1]), R[[1]][, 1]))) == 0) {
      stop(simpleError(paste("There is no code of ", colnames(RRR[[1]])[1],
                             " that appears in both ", inputs[1], " and ", inputs[1 + nrow(x)],
                             ". This is an error. The files should have at least one code of ",
                             colnames(RRR[[1]])[1], " in common to allow the generation of the candidate correspondence table.",
                             sep = "")))
    }
    
    # C1 in A:C1 appears in C1:C2 C2 in C1:C2 appears in C2:C3 ...
    for (i in 1:(k - 1)) {
      
      if (sum(!is.na(match(R[[i]][, 2], R[[i + 1]][, 1]))) == 0) {
        stop(simpleError(paste("There is no code of ", colnames(RRR[[i +
                                                                       nrow(x)]])[2], " that appears in both ", inputs[i + nrow(x)], " and ",
                               inputs[i + 1 + nrow(x)], ". This is an error. The files should have at least one code of ",
                               colnames(RRR[[i + nrow(x)]])[2], " in common to allow the generation of the candidate correspondence table.",
                               sep = "")))
      }
      
    }
    
    # Ck in C(k-1):Ck appears in B:Ck
    if (sum(!is.na(match(R[[k]][, 2], R[[k + 1]][, 2]))) == 0) {
      stop(simpleError(paste("There is no code of ", colnames(RRR[[k + nrow(x)]])[2],
                             " that appears in both ", inputs[k + nrow(x)], " and ", inputs[k +
                                                                                              1 + nrow(x)], ". This is an error. The files should have at least one code of ",
                             colnames(RRR[[k + nrow(x)]])[2], " in common to allow the generation of the candidate correspondence table.",
                             sep = "")))
    }
    
    # B in B:Ck appears in B
    if (sum(!is.na(match(R[[length(R)]][, 1], unlist(RRR[[nrow(x)]][, 1])))) == 0) {
      stop(simpleError(paste("There is no code of ", colnames(RRR[[length(R) +
                                                                     nrow(x)]])[1], " that appears in both ", inputs[nrow(x)], " and ",
                             inputs[length(R) + nrow(x)], ". This is an error. The files should have at least one code of ",
                             colnames(RRR[[length(R) + nrow(x)]])[1], " in common to allow the generation of the candidate correspondence table.",
                             sep = "")))
    }
    
  }
  
  # Warning
  if (k == 1) {
    
    # C1 in C1 appears in A:C1
    if (sum(!is.na(match(unlist(RRR[[2]][, 1]), R[[1]][, 2]))) == 0) {
      message(paste("WARNING: there is no code of ", colnames(RRR[[2]])[1], " that appears in both ",
                    inputs[2], " and ", inputs[1 + nrow(x)], ". When the execution of the function is over, please check the files to ensure that this is not the result of a mistake in their preparation or declaration.\n",
                    sep = ""))
    }
    
    # C1 in C1 appears in B:C1
    if (sum(!is.na(match(unlist(RRR[[2]][, 1]), R[[2]][, 2]))) == 0) {
      message(paste("WARNING: there is no code of ", colnames(RRR[[2]])[1], " that appears in both ",
                    inputs[2], " and ", inputs[2 + nrow(x)], ". When the execution of the function is over, please check the files to ensure that this is not the result of a mistake in their preparation or declaration.\n",
                    sep = ""))
    }
    
  }
  
  if (k == 2) {
    
    for (i in 2:k) {
      
      # C1 in C1 appears in A:C1 C2 in C2 appears in C1:C2 C3 in C3
      # appears in C2:C3
      if (sum(!is.na(match(unlist(RRR[[i]][, 1]), R[[i - 1]][, 2]))) == 0) {
        message(paste("WARNING: there is no code of ", colnames(RRR[[i]])[1],
                      " that appears in both ", inputs[i], " and ", inputs[i - 1 + nrow(x)],
                      ". When the execution of the function is over, please check the files to ensure that this is not the result of a mistake in their preparation or declaration.\n",
                      sep = ""))
      }
      
      # C1 in C1 appears in C1:C2 C2 in C2 appears in C2:C3 C3 in C3
      # appears in C3:C4
      if (sum(!is.na(match(unlist(RRR[[i]][, 1]), R[[i]][, 1]))) == 0) {
        message(paste("WARNING: there is no code of ", colnames(RRR[[i]])[1],
                      " that appears in both ", inputs[i], " and ", inputs[i + nrow(x)],
                      ". When the execution of the function is over, please check the files to ensure that this is not the result of a mistake in their preparation or declaration.\n",
                      sep = ""))
      }
    }
    
    # Ck in Ck appears in C(k-1):Ck
    if (sum(!is.na(match(unlist(RRR[[k + 1]][, 1]), R[[k]][, 2]))) == 0) {
      message(paste("WARNING: there is no code of ", colnames(RRR[[k + 1]])[1],
                    " that appears in both ", inputs[k + 1], " and ", inputs[k + nrow(x)],
                    ". When the execution of the function is over, please check the files to ensure that this is not the result of a mistake in their preparation or declaration.\n",
                    sep = ""))
    }
    
    # Ck in Ck appears in B:Ck
    if (sum(!is.na(match(unlist(RRR[[k + 1]][, 1]), R[[k + 1]][, 2]))) == 0) {
      message(paste("WARNING: there is no code of ", colnames(RRR[[k + 1]])[1],
                    " that appears in both ", inputs[k + 1], " and ", inputs[k + 1 +
                                                                               nrow(x)], ". When the execution of the function is over, please check the files to ensure that this is not the result of a mistake in their preparation or declaration.\n",
                    sep = ""))
    }
    
  }
  
  # Create the final correspondence table moving from the classification A to
  # the classification B.
  tryCatch({
    
    F_AtoB <- list()
    
    # The following if statement is used when we have only the
    # correspondence tables A:C1 and B:C1.
    counter <- 0
    if (length(R) == 2) {
      #creating a progress bar
      message("Percentage of codes of ", colnames(RRR[[1]][1]), " processed:")
      pb <- txtProgressBar(min = 0, max = 100, style = 3, width = 50, char = "=")
      
      # The following for loop creates the desirable correspondence
      # table.  The operations are conducted for each unique element of
      # classification A of the correspondence table A:C1.
      for (i in unique(R[[1]][, 1])) {
        
        # Print the percentage of codes that have been processed.
        counter <- counter + 1
        setTxtProgressBar(pb, round(counter/length(unique(R[[1]][, 1])) * 100, digits = 0))
        # Matrix TT contains the rows of correspondence table A:C1 for
        # a specific element of classification A.  Matrix T contains
        # the rows of correspondence table B:C1 that match with the
        # specific element of classification A based on classification
        # C1.
        x1 <- R[[1]][which(R[[1]][, 1] == i), 2]
        TT <- matrix(R[[1]][which(R[[1]][, 1] == i), 1:2], ncol = 2)
        T <- matrix(R[[2]][!is.na(match(R[[2]][, 2], x1)), 1:2], ncol = 2)
        
        # Create a list whose each element is a matrix that contains
        # all unique rows of matrix T based on the elements of
        # classification C1.
        t <- match(T[, 2], T[, 2])
        v1 <- sequence(rle(sort(t))$lengths)
        v1 <- split(seq_along(v1), cumsum(v1 == 1))
        Z <- lapply(v1, function(x) {
          T[order(t)[x], , drop = FALSE]
        })
        
        # Create a list whose each element is a matrix that contains
        # all unique rows of matrix TT that match with the unique
        # elements of the second column of matrix T.
        t1 <- match(TT[, 2], T[, 2])
        v1 <- sequence(rle(sort(t1))$lengths)
        v1 <- split(seq_along(v1), cumsum(v1 == 1))
        Z1 <- lapply(v1, function(x) {
          TT[order(t1)[x], , drop = FALSE]
        })
        
        # Keep matrices in Z that exist in Z1 based on their second
        # columns (elements of classification C1).
        Z <- Z[!is.na(match(lapply(Z, function(x) {
          unique(x[, 2])
        }), lapply(Z1, function(x) {
          unique(x[, 2])
        })))]
        
        # ZZ is a matrix that consists of matrices in Z1 expanded by
        # their corresponding matrices (based on the elements of
        # classification C1).
        a <- lapply(Z, function(x) {
          1:nrow(x)
        })
        a1 <- lapply(Z1, function(x) {
          1:nrow(x)
        })
        aa <- lapply(Map(function(x, y) {
          x[y[, 1], ]
        }, Z, Map(expand.grid, a, a1)), function(x) {
          matrix(x, ncol = 2)
        })
        aa1 <- lapply(Map(function(x, y) {
          x[y[, 1], ]
        }, Z1, Map(expand.grid, a1, a)), function(x) {
          matrix(x, ncol = 2)
        })
        ZZ <- do.call(rbind, Map(cbind, aa1, aa))
        
        # The records of A:C1 that do not exist in C1:C2 (in terms of
        # the values of classification C1) are adjusted to ZZ which
        # consists of records of A:C1 that exist in C1:C2 (in terms of
        # the values of classification C1).
        t1 <- matrix(TT[is.na(match(TT[, 2], ZZ[, 2])), ], ncol = 2)
        ZZ <- rbind(ZZ, cbind(t1, matrix("", nrow = nrow(t1), ncol = 2)))
        
        F_AtoB[[counter]] <- ZZ
        
      }
    }
    
    # The following if statement is used when we have only the
    # correspondence tables A:C1, C1:C2 and B:C2.
    if (length(R) == 3) {
      #creating a progress bar
      message("Percentage of codes of ", colnames(RRR[[1]][1]), " processed:")
      pb <- txtProgressBar(min = 0, max = 100, style = 3, width = 50, char = "=")
      
      
      # The following for loop creates the desirable correspondence
      # table.  The operations are conducted for each unique element of
      # classification A of the correspondence table A:C1.
      for (i in unique(R[[1]][, 1])) {
        counter <- counter + 1
        setTxtProgressBar(pb, round(counter/length(unique(R[[1]][, 1])) * 100, digits = 0))
        
        # Matrix T contains the rows of correspondence table C1:C2 that
        # match with the specific element of classification A based on
        # classification C1.
        x1 <- R[[1]][which(R[[1]][, 1] == i), 2]
        T <- matrix(R[[2]][!is.na(match(R[[2]][, 1], x1)), 1:2], ncol = 2)
        
        # The records of A:C1 that do not exist in C1:C2 (in terms of
        # the values of classification C1).
        if (length(which(is.na(match(x1, T[, 1])) == TRUE)) > 0) {
          M1 <- matrix(matrix(R[[1]][which(R[[1]][, 1] == i), 1:2], ncol = 2)[is.na(match(x1,
                                                                                          T[, 1])), ], ncol = 2)
        } else {
          M1 = matrix(0, 1, 2 * length(R))
          M1 = M1[FALSE, ]
        }
        
        if (nrow(M1) != 0) {
          for (times in 1:(2 * length(R) - ncol(M1))) {
            
            M1 <- cbind(M1, "")
            
          }
        }
        
        # Matrix TT contains the rows of correspondence table B:C2 that
        # match with the specific element of classification A based on
        # classification C1.
        x2 <- R[[2]][!is.na(match(R[[2]][, 1], x1)), 2]
        T1 <- matrix(R[[3]][!is.na(match(R[[3]][, 2], x2)), 1:2], ncol = 2)
        
        # The records of C1:C2 that do not exist in B:C2 (in terms of
        # the values of classification C2).
        if (length(which(is.na(match(x2, T1[, 2])) == TRUE)) > 0) {
          if (length(which(is.na(match(x2, T1[, 2])) == TRUE)) == 1) {
            M2 <- matrix(c(i, T[is.na(match(x2, T1[, 2])), 1], T[is.na(match(x2,
                                                                             T1[, 2])), ]), ncol = 4)
          } else {
            M2 <- cbind(i, T[is.na(match(x2, T1[, 2])), 1], T[is.na(match(x2,
                                                                          T1[, 2])), ])
          }
        } else {
          M2 = matrix(0, 1, 2 * length(R))
          M2 = M2[FALSE, ]
        }
        
        if (nrow(M2) != 0) {
          for (times in 1:(2 * length(R) - ncol(M2))) {
            
            M2 <- cbind(M2, "")
            
          }
        }
        
        # Create a list whose each element is a matrix that contains
        # all unique rows of matrix T based on the elements of
        # classification C1.
        t <- match(T[, 2], T[, 2])
        v1 <- sequence(rle(sort(t))$lengths)
        v1 <- split(seq_along(v1), cumsum(v1 == 1))
        Z <- lapply(v1, function(x) {
          T[order(t)[x], , drop = FALSE]
        })
        
        # Create a list whose each element is a matrix that contains
        # all unique rows of matrix TT that match with the unique
        # elements of the second column of matrix T.
        t1 <- match(T1[, 2], T[, 2])
        v1 <- sequence(rle(sort(t1))$lengths)
        v1 <- split(seq_along(v1), cumsum(v1 == 1))
        Z1 <- lapply(v1, function(x) {
          T1[order(t1)[x], , drop = FALSE]
        })
        
        # Keep matrices in Z that exist in Z1 based on their second
        # columns (elements of classification C1).
        Z <- Z[!is.na(match(lapply(Z, function(x) {
          unique(x[, 2])
        }), lapply(Z1, function(x) {
          unique(x[, 2])
        })))]
        
        # ZZ is a matrix that consists of matrices in Z1 expanded by
        # their corresponding matrices (based on the elements of
        # classification C1).
        a <- lapply(Z, function(x) {
          1:nrow(x)
        })
        a1 <- lapply(Z1, function(x) {
          1:nrow(x)
        })
        aa <- lapply(Map(function(x, y) {
          x[y[, 1], ]
        }, Z, Map(expand.grid, a, a1)), function(x) {
          matrix(x, ncol = 2)
        })
        aa1 <- lapply(Map(function(x, y) {
          x[y[, 1], ]
        }, Z1, Map(expand.grid, a1, a)), function(x) {
          matrix(x, ncol = 2)
        })
        ZZ <- do.call(rbind, Map(cbind, aa, aa1))
        
        # The records of both M1 and M2 are adjusted to ZZ which
        # consists of records of A:C1 that exist in C1:C2 (in terms of
        # the values of classification C1).
        if (is.null(dim(ZZ))) {
          F_AtoB[[counter]] <- rbind(M1, M2)
        } else {
          F_AtoB[[counter]] <- rbind(cbind(i, ZZ[, 1], ZZ), M1, M2)
        }
        
      }
      
    }
    
    # The following if statement is used in the general situation, in which
    # we have the correspondence tables A:C1, Ci:C(i+1) for i = 1, ...,
    # (k-1) Ci and B:Ck.
    M <- list()
    if (length(R) >= 4) {
      message("Percentage of codes of ", colnames(RRR[[1]][1]), " processed:")
      pb <- txtProgressBar(min = 0, max = 100, style = 3, width = 50, char = "=")
      
      
      # The following for loop creates the desirable correspondence
      # table.  The operations are conducted for each unique element of
      # classification A of the correspondence table A:C1.
      for (i in unique(R[[1]][, 1])) {
        
        counter <- counter + 1
        setTxtProgressBar(pb, round(counter/length(unique(R[[1]][, 1])) * 100, digits = 0))
        
        
        for (j in 1:(length(R) - 2)) {
          # The same operations as in the case that we have only the
          # correspondence tables A:C1 and B:C1, but here for the
          # correspondence tables C1:C2 and C2:C3.
          if (j == 1) {
            
            x1 <- R[[j]][which(R[[j]][, 1] == i), 2]
            T <- matrix(R[[j + 1]][!is.na(match(R[[j + 1]][, 1], x1)), 1:2],
                        ncol = 2)
            
            # The records of A:C1 that do not exist in C1:C2 (in terms
            # of the values of classification C1)
            
            if (length(which(is.na(match(x1, T[, 1])) == TRUE)) > 0) {
              M1 <- matrix(matrix(R[[j]][which(R[[j]][, 1] == i), 1:2], ncol = 2)[is.na(match(x1,
                                                                                              T[, 1])), ], ncol = 2)
            } else {
              M1 = matrix(0, 1, 2 * length(R))
              M1 = M1[FALSE, ]
            }
            
            if (nrow(M1) != 0) {
              for (times in 1:(2 * length(R) - ncol(M1))) {
                
                M1 <- cbind(M1, "")
                
              }
            }
            
            x2 <- R[[j + 1]][!is.na(match(R[[j + 1]][, 1], x1)), 2]
            T1 <- matrix(R[[j + 2]][!is.na(match(R[[j + 2]][, 1], x2)), 1:2],
                         ncol = 2)
            
            if (length(which(is.na(match(x2, T1[, 1])) == TRUE)) > 0) {
              
              if (length(which(is.na(match(x2, T1[, 1])) == TRUE)) == 1) {
                M2 <- matrix(c(i, T[is.na(match(x2, T1[, 1])), 1], T[is.na(match(x2,
                                                                                 T1[, 1])), ]), ncol = 4)
                
              } else {
                M2 <- cbind(i, T[is.na(match(x2, T1[, 1])), 1], T[is.na(match(x2,
                                                                              T1[, 1])), ])
              }
            } else {
              M2 = matrix(0, 1, 2 * length(R))
              M2 = M2[FALSE, ]
            }
            
            if (nrow(M2) != 0) {
              for (times in 1:(2 * length(R) - ncol(M2))) {
                
                M2 <- cbind(M2, "")
                
              }
            }
            
            t <- match(T[, 2], T[, 2])
            v1 <- sequence(rle(sort(t))$lengths)
            v1 <- split(seq_along(v1), cumsum(v1 == 1))
            Z <- lapply(v1, function(x) {
              T[order(t)[x], , drop = FALSE]
            })
            
            t1 <- match(T1[, 1], T[, 2])
            v1 <- sequence(rle(sort(t1))$lengths)
            v1 <- split(seq_along(v1), cumsum(v1 == 1))
            Z1 <- lapply(v1, function(x) {
              T1[order(t1)[x], , drop = FALSE]
            })
            
            Z <- Z[!is.na(match(lapply(Z, function(x) {
              unique(x[, 2])
            }), lapply(Z1, function(x) {
              unique(x[, 1])
            })))]
            
            a <- lapply(Z, function(x) {
              1:nrow(x)
            })
            a1 <- lapply(Z1, function(x) {
              1:nrow(x)
            })
            aa <- lapply(Map(function(x, y) {
              x[y[, 1], ]
            }, Z, Map(expand.grid, a, a1)), function(x) {
              matrix(x, ncol = 2)
            })
            aa1 <- lapply(Map(function(x, y) {
              x[y[, 1], ]
            }, Z1, Map(expand.grid, a1, a)), function(x) {
              matrix(x, ncol = 2)
            })
            ZZ <- do.call(rbind, Map(cbind, aa, aa1))
            
          }
          
          # The same operations as in the case that we have only the
          # correspondence tables A:C1 and B:C1, but here for the pairs
          # of correspondence tables (C2:C3 - C3:C4), (C3:C4 - C4:C5),
          # ..., (C(k-2):C(k-1) - C(k-1):Ck).  For each value of j that
          # satisfies the if statement, the previous matrix ZZ created
          # is used.  For j = 2, the matrix ZZ created in the previous
          # if statement is used.
          if (j >= 2 && j <= (length(R) - 3) && length(R) != 4) {
            
            t <- match(ZZ[, ncol(ZZ)], ZZ[, ncol(ZZ)])
            v1 <- sequence(rle(sort(t))$lengths)
            v1 <- split(seq_along(v1), cumsum(v1 == 1))
            Z <- lapply(v1, function(x) {
              ZZ[order(t)[x], , drop = FALSE]
            })
            
            t1 <- match(R[[j + 2]][, 1], ZZ[, ncol(ZZ)])
            v1 <- sequence(rle(sort(t1))$lengths)
            v1 <- split(seq_along(v1), cumsum(v1 == 1))
            Z1 <- lapply(v1, function(x) {
              R[[j + 2]][order(t1)[x], 1:2, drop = FALSE]
            })
            
            if (length(which(is.na(match(ZZ[, ncol(ZZ)], R[[j + 2]][, 1])) ==
                             TRUE)) > 0) {
              if (length(which(is.na(match(ZZ[, ncol(ZZ)], R[[j + 2]][, 1])) ==
                               TRUE)) == 1) {
                M3 <- matrix(c(i, ZZ[is.na(match(ZZ[, ncol(ZZ)], R[[j + 2]][,
                                                                            1])), 1], ZZ[is.na(match(ZZ[, ncol(ZZ)], R[[j + 2]][, 1])),
                                                                            ]), ncol = ncol(ZZ) + 2)
              } else {
                M3 <- cbind(i, ZZ[is.na(match(ZZ[, ncol(ZZ)], R[[j + 2]][,
                                                                         1])), 1], ZZ[is.na(match(ZZ[, ncol(ZZ)], R[[j + 2]][, 1])),
                                                                         ])
              }
            } else {
              M3 = matrix(0, 1, 2 * length(R))
              M3 = M3[FALSE, ]
            }
            
            if (nrow(M3) != 0) {
              for (times in 1:(2 * length(R) - ncol(M3))) {
                
                M3 <- cbind(M3, "")
                
              }
            }
            
            M[[j - 1]] <- M3
            
            Z <- Z[!is.na(match(lapply(Z, function(x) {
              unique(x[, ncol(ZZ)])
            }), lapply(Z1, function(x) {
              unique(x[, 1])
            })))]
            
            a <- lapply(Z, function(x) {
              1:nrow(x)
            })
            a1 <- lapply(Z1, function(x) {
              1:nrow(x)
            })
            
            aa <- lapply(Map(function(x, y) {
              x[y[, 1], ]
            }, Z, Map(expand.grid, a, a1)), function(x) {
              matrix(x, ncol = ncol(ZZ))
            })
            aa1 <- lapply(Map(function(x, y) {
              x[y[, 1], ]
            }, Z1, Map(expand.grid, a1, a)), function(x) {
              matrix(x, ncol = 2)
            })
            
            ZZ <- do.call(rbind, Map(cbind, aa, aa1))
            
          }
          
          # The same operations as in the case that we have only the
          # correspondence tables A:C1 and B:C1, but here for the
          # correspondence tables C(k-1):Ck and B:Ck.  For the value of
          # j that satisfies the if statement, the matrix ZZ created in
          # the previous if statement is used.
          if (j == (length(R) - 2)) {
            
            t <- match(ZZ[, ncol(ZZ)], ZZ[, ncol(ZZ)])
            v1 <- sequence(rle(sort(t))$lengths)
            v1 <- split(seq_along(v1), cumsum(v1 == 1))
            Z <- lapply(v1, function(x) {
              ZZ[order(t)[x], , drop = FALSE]
            })
            
            t1 <- match(R[[length(R)]][, 2], ZZ[, ncol(ZZ)])
            v1 <- sequence(rle(sort(t1))$lengths)
            v1 <- split(seq_along(v1), cumsum(v1 == 1))
            Z1 <- lapply(v1, function(x) {
              R[[length(R)]][order(t1)[x], 1:2, drop = FALSE]
            })
            
            if (length(which(is.na(match(ZZ[, ncol(ZZ)], R[[length(R)]][,
                                                                        2])) == TRUE)) > 0) {
              if (length(which(is.na(match(ZZ[, ncol(ZZ)], R[[length(R)]][,
                                                                          2])) == TRUE)) == 1) {
                M4 <- matrix(c(i, ZZ[is.na(match(ZZ[, ncol(ZZ)], R[[length(R)]][,
                                                                                2])), 1], ZZ[is.na(match(ZZ[, ncol(ZZ)], R[[length(R)]][,
                                                                                                                                        2])), ]), ncol = ncol(ZZ) + 2)
              } else {
                M4 <- cbind(i, ZZ[is.na(match(ZZ[, ncol(ZZ)], R[[length(R)]][,
                                                                             2])), 1], ZZ[is.na(match(ZZ[, ncol(ZZ)], R[[length(R)]][,
                                                                                                                                     2])), ])
              }
            } else {
              M4 = matrix(0, 1, 2 * length(R))
              M4 = M4[FALSE, ]
            }
            
            
            if (nrow(M4) != 0) {
              for (times in 1:(2 * length(R) - ncol(M4))) {
                
                M4 <- cbind(M4, "")
                
              }
            }
            
            Z <- Z[!is.na(match(lapply(Z, function(x) {
              unique(x[, ncol(ZZ)])
            }), lapply(Z1, function(x) {
              unique(x[, 2])
            })))]
            
            a <- lapply(Z, function(x) {
              1:nrow(x)
            })
            a1 <- lapply(Z1, function(x) {
              1:nrow(x)
            })
            
            aa <- lapply(Map(function(x, y) {
              x[y[, 1], ]
            }, Z, Map(expand.grid, a, a1)), function(x) {
              matrix(x, ncol = ncol(ZZ))
            })
            aa1 <- lapply(Map(function(x, y) {
              x[y[, 1], ]
            }, Z1, Map(expand.grid, a1, a)), function(x) {
              matrix(x, ncol = 2)
            })
            
            ZZ <- do.call(rbind, Map(cbind, aa, aa1))
            
          }
        }
        
        if (is.null(dim(ZZ))) {
          F_AtoB[[counter]] <- rbind(M1, M2, do.call(rbind, M), M4)
        } else {
          F_AtoB[[counter]] <- rbind(cbind(i, ZZ[, 1], ZZ), M1, M2, do.call(rbind,
                                                                            M), M4)
        }
      }
    }
    
    # Create the desired correspondence table for the selected element of
    # classification A.
    F_AtoB <- do.call(rbind, F_AtoB)
    
    # Keep in F the classifications A, C1, C2, ..., Ck, B once, based on
    # the number of the correspondence tables.
    if (length(R) == 2) {
      F_AtoB <- F_AtoB[, c(1, 2, 3)]
    }
    if (length(R) == 3) {
      F_AtoB <- F_AtoB[, c(1, 2, 4, 5)]
    }
    if (length(R) >= 4) {
      F_AtoB <- F_AtoB[, sort(c(1, seq(2, 2 * length(R) - 2, 2), 2 * length(R) -
                                  1))]
    }
    
    # Convert classifications as well as correspondence tables so as to
    # move from classification B to classification A.  Until the next
    # comment, all the lines are the same as in the case that we move from
    # classification A to classification B.
    RRR_BtoA <- RRR[c(rev(1:(k + 2)), rev(utils::tail(c(1:length(RRR)), (length(RRR) -
                                                                           1)/2)))]
    if (length(rev(utils::tail(c(1:length(RR)), (length(RR) - 1)/2))) >= 3) {
      for (rev in (k + 4):(length(RRR_BtoA) - 1)) {
        column_2 <- RRR_BtoA[[rev]][, 2]
        RRR_BtoA[[rev]][, 2] <- RRR_BtoA[[rev]][, 1]
        RRR_BtoA[[rev]][, 1] <- column_2
      }
    }
    
    RR <- lapply(RRR_BtoA, function(x) {
      matrix(unlist(x), ncol = ncol(x))
    })
    
    R <- RR[utils::tail(c(1:length(RR)), (length(RR) - 1)/2)]
    
    F_BtoA <- list()
    
    counter <- 0
    message("\n")
    if (length(R) == 2) {
      
      message("Percentage of codes of ", colnames(RRR_BtoA[[1]][1]), " processed:")
      pb <- txtProgressBar(min = 0, max = 100, style = 3, width = 50, char = "=")
      
      for (i in unique(R[[1]][, 1])) {
        
        counter <- counter + 1
        setTxtProgressBar(pb, round(counter/length(unique(R[[1]][, 1])) * 100, digits = 0))
        
        x1 <- R[[1]][which(R[[1]][, 1] == i), 2]
        TT <- matrix(R[[1]][which(R[[1]][, 1] == i), 1:2], ncol = 2)
        T <- matrix(R[[2]][!is.na(match(R[[2]][, 2], x1)), 1:2], ncol = 2)
        
        t <- match(T[, 2], T[, 2])
        v1 <- sequence(rle(sort(t))$lengths)
        v1 <- split(seq_along(v1), cumsum(v1 == 1))
        Z <- lapply(v1, function(x) {
          T[order(t)[x], , drop = FALSE]
        })
        
        t1 <- match(TT[, 2], T[, 2])
        v1 <- sequence(rle(sort(t1))$lengths)
        v1 <- split(seq_along(v1), cumsum(v1 == 1))
        Z1 <- lapply(v1, function(x) {
          TT[order(t1)[x], , drop = FALSE]
        })
        
        Z <- Z[!is.na(match(lapply(Z, function(x) {
          unique(x[, 2])
        }), lapply(Z1, function(x) {
          unique(x[, 2])
        })))]
        
        a <- lapply(Z, function(x) {
          1:nrow(x)
        })
        a1 <- lapply(Z1, function(x) {
          1:nrow(x)
        })
        aa <- lapply(Map(function(x, y) {
          x[y[, 1], ]
        }, Z, Map(expand.grid, a, a1)), function(x) {
          matrix(x, ncol = 2)
        })
        aa1 <- lapply(Map(function(x, y) {
          x[y[, 1], ]
        }, Z1, Map(expand.grid, a1, a)), function(x) {
          matrix(x, ncol = 2)
        })
        ZZ <- do.call(rbind, Map(cbind, aa1, aa))
        
        t1 <- matrix(TT[is.na(match(TT[, 2], ZZ[, 2])), ], ncol = 2)
        ZZ <- rbind(ZZ, cbind(t1, matrix("", nrow = nrow(t1), ncol = 2)))
        
        F_BtoA[[counter]] <- ZZ
        
      }
    }
    
    if (length(R) == 3) {
      message("Percentage of codes of ", colnames(RRR_BtoA[[1]][1]), " processed:")
      pb <- txtProgressBar(min = 0, max = 100, style = 3, width = 50, char = "=")
      for (i in unique(R[[1]][, 1])) {
        
        counter <- counter + 1
        setTxtProgressBar(pb, round(counter/length(unique(R[[1]][, 1])) * 100, digits = 0))
        
        x1 <- R[[1]][which(R[[1]][, 1] == i), 2]
        T <- matrix(R[[2]][!is.na(match(R[[2]][, 1], x1)), 1:2], ncol = 2)
        
        if (length(which(is.na(match(x1, T[, 1])) == TRUE)) > 0) {
          M1 <- matrix(matrix(R[[1]][which(R[[1]][, 1] == i), 1:2], ncol = 2)[is.na(match(x1,
                                                                                          T[, 1])), ], ncol = 2)
        } else {
          M1 = matrix(0, 1, 2 * length(R))
          M1 = M1[FALSE, ]
        }
        
        if (nrow(M1) != 0) {
          for (times in 1:(2 * length(R) - ncol(M1))) {
            
            M1 <- cbind(M1, "")
            
          }
        }
        
        x2 <- R[[2]][!is.na(match(R[[2]][, 1], x1)), 2]
        T1 <- matrix(R[[3]][!is.na(match(R[[3]][, 2], x2)), 1:2], ncol = 2)
        
        if (length(which(is.na(match(x2, T1[, 2])) == TRUE)) > 0) {
          if (length(which(is.na(match(x2, T1[, 2])) == TRUE)) == 1) {
            M2 <- matrix(c(i, T[is.na(match(x2, T1[, 2])), 1], T[is.na(match(x2,
                                                                             T1[, 2])), ]), ncol = 4)
          } else {
            M2 <- cbind(i, T[is.na(match(x2, T1[, 2])), 1], T[is.na(match(x2,
                                                                          T1[, 2])), ])
          }
        } else {
          M2 = matrix(0, 1, 2 * length(R))
          M2 = M2[FALSE, ]
        }
        
        if (nrow(M2) != 0) {
          for (times in 1:(2 * length(R) - ncol(M2))) {
            
            M2 <- cbind(M2, "")
            
          }
        }
        
        t <- match(T[, 2], T[, 2])
        v1 <- sequence(rle(sort(t))$lengths)
        v1 <- split(seq_along(v1), cumsum(v1 == 1))
        Z <- lapply(v1, function(x) {
          T[order(t)[x], , drop = FALSE]
        })
        
        t1 <- match(T1[, 2], T[, 2])
        v1 <- sequence(rle(sort(t1))$lengths)
        v1 <- split(seq_along(v1), cumsum(v1 == 1))
        Z1 <- lapply(v1, function(x) {
          T1[order(t1)[x], , drop = FALSE]
        })
        
        Z <- Z[!is.na(match(lapply(Z, function(x) {
          unique(x[, 2])
        }), lapply(Z1, function(x) {
          unique(x[, 2])
        })))]
        
        a <- lapply(Z, function(x) {
          1:nrow(x)
        })
        a1 <- lapply(Z1, function(x) {
          1:nrow(x)
        })
        aa <- lapply(Map(function(x, y) {
          x[y[, 1], ]
        }, Z, Map(expand.grid, a, a1)), function(x) {
          matrix(x, ncol = 2)
        })
        aa1 <- lapply(Map(function(x, y) {
          x[y[, 1], ]
        }, Z1, Map(expand.grid, a1, a)), function(x) {
          matrix(x, ncol = 2)
        })
        ZZ <- do.call(rbind, Map(cbind, aa, aa1))
        
        if (is.null(dim(ZZ))) {
          F_BtoA[[counter]] <- rbind(M1, M2)
        } else {
          F_BtoA[[counter]] <- rbind(cbind(i, ZZ[, 1], ZZ), M1, M2)
        }
        
      }
      
    }
    M <- list()
    if (length(R) >= 4) {
      message("Percentage of codes of ", colnames(RRR_BtoA[[1]][1]), " processed:")
      pb <- txtProgressBar(min = 0, max = 100, style = 3, width = 50, char = "=")
      for (i in unique(R[[1]][, 1])) {
        
        counter <- counter + 1
        setTxtProgressBar(pb, round(counter/length(unique(R[[1]][, 1])) * 100, digits = 0))
        
        for (j in 1:(length(R) - 2)) {
          if (j == 1) {
            
            x1 <- R[[j]][which(R[[j]][, 1] == i), 2]
            T <- matrix(R[[j + 1]][!is.na(match(R[[j + 1]][, 1], x1)), 1:2],
                        ncol = 2)
            
            if (length(which(is.na(match(x1, T[, 1])) == TRUE)) > 0) {
              M1 <- matrix(matrix(R[[j]][which(R[[j]][, 1] == i), 1:2], ncol = 2)[is.na(match(x1,
                                                                                              T[, 1])), ], ncol = 2)
            } else {
              M1 = matrix(0, 1, 2 * length(R))
              M1 = M1[FALSE, ]
            }
            
            if (nrow(M1) != 0) {
              for (times in 1:(2 * length(R) - ncol(M1))) {
                
                M1 <- cbind(M1, "")
                
              }
            }
            
            x2 <- R[[j + 1]][!is.na(match(R[[j + 1]][, 1], x1)), 2]
            T1 <- matrix(R[[j + 2]][!is.na(match(R[[j + 2]][, 1], x2)), 1:2],
                         ncol = 2)
            
            if (length(which(is.na(match(x2, T1[, 1])) == TRUE)) > 0) {
              
              if (length(which(is.na(match(x2, T1[, 1])) == TRUE)) == 1) {
                M2 <- matrix(c(i, T[is.na(match(x2, T1[, 1])), 1], T[is.na(match(x2,
                                                                                 T1[, 1])), ]), ncol = 4)
                
              } else {
                M2 <- cbind(i, T[is.na(match(x2, T1[, 1])), 1], T[is.na(match(x2,
                                                                              T1[, 1])), ])
              }
            } else {
              M2 = matrix(0, 1, 2 * length(R))
              M2 = M2[FALSE, ]
            }
            
            if (nrow(M2) != 0) {
              for (times in 1:(2 * length(R) - ncol(M2))) {
                
                M2 <- cbind(M2, "")
                
              }
            }
            
            t <- match(T[, 2], T[, 2])
            v1 <- sequence(rle(sort(t))$lengths)
            v1 <- split(seq_along(v1), cumsum(v1 == 1))
            Z <- lapply(v1, function(x) {
              T[order(t)[x], , drop = FALSE]
            })
            
            t1 <- match(T1[, 1], T[, 2])
            v1 <- sequence(rle(sort(t1))$lengths)
            v1 <- split(seq_along(v1), cumsum(v1 == 1))
            Z1 <- lapply(v1, function(x) {
              T1[order(t1)[x], , drop = FALSE]
            })
            
            Z <- Z[!is.na(match(lapply(Z, function(x) {
              unique(x[, 2])
            }), lapply(Z1, function(x) {
              unique(x[, 1])
            })))]
            
            a <- lapply(Z, function(x) {
              1:nrow(x)
            })
            a1 <- lapply(Z1, function(x) {
              1:nrow(x)
            })
            aa <- lapply(Map(function(x, y) {
              x[y[, 1], ]
            }, Z, Map(expand.grid, a, a1)), function(x) {
              matrix(x, ncol = 2)
            })
            aa1 <- lapply(Map(function(x, y) {
              x[y[, 1], ]
            }, Z1, Map(expand.grid, a1, a)), function(x) {
              matrix(x, ncol = 2)
            })
            ZZ <- do.call(rbind, Map(cbind, aa, aa1))
            
          }
          
          if (j >= 2 && j <= (length(R) - 3) && length(R) != 4) {
            
            t <- match(ZZ[, ncol(ZZ)], ZZ[, ncol(ZZ)])
            v1 <- sequence(rle(sort(t))$lengths)
            v1 <- split(seq_along(v1), cumsum(v1 == 1))
            Z <- lapply(v1, function(x) {
              ZZ[order(t)[x], , drop = FALSE]
            })
            
            t1 <- match(R[[j + 2]][, 1], ZZ[, ncol(ZZ)])
            v1 <- sequence(rle(sort(t1))$lengths)
            v1 <- split(seq_along(v1), cumsum(v1 == 1))
            Z1 <- lapply(v1, function(x) {
              R[[j + 2]][order(t1)[x], 1:2, drop = FALSE]
            })
            
            if (length(which(is.na(match(ZZ[, ncol(ZZ)], R[[j + 2]][, 1])) ==
                             TRUE)) > 0) {
              if (length(which(is.na(match(ZZ[, ncol(ZZ)], R[[j + 2]][, 1])) ==
                               TRUE)) == 1) {
                M3 <- matrix(c(i, ZZ[is.na(match(ZZ[, ncol(ZZ)], R[[j + 2]][,
                                                                            1])), 1], ZZ[is.na(match(ZZ[, ncol(ZZ)], R[[j + 2]][, 1])),
                                                                            ]), ncol = ncol(ZZ) + 2)
              } else {
                M3 <- cbind(i, ZZ[is.na(match(ZZ[, ncol(ZZ)], R[[j + 2]][,
                                                                         1])), 1], ZZ[is.na(match(ZZ[, ncol(ZZ)], R[[j + 2]][, 1])),
                                                                         ])
              }
            } else {
              M3 = matrix(0, 1, 2 * length(R))
              M3 = M3[FALSE, ]
            }
            
            if (nrow(M3) != 0) {
              for (times in 1:(2 * length(R) - ncol(M3))) {
                
                M3 <- cbind(M3, "")
                
              }
            }
            M[[j - 1]] <- M3
            
            Z <- Z[!is.na(match(lapply(Z, function(x) {
              unique(x[, ncol(ZZ)])
            }), lapply(Z1, function(x) {
              unique(x[, 1])
            })))]
            
            a <- lapply(Z, function(x) {
              1:nrow(x)
            })
            a1 <- lapply(Z1, function(x) {
              1:nrow(x)
            })
            
            aa <- lapply(Map(function(x, y) {
              x[y[, 1], ]
            }, Z, Map(expand.grid, a, a1)), function(x) {
              matrix(x, ncol = ncol(ZZ))
            })
            aa1 <- lapply(Map(function(x, y) {
              x[y[, 1], ]
            }, Z1, Map(expand.grid, a1, a)), function(x) {
              matrix(x, ncol = 2)
            })
            
            ZZ <- do.call(rbind, Map(cbind, aa, aa1))
            
          }
          
          if (j == (length(R) - 2)) {
            
            t <- match(ZZ[, ncol(ZZ)], ZZ[, ncol(ZZ)])
            v1 <- sequence(rle(sort(t))$lengths)
            v1 <- split(seq_along(v1), cumsum(v1 == 1))
            Z <- lapply(v1, function(x) {
              ZZ[order(t)[x], , drop = FALSE]
            })
            
            t1 <- match(R[[length(R)]][, 2], ZZ[, ncol(ZZ)])
            v1 <- sequence(rle(sort(t1))$lengths)
            v1 <- split(seq_along(v1), cumsum(v1 == 1))
            Z1 <- lapply(v1, function(x) {
              R[[length(R)]][order(t1)[x], 1:2, drop = FALSE]
            })
            
            if (length(which(is.na(match(ZZ[, ncol(ZZ)], R[[length(R)]][,
                                                                        2])) == TRUE)) > 0) {
              if (length(which(is.na(match(ZZ[, ncol(ZZ)], R[[length(R)]][,
                                                                          2])) == TRUE)) == 1) {
                M4 <- matrix(c(i, ZZ[is.na(match(ZZ[, ncol(ZZ)], R[[length(R)]][,
                                                                                2])), 1], ZZ[is.na(match(ZZ[, ncol(ZZ)], R[[length(R)]][,
                                                                                                                                        2])), ]), ncol = ncol(ZZ) + 2)
              } else {
                M4 <- cbind(i, ZZ[is.na(match(ZZ[, ncol(ZZ)], R[[length(R)]][,
                                                                             2])), 1], ZZ[is.na(match(ZZ[, ncol(ZZ)], R[[length(R)]][,
                                                                                                                                     2])), ])
              }
            } else {
              M4 = matrix(0, 1, 2 * length(R))
              M4 = M4[FALSE, ]
            }
            
            
            if (nrow(M4) != 0) {
              for (times in 1:(2 * length(R) - ncol(M4))) {
                
                M4 <- cbind(M4, "")
                
              }
            }
            
            Z <- Z[!is.na(match(lapply(Z, function(x) {
              unique(x[, ncol(ZZ)])
            }), lapply(Z1, function(x) {
              unique(x[, 2])
            })))]
            
            a <- lapply(Z, function(x) {
              1:nrow(x)
            })
            a1 <- lapply(Z1, function(x) {
              1:nrow(x)
            })
            
            aa <- lapply(Map(function(x, y) {
              x[y[, 1], ]
            }, Z, Map(expand.grid, a, a1)), function(x) {
              matrix(x, ncol = ncol(ZZ))
            })
            aa1 <- lapply(Map(function(x, y) {
              x[y[, 1], ]
            }, Z1, Map(expand.grid, a1, a)), function(x) {
              matrix(x, ncol = 2)
            })
            
            ZZ <- do.call(rbind, Map(cbind, aa, aa1))
            
          }
        }
        
        if (is.null(dim(ZZ))) {
          F_BtoA[[counter]] <- rbind(M1, M2, do.call(rbind, M), M4)
        } else {
          F_BtoA[[counter]] <- rbind(cbind(i, ZZ[, 1], ZZ), M1, M2, do.call(rbind,
                                                                            M), M4)
        }
        
        
      }
    }
    
    F_BtoA <- do.call(rbind, F_BtoA)
    
    if (length(R) == 2) {
      F_BtoA <- F_BtoA[, c(1, 2, 3)]
    }
    if (length(R) == 3) {
      F_BtoA <- F_BtoA[, c(1, 2, 4, 5)]
    }
    if (length(R) >= 4) {
      F_BtoA <- F_BtoA[, sort(c(1, seq(2, 2 * length(R) - 2, 2), 2 * length(R) -
                                  1))]
    }
    
    
    F_BtoA <- F_BtoA[, rev(1:ncol(F_BtoA))]
    # Combine the results from moving from classification A to B, and vice
    # versa.  F_AtoB
    keep <- 0
    keepF_AtoB <- c(0)
    for (iterr in 1:nrow(F_AtoB)) {
      
      if (F_AtoB[iterr, 1] != "") {
        blanks <- F_AtoB[iterr, ] == ""
        
        if (all(blanks == FALSE)) {
          keep <- keep + 1
          keepF_AtoB[keep] <- iterr
        } else {
          blanks = which(F_AtoB[iterr, ] == "")
          if (all(c(blanks[1]:ncol(F_AtoB)) == "")) {
            keep <- keep + 1
            keepF_AtoB[keep] <- iterr
          }
        }
        
      }
    }
    
    NoNullF_AtoB <- matrix(F_AtoB[keepF_AtoB, ], ncol = k + 2)
    if (nrow(NoNullF_AtoB) != nrow(F_AtoB)) {
      if (length(keepF_AtoB) == 1 && keepF_AtoB == c(0)) {
        FNullAtoB <- matrix(F_AtoB, ncol = k + 2)
        for (iter in 1:nrow(FNullAtoB)) {
          FNullAtoB[iter, (which(FNullAtoB[iter, ] == "")[1]):(k + 2)] <- ""
        }
      } else {
        FNullAtoB <- matrix(F_AtoB[-keepF_AtoB, ], ncol = k + 2)
        for (iter in 1:nrow(FNullAtoB)) {
          FNullAtoB[iter, (which(FNullAtoB[iter, ] == "")[1]):(k + 2)] <- ""
        }
      }
    } else {
      FNullAtoB <- matrix(0, 1, k + 2)
      FNullAtoB <- FNullAtoB[FALSE, ]
    }
    
    # F_BtoA
    keep <- 0
    keepF_BtoA <- c(0)
    for (iterr in 1:nrow(F_BtoA)) {
      
      if (F_BtoA[iterr, ncol(F_AtoB)] != "") {
        blanks <- F_BtoA[iterr, ] == ""
        
        if (all(blanks == FALSE)) {
          keep <- keep + 1
          keepF_BtoA[keep] <- iterr
        } else {
          blanks <- which(F_BtoA[iterr, ] == "")
          if (all(c(1:length(blanks)) == "")) {
            keep <- keep + 1
            keepF_BtoA[keep] <- iterr
          }
        }
        
      }
    }
    
    # Combine all together
    
    NoNullF_BtoA <- matrix(F_BtoA[keepF_BtoA, ], ncol = k + 2)
    if (nrow(NoNullF_BtoA) != nrow(F_BtoA)) {
      if (length(keepF_BtoA) == 1 && keepF_BtoA == c(0)) {
        FNullBtoA <- matrix(F_BtoA, ncol = k + 2)
        for (iter in 1:nrow(FNullBtoA)) {
          FNullBtoA[iter, (which(FNullBtoA[iter, ] == "")[length(which(FNullBtoA[iter,
          ] == ""))]):1] <- ""
        }
      } else {
        FNullBtoA <- matrix(F_BtoA[-keepF_BtoA, ], ncol = k + 2)
        for (iter in 1:nrow(FNullBtoA)) {
          FNullBtoA[iter, (which(FNullBtoA[iter, ] == "")[length(which(FNullBtoA[iter,
          ] == ""))]):1] <- ""
        }
      }
    } else {
      FNullBtoA <- matrix(0, 1, k + 2)
      FNullBtoA <- FNullBtoA[FALSE, ]
    }
    
    F <- unique(rbind(NoNullF_AtoB, NoNullF_BtoA))
    F <- unique(rbind(F, unique(FNullAtoB), unique(FNullBtoA)))
    if (length(which(apply(F, 1, function(x) {
      length(which(x == ""))
    } == k + 2) == TRUE)) >= 1) {
      F <- F[-which(apply(F, 1, function(x) {
        length(which(x == ""))
      } == k + 2) == TRUE), ]
    }
    
    # The if statement is based on which of classifications A or B is the
    # reference one (if any).
    if (length(which(apply(F, 1, function(x) {
      length(which(x == ""))
    }) == 0)) >= 1) {
      
      if (Reference == "A") {
        idx <- k + 5
        
        # Creation of the review flag for the correspondence table A:B.
        F1 <- matrix(F[apply(F, 1, function(x) {
          length(which(x == ""))
        }) == 0, ], ncol = k + 2)
        F2 <- F[apply(F, 1, function(x) {
          length(which(x == ""))
        }) >= 1, ]
        F2 <- matrix(unlist(F2), ncol = k + 2)
        f <- stats::aggregate(matrix(unique(F1[, c(1, ncol(F1))]), ncol = 2)[, 2],
                              list(num = matrix(unique(F1[, c(1, ncol(F1))]), ncol = 2)[, 2]),
                              length)[which(stats::aggregate(matrix(unique(F1[, c(1, ncol(F1))]), ncol = 2)[,
                                                                                                            2], list(num = matrix(unique(F1[, c(1, ncol(F1))]), ncol = 2)[,
                                                                                                                                                                          2]), length)[, 2] > 1), 1]
        reviewF1 <- rep(0, nrow(F1))
        reviewF1[which(F1[, ncol(F1)] %in% f)] <- 1
        Review <- data.frame(cbind(rbind(F1, F2), c(reviewF1, rep(0, nrow(F2)))))
        
        # Creation of the redundancy flag for the correspondence table
        # A:B.
        F1 <- Review[apply(Review, 1, function(x) {
          length(which(x == ""))
        }) == 0, ]
        F1 <- matrix(unlist(F1), ncol = k + 3)
        F1 <- data.frame(F1)
        colnames(F1) <- colnames(Review)
        F2 <- Review[apply(Review, 1, function(x) {
          length(which(x == ""))
        }) >= 1, ]
        F2 <- matrix(unlist(F2), ncol = k + 3)
        F2 <- data.frame(F2)
        colnames(F2) <- colnames(F1)
        f1 <- stats::aggregate(F1[, c(1, ncol(F1) - 1)], by = F1[, c(1, ncol(F1) -
                                                                       1)], length)[1:(ncol(F1[, c(1, ncol(F1) - 1)]) + 1)][which(stats::aggregate(F1[,
                                                                                                                                                      c(1, ncol(F1) - 1)], by = F1[, c(1, ncol(F1) - 1)], length)[1:(ncol(F1[,
                                                                                                                                                                                                                             c(1, ncol(F1) - 1)]) + 1)][, 3] >= 2), 1:2]
        redundancyF1 <- rep(0, nrow(F1))
        redundancyF1[which(apply(F1[, c(1, ncol(F1) - 1)], 1, paste, collapse = "") %in%
                             apply(f1, 1, paste, collapse = ""))] <- 1
        correspondenceAB <- data.frame(cbind(rbind(F1, F2), c(redundancyF1,
                                                              rep(0, nrow(F2)))))
        
        # Creation of the unmatched flag for the correspondence table
        # A:B.
        correspondenceAB <- data.frame(correspondenceAB, 1)
        colnames(correspondenceAB) <- c(paste(colnames(RRR[[1]][1])), paste(unlist(lapply(RRR,
                                                                                          function(x) {
                                                                                            colnames(x)[1]
                                                                                          }))[seq(k) + 1]), paste(colnames(RRR[[k + 2]][1])), "Review", "Redundancy",
                                        "Unmatched")
        
      } else if (Reference == "B") {
        idx <- k + 5
        
        # Creation of the review flag for the correspondence table A:B.
        F1 <- matrix(F[apply(F, 1, function(x) {
          length(which(x == ""))
        }) == 0, ], ncol = k + 2)
        F2 <- F[apply(F, 1, function(x) {
          length(which(x == ""))
        }) >= 1, ]
        F2 <- matrix(unlist(F2), ncol = k + 2)
        f <- stats::aggregate(matrix(unique(F1[, c(1, ncol(F1))]), ncol = 2)[, 1],
                              list(num = matrix(unique(F1[, c(1, ncol(F1))]), ncol = 2)[, 1]),
                              length)[which(stats::aggregate(matrix(unique(F1[, c(1, ncol(F1))]), ncol = 2)[,
                                                                                                            1], list(num = matrix(unique(F1[, c(1, ncol(F1))]), ncol = 2)[,
                                                                                                                                                                          1]), length)[, 2] > 1), 1]
        reviewF1 <- rep(0, nrow(F1))
        reviewF1[which(F1[, 1] %in% f)] <- 1
        Review <- data.frame(cbind(rbind(F1, F2), c(reviewF1, rep(0, nrow(F2)))))
        
        # Creation of the redundancy flag for the correspondence table
        # A:B.
        F1 <- Review[apply(Review, 1, function(x) {
          length(which(x == ""))
        }) == 0, ]
        F1 <- matrix(unlist(F1), ncol = k + 3)
        F1 <- data.frame(F1)
        colnames(F1) <- colnames(Review)
        F2 <- Review[apply(Review, 1, function(x) {
          length(which(x == ""))
        }) >= 1, ]
        F2 <- matrix(unlist(F2), ncol = k + 3)
        F2 <- data.frame(F2)
        colnames(F2) <- colnames(F1)
        f1 <- stats::aggregate(F1[, c(1, ncol(F1) - 1)], by = F1[, c(1, ncol(F1) -
                                                                       1)], length)[1:(ncol(F1[, c(1, ncol(F1) - 1)]) + 1)][which(stats::aggregate(F1[,
                                                                                                                                                      c(1, ncol(F1) - 1)], by = F1[, c(1, ncol(F1) - 1)], length)[1:(ncol(F1[,
                                                                                                                                                                                                                             c(1, ncol(F1) - 1)]) + 1)][, 3] >= 2), 1:2]
        redundancyF1 <- rep(0, nrow(F1))
        redundancyF1[which(apply(F1[, c(1, ncol(F1) - 1)], 1, paste, collapse = "") %in%
                             apply(f1, 1, paste, collapse = ""))] <- 1
        correspondenceAB <- data.frame(cbind(rbind(F1, F2), c(redundancyF1,
                                                              rep(0, nrow(F2)))))
        
        # Creation of the unmatched flag for the correspondence table
        # A:B.
        correspondenceAB <- data.frame(correspondenceAB, 1)
        colnames(correspondenceAB) <- c(paste(colnames(RRR[[1]][1])), paste(unlist(lapply(RRR,
                                                                                          function(x) {
                                                                                            colnames(x)[1]
                                                                                          }))[seq(k) + 1]), paste(colnames(RRR[[k + 2]][1])), "Review", "Redundancy",
                                        "Unmatched")
        
      } else if (Reference == "none") {
        idx <- k + 4
        
        # Creation of the redundancy flag for the correspondence table
        # A:B.
        F1 <- data.frame(F[apply(F, 1, function(x) {
          length(which(x == ""))
        }) == 0, ])
        F1 <- matrix(unlist(F1), ncol = k + 2)
        F1 <- data.frame(F1)
        F2 <- data.frame(F[apply(F, 1, function(x) {
          length(which(x == ""))
        }) >= 1, ])
        F2 <- matrix(unlist(F2), ncol = k + 2)
        F2 <- data.frame(F2)
        colnames(F2) <- colnames(F1)
        f1 <- stats::aggregate(F1[, c(1, ncol(F1))], by = F1[, c(1, ncol(F1))],
                               length)[1:(ncol(F1[, c(1, ncol(F1))]) + 1)][which(stats::aggregate(F1[,
                                                                                                     c(1, ncol(F1))], by = F1[, c(1, ncol(F1))], length)[1:(ncol(F1[,
                                                                                                                                                                    c(1, ncol(F1))]) + 1)][, 3] >= 2), 1:2]
        redundancyF1 <- rep(0, nrow(F1))
        redundancyF1[which(apply(F1[, c(1, ncol(F1))], 1, paste, collapse = "") %in%
                             apply(f1, 1, paste, collapse = ""))] <- 1
        correspondenceAB <- data.frame(cbind(rbind(F1, F2), c(redundancyF1,
                                                              rep(0, nrow(F2)))))
        
        # Creation of the unmatched flag for the correspondence table
        # A:B.
        correspondenceAB <- data.frame(correspondenceAB, 1)
        colnames(correspondenceAB) <- c(paste(colnames(RRR[[1]][1])), paste(unlist(lapply(RRR,
                                                                                          function(x) {
                                                                                            colnames(x)[1]
                                                                                          }))[seq(k) + 1]), paste(colnames(RRR[[k + 2]][1])), "Redundancy",
                                        "Unmatched")
        
      }
    } else {
      if (Reference %in% c("A", "B")) {
        Review <- rep(0, nrow(F))
        Redundancy <- rep(0, nrow(F))
        Unmatched <- rep(1, nrow(F))
        correspondenceAB <- data.frame(cbind(F, Review, Redundancy, Unmatched))
        colnames(correspondenceAB) <- c(paste(colnames(RRR[[1]][1])), paste(unlist(lapply(RRR,
                                                                                          function(x) {
                                                                                            colnames(x)[1]
                                                                                          }))[seq(k) + 1]), paste(colnames(RRR[[k + 2]][1])), "Review", "Redundancy",
                                        "Unmatched")
      }
      if (Reference == "none") {
        Redundancy <- rep(0, nrow(F))
        Unmatched <- rep(1, nrow(F))
        correspondenceAB <- data.frame(cbind(F, Redundancy, Unmatched))
        colnames(correspondenceAB) <- c(paste(colnames(RRR[[1]][1])), paste(unlist(lapply(RRR,
                                                                                          function(x) {
                                                                                            colnames(x)[1]
                                                                                          }))[seq(k) + 1]), paste(colnames(RRR[[k + 2]][1])), "Redundancy",
                                        "Unmatched")
      }
    }
    
    
    # The final Unmatched and the NoMatchFrom flags are created
    NoMatchFromA <- rep("", nrow(correspondenceAB))
    NoMatchFromB <- rep("", nrow(correspondenceAB))
    correspondenceAB <- cbind(correspondenceAB, NoMatchFromA, NoMatchFromB)
    
    inA <- which(is.na(match(unlist(RRR[[1]][, 1]), correspondenceAB[, 1])) == TRUE)
    if (length(inA) >= 1) {
      InA <- cbind(matrix(RRR[[1]][inA, 1], length(inA), 1), matrix("", length(inA),
                                                                    idx - 1))
      InA <- cbind(InA, matrix("", length(inA), 2))
      InA <- data.frame(InA)
      colnames(InA) <- colnames(correspondenceAB)
      correspondenceAB <- rbind(correspondenceAB, InA)
    }
    
    inB <- which(is.na(match(unlist(RRR[[nrow(x)]][, 1]), correspondenceAB[, k + 2])) ==
                   TRUE)
    if (length(inB) >= 1) {
      InB <- cbind(matrix("", length(inB), k + 1), matrix(RRR[[nrow(x)]][inB,
                                                                         1], length(inB), 1), matrix("", length(inB), idx - k - 2))
      InB <- cbind(InB, matrix("", length(inB), 2))
      InB <- data.frame(InB)
      colnames(InB) <- colnames(correspondenceAB)
      correspondenceAB <- rbind(correspondenceAB, InB)
    }
    
    yesA <- which(!is.na(match(correspondenceAB[, 1], unlist(RRR[[1]][, 1]))) == TRUE)
    yesAC1 <- which(!is.na(match(correspondenceAB[, 1], unlist(RRR[[nrow(x) + 1]][,
                                                                                  1]))) == TRUE)
    noAC1 <- which(is.na(match(correspondenceAB[, 1], unlist(RRR[[nrow(x) + 1]][, 1]))) ==
                     TRUE)
    
    correspondenceAB$NoMatchFromA[intersect(yesA, yesAC1)] <- 0
    correspondenceAB$NoMatchFromA[intersect(yesA, noAC1)] <- 1
    
    yesB <- which(!is.na(match(correspondenceAB[, k + 2], unlist(RRR[[nrow(x)]][, 1]))) ==
                    TRUE)
    yesBCk <- which(!is.na(match(correspondenceAB[, k + 2], unlist(RRR[[length(RRR)]][,
                                                                                      1]))) == TRUE)
    noBCk <- which(is.na(match(correspondenceAB[, k + 2], unlist(RRR[[length(RRR)]][,
                                                                                    1]))) == TRUE)
    
    correspondenceAB$NoMatchFromB[intersect(yesB, yesBCk)] <- 0
    correspondenceAB$NoMatchFromB[intersect(yesB, noBCk)] <- 1
    
    yesFinalA <- which(correspondenceAB[, 1] != "")
    yesFinalB <- which(correspondenceAB[, k + 2] != "")
    correspondenceAB$Unmatched <- 1
    correspondenceAB$Unmatched[intersect(yesFinalA, yesFinalB)] <- 0
    
    if ((Reference %in% c("A", "B"))) {
      correspondenceAB$Review[which(correspondenceAB[, 1] == "")] <- ""
      correspondenceAB$Review[which(correspondenceAB[, k + 2] == "")] <- ""
    }
    
    # Final redundancy flag
    correspondenceAB$Redundancy <- 0
    f1 <- stats::aggregate(correspondenceAB[, c(1, k + 2)], by = correspondenceAB[,
                                                                                  c(1, k + 2)], length)[1:(ncol(correspondenceAB[, c(1, k + 2)]) + 1)][which(stats::aggregate(correspondenceAB[,
                                                                                                                                                                                               c(1, k + 2)], by = correspondenceAB[, c(1, k + 2)], length)[1:(ncol(correspondenceAB[,
                                                                                                                                                                                                                                                                                    c(1, k + 2)]) + 1)][, 3] >= 2), 1:2]
    correspondenceAB$Redundancy[which(apply(correspondenceAB[, c(1, k + 2)],
                                            1, paste, collapse = " ") %in% apply(f1, 1, paste, collapse = " "))] <- 1
    
  }, error = function(e) {
    stop(simpleError(paste("An error has occurred and execution needs to stop. Please check the input data. \n Details line 1734:\n",e)))
  })
  
  message("\n")
  
  # Check the number of the unmatched codes.
  if (length(which(as.vector(correspondenceAB$Unmatched) == 1))/nrow(correspondenceAB) <
      MismatchTolerance) {
    
    tryCatch({
      
      # The following if statement is applied if there are any
      # supplementary information for the classification A, in order to
      # be adjusted next to the correspondence table A:B.
      if (ncol(RRR[[1]]) >= 2) {
        A1 <- RRR[[1]][match(correspondenceAB[, 1], unlist(RRR[[1]][, 1])),
                       2:ncol(RRR[[1]])]
        A1[is.na(A1)] <- ""
        A1 <- matrix(unlist(A1), ncol = length(2:ncol(RRR[[1]])))
        colnames(A1) <- paste(paste(colnames(RRR[[1]])[1]), colnames(RRR[[1]])[2:ncol(RRR[[1]])],
                              sep = "_")
        correspondenceAB <- cbind(correspondenceAB, A1)
      }
      
      # The following for loop is applied for the classfications C1, C2,
      # ..., Ck.
      for (i1 in c(2:(((length(RRR) + 1)/2) - 1))) {
        
        # The if statement is applied if there are any supplementary
        # information for the classfications C1, C2, ..., Ck, in order
        # to be adjusted next to the correspondence table A:B.
        if (ncol(RRR[[i1]]) >= 2) {
          A1 <- RRR[[i1]][match(correspondenceAB[, i1], unlist(RRR[[i1]][,
                                                                         1])), 2:ncol(RRR[[i1]])]
          A1[is.na(A1)] <- ""
          A1 <- matrix(unlist(A1), ncol = length(2:ncol(RRR[[i1]])))
          colnames(A1) <- paste(paste(colnames(RRR[[i1]])[1]), colnames(RRR[[i1]])[2:ncol(RRR[[i1]])],
                                sep = "_")
          correspondenceAB <- cbind(correspondenceAB, A1)
        }
        
      }
      
      # The following if statement is applied if there are any
      # supplementary information for the classification B, in order to
      # be adjusted next to the correspondence table A:B.
      if (ncol(RRR[[(length(RRR) + 1)/2]]) >= 2) {
        A1 <- RRR[[(length(RRR) + 1)/2]][match(correspondenceAB[, (length(RRR) +
                                                                     1)/2], unlist(RRR[[(length(RRR) + 1)/2]][, 1])), 2:ncol(RRR[[(length(RRR) +
                                                                                                                                     1)/2]])]
        A1[is.na(A1)] <- ""
        A1 <- matrix(unlist(A1), ncol = length(2:ncol(RRR[[(length(RRR) +
                                                              1)/2]])))
        colnames(A1) <- paste(paste(colnames(RRR[[k + 2]])[1]), colnames(RRR[[(length(RRR) +
                                                                                 1)/2]])[2:ncol(RRR[[(length(RRR) + 1)/2]])], sep = "_")
        correspondenceAB <- cbind(correspondenceAB, A1)
      }
      
      # Find which .csv files are the correspondence tables.
      Tail <- utils::tail(c(1:length(RRR)), (length(RRR) - 1)/2)
      
      # The following if statement is applied if there are any
      # supplementary information for the correspondence table A:C1, in
      # order to be adjusted next to the correspondence table A:B.
      if (ncol(RRR[[Tail[1]]]) >= 3) {
        A1 <- RRR[[Tail[1]]][match(data.frame(t(correspondenceAB[, 1:2])),
                                   data.frame(t(RRR[[Tail[1]]][, 1:2]))), 3:ncol(RRR[[Tail[1]]])]
        A1[is.na(A1)] <- ""
        A1 <- matrix(unlist(A1), ncol = length(3:ncol(RRR[[Tail[1]]])))
        colnames(A1) <- paste(paste(colnames(RRR[[Tail[1]]])[1]), colnames(RRR[[Tail[1]]])[3:ncol(RRR[[Tail[1]]])],
                              sep = "_")
        correspondenceAB <- cbind(correspondenceAB, A1)
      }
      
      # The following if statement is applied if there are any
      # supplementary information for the correspondence tables (C1:C2 -
      # C2:C3), (C2:C3 - C3:C4), ..., (C(k-2):C(k-1) - C(k-1):Ck), in
      # order to be adjusted next to the correspondence table A:B.
      if (length(Tail) >= 3) {
        for (i2 in 2:(length(Tail) - 1)) {
          if (ncol(RRR[[Tail[i2]]]) >= 3) {
            A1 <- RRR[[Tail[i2]]][match(data.frame(t(correspondenceAB[, c(i2,
                                                                          i2 + 1)])), data.frame(t(RRR[[Tail[i2]]][, 1:2]))), 3:ncol(RRR[[Tail[i2]]])]
            A1[is.na(A1)] <- ""
            A1 <- matrix(unlist(A1), ncol = length(3:ncol(RRR[[Tail[i2]]])))
            colnames(A1) <- paste(paste(colnames(RRR[[Tail[i2]]])[1]), colnames(RRR[[Tail[i2]]])[3:ncol(RRR[[Tail[i2]]])],
                                  sep = "_")
            correspondenceAB <- cbind(correspondenceAB, A1)
          }
        }
      }
      
      # The following if statement is applied if there are any
      # supplementary information for the correspondence table B:Ck, in
      # order to be adjusted next to the correspondence table A:B.
      if (ncol(RRR[[Tail[length(Tail)]]]) >= 3) {
        A1 <- RRR[[Tail[length(Tail)]]][match(data.frame(t(correspondenceAB[,
                                                                            c(((length(RRR) + 1)/2) - 1, (length(RRR) + 1)/2)])), data.frame(t(RRR[[Tail[length(Tail)]]][,
                                                                                                                                                                         c(2, 1)]))), 3:ncol(RRR[[Tail[length(Tail)]]])]
        A1[is.na(A1)] <- ""
        A1 <- matrix(unlist(A1), ncol = length(3:ncol(RRR[[Tail[length(Tail)]]])))
        colnames(A1) <- paste(paste(colnames(RRR[[Tail[length(Tail)]]])[1]),
                              colnames(RRR[[Tail[length(Tail)]]])[3:ncol(RRR[[Tail[length(Tail)]]])],
                              sep = "_")
        correspondenceAB <- cbind(correspondenceAB, A1)
      }
    }, error = function(e) {
      stop(simpleError(paste("An error has occurred and execution needs to stop. Please check the input data. \n Details line 1841: \n",e)))
    })
    
  } else {
    # Error message in case the percentage of unmatched codes between A and
    # B is larger than the desired threshold.
    stop("Too many codes in either of classifications A and B cannot be mapped to any code in the other one.\n",
         round(length(which(as.vector(correspondenceAB$Unmatched) == 1))/nrow(correspondenceAB)*100,2),"% is unmatched which exceeds the mismatch tolerance of ", MismatchTolerance)
  }
  
  
  tryCatch({

    # The final correspondence table A:B is sorted, firstly, based on
    # classification A, and then, based on classification B.
    correspondenceAB <- correspondenceAB[order(correspondenceAB[, 1], correspondenceAB[,
                                                                                       (length(RRR) + 1)/2], decreasing = FALSE), ]
     
    
    # Redundancy_trim parameter (MP)
    # Find the columns which are related to linking datasets which values need to be recorded as "Multiple"
    ## 2 + n_link_data*2 + 1 = n_data
    num_link = (length(test.names) - 3)/2
    col_multiple = numeric(0)
    for (nl in 1:num_link){
      col_multiple = unique(c(col_multiple, grep(colnames(correspondenceAB)[1 + nl], colnames(correspondenceAB), value = T)))
    }
    max_col = num_link + 2
    
    #Do redundancy trim only if redundancy is there (MP) 
    if (length(which(correspondenceAB$Redundancy == 1)) != 0){
    
    # Find unique combination of A and B and identify them with a number
    uniqueAB = unique(correspondenceAB[which(correspondenceAB$Redundancy == 1),c(1, max_col)])
    uniqueAB$id_to_use = 1:nrow(uniqueAB)
    
    correspondenceAB = merge(correspondenceAB, uniqueAB, by = colnames(correspondenceAB)[c(1,max_col)], all.x = TRUE)[, union(names(correspondenceAB), names(uniqueAB))]
    col_link = grep("id_to_use", colnames(correspondenceAB), value = T)

    ### new but probably slower 
    if (Redundancy_trim == TRUE){
      x_temp = split(correspondenceAB[which(correspondenceAB$Redundancy == 1), col_multiple], correspondenceAB[which(correspondenceAB$Redundancy == 1), col_link])
      
      for (i in 1:nrow(uniqueAB)){ 
        multiple_values = apply(x_temp[[i]], 2, function(x) length(unique(x)))
        x_change = which(multiple_values != 1)
        #replace with multiple
        col_multiple_temp = c(col_multiple)[x_change]
        correspondenceAB[which(correspondenceAB$Redundancy == 1 & correspondenceAB[,col_link] == i), unique(col_multiple_temp)] = "Multiple"
      }
      
      correspondenceAB = correspondenceAB[,!names(correspondenceAB) %in% "id_to_use"]
      
      ### old but probably faster  
      ##replace with multiple
      ##replace with multiple
      #correspondenceAB[which(correspondenceAB$Redundancy == 1), unique(col_multiple)] = "Multiple"
      
      #eliminate duplicates
      dup = as.numeric(duplicated(correspondenceAB[,1:max_col]))
      correspondenceAB = correspondenceAB[which(dup == 0), ]
      
    }
    } else {

      correspondenceAB = correspondenceAB
    }
    
    if (Redundancy_trim==FALSE){
      #add a redundancy keep flag to indicate which row will be kept
      dup = as.numeric(duplicated(correspondenceAB[,c(1,max_col)]))
      red_col = which(colnames(correspondenceAB) == "Redundancy")
      correspondenceAB$Redundancy_keep = rep(0, nrow(correspondenceAB))
      correspondenceAB$Redundancy_keep[which(dup == "0" & correspondenceAB$Redundancy == "1")] = 1
      correspondenceAB = correspondenceAB[,c(1:red_col, ncol(correspondenceAB), (red_col+1):(ncol(correspondenceAB)-1))]
      correspondenceAB = correspondenceAB[,!names(correspondenceAB) %in% "id_to_use"]
    }
    
    
    # Create a data frame that contains the names of the classifications.
    CsvNames <- data.frame(matrix(0, k + 2, 1))
    
    CsvNames[1, 1] <- paste("A:", colnames(correspondenceAB)[1], sep = " ")
    
    CsvNames[k + 2, 1] <- paste("B:", colnames(correspondenceAB)[k + 2], sep = " ")
    
    for (i3 in seq(k) + 1) {
      CsvNames[i3, 1] <- paste(paste("C", i3 - 1, ":", sep = ""), colnames(correspondenceAB)[i3],
                               sep = " ")
    }
    
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
    }
    
    colnames(CsvNames) <- paste("Classification:", "Name", sep = " ")
    
    # Create a data frame that contains the final correspondence table
    # (final desired table).
    Final <- apply(correspondenceAB, 2, function(x) {
      gsub(" ", " ", x)
    })
    
    
    if (is.null(dim(Final))) {
      Final <- t(data.frame(Final))
      rownames(Final) <- 1
    }
    
    
  }, error = function(e) {
    stop(simpleError(paste("An error has occurred and execution needs to stop. Please check the input data. \n Deatils line 1895:\n",e)))
  })
  
  # Check so as to write (or not) the final correspondence table (final
  # desired table) as well as the names of classifications in two seperate
  # csv files.
  tryCatch({
    
    if (!is.null(CSVout)) {
      data.table::fwrite(data.frame(Final, check.names = FALSE), file = CSVout, quote = TRUE)
      utils::write.csv(CsvNames, file = paste0(Name1, "classificationNames_", Name2),
                       row.names = FALSE)
    }
    
  }, error = function(e) {
    stop(simpleError("An error occurred while trying to write the output to the specified files. Please check the respective input parameters."))
  })
  
  # The final list that contains the final correspondence table (final
  # desired table) as a data frame as well as the names of classifications as
  # a data frame.
  tryCatch({
    
    FinalResults <- list()
    FinalResults[[1]] <- data.frame(Final, check.names = FALSE, row.names = NULL)
    FinalResults[[2]] <- CsvNames
    names(FinalResults) <- c("newCorrespondenceTable", "classificationNames")
    
    # newCorrespondenceTable function returns the final correspondence
    # table A:B, that contains the pivot classifications C1, C2, ..., Ck,
    # as well as any supplementary information about the classification
    # tables A, C1, C2, ..., Ck, B, and the correspondence tables A:C1,
    # (C1:C2 - C2:C3), (C2:C3 - C3:C4), ..., (C(k-2):C(k-1) - C(k-1):Ck),
    # B:Ck.
    
    return(FinalResults)
  }, error = function(e) {
    stop(simpleError(paste("An error has occurred and execution needs to stop. Please check the input data. \n Details line 1946:\n",e)))
  })
  
}

