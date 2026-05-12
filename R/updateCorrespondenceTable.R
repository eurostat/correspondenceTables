#' @title Correspondence table creation
#'
#' @description
#' Update the correspondence table between two classifications when one of them has been updated.
#'
#' @param A A data frame containing the original classification A. The first column holds the codes;
#'          optional subsequent columns may hold labels and supplementary info.
#' @param B A data frame containing the target classification B. Same structure expectations as \code{A}.
#' @param AStar A data frame containing the updated version A*. Same structure expectations as \code{A}.
#' @param AB A data frame containing the previous correspondence table A:B (at least two columns: A-codes, B-codes).
#' @param AAStar A data frame containing the concordance table A:A* (at least two columns: A-codes, A*-codes).
#' @param Reference The reference classification among A and B. Valid values: \code{"none"}, \code{"A"}, \code{"B"}.
#'                  If \code{"A"} or \code{"B"}, a "Review" flag column is included (see "Explanation of the flags").
#' @param MismatchToleranceB Maximum acceptable proportion of rows in the updated table with no code of B
#'                           among those that have a code for A and/or A*. Default \code{0.2}, range [0,1].
#' @param MismatchToleranceAStar Maximum acceptable proportion of rows in the updated table with no code of A*
#'                               among those that have a code for A and/or B. Default \code{0.2}, range [0,1].
#' @param Redundancy_trim Logical. If \code{TRUE} (default) trims correct redundant records by collapsing A-side fields to "Multiple"
#'                        when appropriate; if \code{FALSE}, keeps redundancies and adds a \code{Redundancy_keep} indicator.
#'
#' @details
#' *Input data frame requirements*
#' \itemize{
#'   \item \code{A}, \code{B}, \code{AStar}: at least 1 column and at least 1 row. The first column contains the codes; the first row is the header.
#'   \item \code{AB}, \code{AAStar}: at least 2 columns and at least 1 row; the first two columns contain code pairs; the first row is the header.
#'   \item Codes in the first column of \code{A}, \code{B}, \code{AStar} must be unique (no duplicates).
#'   \item Pairs (first two columns) in \code{AB} and \code{AAStar} must be unique (no duplicate pairs).
#'   \item If additional columns are present in classification or correspondence tables, the second column is treated as a label.
#'   \item All columns are coerced to character.
#' }
#'
#' *Minimum interdependencies*
#' \itemize{
#'   \item At least one code of A must appear in both \code{AAStar} and \code{AB}.
#'   \item At least one code of A* must appear in both \code{AStar} and \code{AAStar}.
#'   \item At least one code of B must appear in both \code{B} and \code{AB}.
#' }
#'
#' *Mismatch tolerance*
#' \itemize{
#'   \item If the share of rows with \code{NoMatchToAStar == 1} exceeds \code{MismatchToleranceAStar}, the function stops with an error.
#'   \item If the share of rows with \code{NoMatchToB == 1} exceeds \code{MismatchToleranceB}, the function stops with an error.
#' }
#'
#' @return
#' \code{updateCorrespondenceTable()} returns a list with two data frames:
#' \itemize{
#'   \item \code{updateCorrespondenceTable}: the updated correspondence A*:B with flags "CodeChange", "Review" (if applicable),
#'         "Redundancy", "NoMatchToAStar", "NoMatchToB", "NoMatchFromAStar", "NoMatchFromB", "LabelChange",
#'         plus any additional columns coming from \code{A}, \code{B}, \code{AStar}, \code{AB}, \code{AAStar}.
#'   \item \code{classificationNames}: the names of the classifications (A, B, A*) read from the first column headers.
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
#'
#' @examples
#' \dontrun{
#' 
#'   # Read CSVs outside, pass data frames in:
#'   
#'   A_df      <- utils::read.csv(system.file("extdata/test", "NAICS2017.csv",
#'   package = "correspondenceTables"),
#'   sep = ",", header = TRUE, check.names = FALSE, 
#'   colClasses = "character", encoding = "UTF-8")
#'                                
#'   AStar_df  <- utils::read.csv(system.file("extdata/test", "NAICS2022.csv",
#'   package = "correspondenceTables"),
#'   sep = ",", header = TRUE, check.names = FALSE,
#'   colClasses = "character", encoding = "UTF-8")
#'                                
#'   B_df      <- utils::read.csv(system.file("extdata/test", "NACE.csv",
#'   package = "correspondenceTables"),
#'   sep = ",", header = TRUE, check.names = FALSE,
#'   colClasses = "character", encoding = "UTF-8")
#'   
#'   AB_df     <- utils::read.csv(system.file("extdata/test", "NAICS2017_NACE.csv",
#'   package = "correspondenceTables"),
#'   sep = ",", header = TRUE, check.names = FALSE,
#'   colClasses = "character", encoding = "UTF-8")
#'                                
#'   AAStar_df <- utils::read.csv(system.file("extdata/test", "NAICS2017_NAICS2022.csv",
#'   package = "correspondenceTables"),
#'   sep = ",", header = TRUE, check.names = FALSE,
#'   colClasses = "character", encoding = "UTF-8")
#'
#'   UPC <- updateCorrespondenceTable(
#'     A = A_df, B = B_df, AStar = AStar_df, AB = AB_df, AAStar = AAStar_df,
#'     Reference = "none", MismatchToleranceB = 0.5, MismatchToleranceAStar = 0.3,
#'     Redundancy_trim = FALSE
#'   )
#'
#'   summary(UPC)
#'   head(UPC$updateCorrespondenceTable)
#'   UPC$classificationNames
#' }
#'
#' @export
updateCorrespondenceTable <- function(
    A, B, AStar, AB, AAStar,
    Reference = "none",
    MismatchToleranceB = 0.2,
    MismatchToleranceAStar = 0.2,
    Redundancy_trim = TRUE
) {
  # --- Helpers ---------------------------------------------------------------
  arg_names <- vapply(as.list(substitute(list(A, B, AStar, AB, AAStar)))[-1], deparse, "")
  names(arg_names) <- c("A", "B", "AStar", "AB", "AAStar")
  
  stop_if_not_df <- function(x, nm) {
    if (!is.data.frame(x)) {
      stop(
        simpleError(
          paste0(
            "As of this version, `updateCorrespondenceTable()` accepts only data frames. ",
            "Argument `", nm, "` is not a data frame.\n",
            "-> Read the CSV first (e.g., `read.csv()` or `readr::read_csv()`), then pass the data frame."
          )
        )
      )
    }
  }
  
  ensure_df_chars <- function(x, what) {
    stop_if_not_df(x, what)
    x[] <- lapply(x, function(col) if (is.factor(col)) as.character(col) else as.character(col))
    colnames(x) <- gsub("\\xef\\xbb\\xbf", "", colnames(x), useBytes = TRUE)  # strip BOM if present
    x
  }
  
  # --- Validate scalar params ------------------------------------------------
  if (!(Reference %in% c("A", "B", "none"))) {
    stop(simpleError('You entered a non-allowed value for Reference. Allowed: "A", "B", "none".'))
  }
  if (is.character(MismatchToleranceB) || MismatchToleranceB < 0 || MismatchToleranceB > 1) {
    stop(simpleError("MismatchToleranceB must be numeric in [0, 1]."))
  }
  if (is.character(MismatchToleranceAStar) || MismatchToleranceAStar < 0 || MismatchToleranceAStar > 1) {
    stop(simpleError("MismatchToleranceAStar must be numeric in [0, 1]."))
  }
  
  # --- Coerce/validate inputs as character data frames -----------------------
  classA     <- ensure_df_chars(A,      "A")
  classB     <- ensure_df_chars(B,      "B")
  classAStar <- ensure_df_chars(AStar,  "AStar")
  corrAB     <- ensure_df_chars(AB,     "AB")
  corrAAStar <- ensure_df_chars(AAStar, "AAStar")
  
  # --- Remove rows with empty first column & non-empty second in corr tables --
  NoCorrAAStar <- if (length(which(corrAAStar[, 1] == "" & corrAAStar[, 2] != "")) >= 1) {
    corrAAStar[-which(corrAAStar[, 1] == "" & corrAAStar[, 2] != ""), ]
  } else corrAAStar
  
  NoCorrAB <- if (length(which(corrAB[, 1] == "" & corrAB[, 2] != "")) >= 1) {
    corrAB[-which(corrAB[, 1] == "" & corrAB[, 2] != ""), ]
  } else corrAB
  
  # --- Dimension checks ------------------------------------------------------
  test.dimClass <- list(classA, classB, classAStar)
  for (i in 1:3) {
    if (ncol(test.dimClass[[i]]) < 1 || nrow(test.dimClass[[i]]) < 1) {
      stop(simpleError(
        paste0("Argument `", names(arg_names)[i], "` should have at least one column and one row (headers included).")
      ))
    }
  }
  
  test.dimCorr <- list(corrAB, corrAAStar)
  for (i in 1:2) {
    if (ncol(test.dimCorr[[i]]) <= 1 || nrow(test.dimCorr[[i]]) < 1) {
      stop(simpleError(
        paste0("Argument `", names(arg_names)[i + 3], "` should have at least two columns and one row (headers included).")
      ))
    }
  }
  
  # --- Uniqueness checks -----------------------------------------------------
  for (i in 1:3) {
    if (sum(duplicated(test.dimClass[[i]][, 1])) >= 1) {
      stop(simpleError(
        paste0("At least one code of ", colnames(test.dimClass[[i]])[1],
               " appears more than once in `", names(arg_names)[i], "`. Each code must appear only once.")
      ))
    }
  }
  
  for (i in 1:2) {
    if (nrow(test.dimCorr[[i]][, 1:2]) != nrow(unique(test.dimCorr[[i]][, 1:2]))) {
      stop(simpleError(
        paste0("At least one pair of codes (", colnames(test.dimCorr[[i]])[1], ", ",
               colnames(test.dimCorr[[i]])[2], ") appears more than once in `",
               names(arg_names)[i + 3], "`. Each pair must be unique.")
      ))
    }
  }
  
  # --- Interdependency checks ------------------------------------------------
  if (sum(!is.na(match(classAStar[, 1], corrAAStar[, 2]))) == 0) {
    stop(simpleError(
      paste0("No code of ", colnames(classAStar)[1], " appears in both `AStar` and `AAStar`. At least one is required.")
    ))
  }
  if (sum(!is.na(match(corrAAStar[, 1], corrAB[, 1]))) == 0) {
    stop(simpleError(
      paste0("No code of ", colnames(corrAAStar)[1], " appears in both `AAStar` and `AB`. At least one is required.")
    ))
  }
  if (sum(!is.na(match(classB[, 1], corrAB[, 2]))) == 0) {
    stop(simpleError(
      paste0("No code of ", colnames(classB)[1], " appears in both `B` and `AB`. At least one is required.")
    ))
  }
  if (sum(!is.na(match(classA[, 1], corrAAStar[, 1]))) == 0) {
    message(paste0("WARNING: No code of ", colnames(classA)[1], " appears in both `A` and `AAStar`. Please double-check."))
  }
  if (sum(!is.na(match(classA[, 1], corrAB[, 1]))) == 0) {
    message(paste0("WARNING: No code of ", colnames(classA)[1], " appears in both `A` and `AB`. Please double-check."))
  }
  
  # --- Set indices based on Reference ---------------------------------------
  tryCatch({
    if (Reference == "A") {
      idx <- 2; idx.thres <- 7
    } else if (Reference == "B") {
      idx <- 1; idx.thres <- 7
    } else { # "none"
      idx <- 3; idx.thres <- 6
    }
    
    # --- codeChange ----------------------------------------------------------
    codeChange <- function(X) {
      diffX <- as.matrix(X[, 1:2])
      Diff <- rep(0, nrow(diffX))
      Diff[which(diffX[, 1] != diffX[, 2])] <- 1
      difference <- data.frame(diffX, Diff)
      
      dif1 <- difference[order(difference[, 1]), ]
      dif1.values <- lapply(split(seq_along(dif1[, 1]), dif1[, 1]), function(x) dif1[x, 3])
      dif1 <- cbind(dif1, unlist(mapply(rep, lapply(dif1.values, max), lapply(dif1.values, length))))
      
      dif2 <- difference[order(difference[, 2]), ]
      dif2.values <- lapply(split(seq_along(dif2[, 2]), dif2[, 2]), function(x) dif2[x, 3])
      dif2 <- cbind(dif2, unlist(mapply(rep, lapply(dif2.values, max), lapply(dif2.values, length))))
      
      dif2.new <- dif2[order(dif2[, 1]), ]
      final.diff <- cbind(dif1, dif2.new[, 4])
      final.diff <- cbind(dif1, dif2.new[, 4], apply(final.diff[, 4:5], 1, max))
      final.diff
    }
    
    # --- review --------------------------------------------------------------
    review <- function(X, Y) {
      X1 <- X[!is.na(match(X[, 1], Y[, 1])), ]
      Y1 <- Y[!is.na(match(Y[, 1], X[, 1])), ]
      X1 <- X1[order(X1[, 1]), ]
      Y1 <- Y1[order(Y1[, 1]), ]
      
      x1 <- unlist(lapply(split(seq_along(X1[, 1]), X1[, 1]), length))
      y1 <- unlist(lapply(split(seq_along(Y1[, 1]), Y1[, 1]), length))
      Review <- data.frame(
        X1[rep(1:nrow(X1), rep(y1, x1)), 1:2],
        Y1[unlist(rep(split(seq_along(Y1[, 1]), Y1[, 1]), x1)), 2],
        0
      )
      colnames(Review) <- c("R1", "R2", "R3", "R4")
      
      q1 <- 0
      if (length(which(is.na(match(Y[, 1], X[, 1])) == TRUE)) >= 1) {
        Y2 <- matrix(unlist(Y[is.na(match(Y[, 1], X[, 1])), 1:2]), ncol = 2)
        Y2 <- cbind(Y2[, 1], rep("", nrow(Y2)), Y2[, 2], 0); colnames(Y2) <- c("R1", "R2", "R3", "R4")
        q1 <- 1
      }
      
      q2 <- 0
      if (length(which(is.na(match(X[, 1], Y[, 1])) == TRUE)) >= 1) {
        YY2 <- matrix(unlist(X[is.na(match(X[, 1], Y[, 1])), 1:2]), ncol = 2)
        YY2 <- cbind(YY2, rep("", nrow(YY2)), 0); colnames(YY2) <- c("R1", "R2", "R3", "R4")
        q2 <- 1
      }
      
      if (q1 == 1) Review <- rbind(Review, Y2)
      if (q2 == 1) Review <- rbind(Review, YY2)
      Review <- Review[!duplicated(Review[, c(1:3)]), ]
      
      F1 <- Review[apply(Review, 1, function(x) length(which(x == ""))) == 0, ]
      F2 <- Review[apply(Review, 1, function(x) length(which(x == ""))) >= 1, ]
      f  <- stats::aggregate(unique(F1[, 2:3])[, idx], list(num = unique(F1[, 2:3])[, idx]), length)[
        which(stats::aggregate(unique(F1[, 2:3])[, idx], list(num = unique(F1[, 2:3])[, idx]), length)[, 2] > 1), 1
      ]
      F1[which(F1[, idx + 1] %in% f), 4] <- 1
      Review <- rbind(F1, F2)
      ChangeReview <- data.frame(Review[, 1:3], 0, Review[, 4])
      ChangeReview[which(ChangeReview[, 1] %in% unique(codeChange(X)[which(codeChange(X)[, 6] == 1), 1])), 4] <- 1
      ChangeReview[which(ChangeReview[, 2] == ""), 4] <- ""
      ChangeReview
    }
    
    # --- redundancy (unchanged core) ----------------------------------------
    X1 <- NoCorrAAStar[!is.na(match(NoCorrAAStar[, 1], NoCorrAB[, 1])), ]
    Y1 <- NoCorrAB[!is.na(match(NoCorrAB[, 1], NoCorrAAStar[, 1])), ]
    X1 <- X1[order(X1[, 1]), ]; Y1 <- Y1[order(Y1[, 1]), ]
    x1 <- unlist(lapply(split(seq_along(X1[, 1]), X1[, 1]), length))
    y1 <- unlist(lapply(split(seq_along(Y1[, 1]), Y1[, 1]), length))
    Redun <- data.frame(X1[rep(1:nrow(X1), rep(y1, x1)), 1:2],
                        Y1[unlist(rep(split(seq_along(Y1[, 1]), Y1[, 1]), x1)), 2], 0)
    colnames(Redun) <- c("R1","R2","R3","R4")
    
    q1 <- 0
    if (length(which(is.na(match(NoCorrAB[, 1], NoCorrAAStar[, 1])) == TRUE)) >= 1) {
      Y2 <- matrix(unlist(NoCorrAB[is.na(match(NoCorrAB[, 1], NoCorrAAStar[, 1])), 1:2]), ncol = 2)
      Y2 <- cbind(Y2[, 1], rep("", nrow(Y2)), Y2[, 2], 0); colnames(Y2) <- c("R1","R2","R3","R4")
      q1 <- 1
    }
    q2 <- 0
    if (length(which(is.na(match(NoCorrAAStar[, 1], NoCorrAB[, 1])) == TRUE)) >= 1) {
      YY2 <- matrix(unlist(NoCorrAAStar[is.na(match(NoCorrAAStar[, 1], NoCorrAB[, 1])), 1:2]), ncol = 2)
      YY2 <- cbind(YY2, rep("", nrow(YY2)), 0); colnames(YY2) <- c("R1","R2","R3","R4")
      q2 <- 1
    }
    if (q1 == 1) Redun <- rbind(Redun, Y2)
    if (q2 == 1) Redun <- rbind(Redun, YY2)
    Redun <- Redun[!duplicated(Redun[, c(1:3)]), ]
    
    F1 <- Redun[apply(Redun, 1, function(x) length(which(x == ""))) == 0, ]
    F2 <- Redun[apply(Redun, 1, function(x) length(which(x == ""))) >= 1, ]
    
    if (idx != 3) {
      f1 <- stats::aggregate(F1[, 2:3], by = F1[, 2:3], length)[1:(ncol(F1[, 2:3]) + 1)][
        which(stats::aggregate(F1[, 2:3], by = F1[, 2:3], length)[1:(ncol(F1[, 2:3]) + 1)][, 3] >= 2), 1:2
      ]
      F1[which(apply(F1[, 2:3], 1, paste, collapse = " ") %in% apply(f1, 1, paste, collapse = " ")), 4] <- 1
      Redundancy <- rbind(F1, F2)
      ChangeReviewRedundancy <- data.frame(review(NoCorrAAStar, NoCorrAB), Redundancy[, 4])
      ChangeReviewRedundancy <- ChangeReviewRedundancy[order(ChangeReviewRedundancy[, 2], ChangeReviewRedundancy[, 3], decreasing = FALSE), ]
      List <- cbind(ChangeReviewRedundancy, 0, 0)
      List[which(ChangeReviewRedundancy[, 2] == ""), 7] <- 1
      List[which(ChangeReviewRedundancy[, 3] == ""), 8] <- 1
      colnames(List) <- c(colnames(classA[1]), colnames(classAStar[1]), colnames(classB[1]),
                          "CodeChange", "Review", "Redundancy", "NoMatchToAStar", "NoMatchToB")
    } else {
      if (nrow(F2) >= 1) { F2 <- cbind(F2, 0); colnames(F2) <- c("R1","R2","R3","R4","R5") }
      f1 <- stats::aggregate(F1[, 2:3], by = F1[, 2:3], length)[1:(ncol(F1[, 2:3]) + 1)][
        which(stats::aggregate(F1[, 2:3], by = F1[, 2:3], length)[1:(ncol(F1[, 2:3]) + 1)][, 3] >= 2), 1:2
      ]
      F1 <- data.frame(F1[, 1:3], 0, 0)
      F1[which(apply(F1[, 2:3], 1, paste, collapse = " ") %in% apply(f1, 1, paste, collapse = " ")), 5] <- 1
      colnames(F1) <- c("R1","R2","R3","R4","R5")
      ChangeRedundancy <- rbind(F1, F2)
      ChangeRedundancy[which(ChangeRedundancy[, 1] %in% unique(codeChange(NoCorrAAStar)[which(codeChange(NoCorrAAStar)[, 6] == 1), 1])), 4] <- 1
      ChangeRedundancy[which(ChangeRedundancy[, 2] == ""), 4] <- ""
      ChangeRedundancy <- ChangeRedundancy[order(ChangeRedundancy[, 2], ChangeRedundancy[, 3], decreasing = FALSE), ]
      List <- cbind(ChangeRedundancy, 0, 0)
      List[which(ChangeRedundancy[, 2] == ""), 6] <- 1
      List[which(ChangeRedundancy[, 3] == ""), 7] <- 1
      colnames(List) <- c(colnames(classA[1]), colnames(classAStar[1]), colnames(classB[1]),
                          "CodeChange", "Redundancy", "NoMatchToAStar", "NoMatchToB")
    }
    
    # --- Append missing-code rows & compute NoMatchFrom* ---------------------
    NoMatchFromAStar <- rep("", nrow(List))
    NoMatchFromB     <- rep("", nrow(List))
    List <- cbind(List, NoMatchFromAStar, NoMatchFromB)
    
    inA1 <- which(is.na(match(classA[, 1], corrAAStar[, 1])) == TRUE)
    inA2 <- which(is.na(match(classA[, 1], corrAB[, 1])) == TRUE)
    inA  <- intersect(inA1, inA2)
    if (length(inA) >= 1) {
      InA <- cbind(matrix(classA[inA, 1], length(inA), 1), matrix("", length(inA), 2), matrix("", length(inA), idx.thres - 2))
      InA <- cbind(InA, matrix("", length(inA), 2))
      InA <- data.frame(InA); colnames(InA) <- colnames(List); List <- rbind(List, InA)
    }
    
    inAStar <- which(is.na(match(classAStar[, 1], corrAAStar[, 2])) == TRUE)
    if (length(inAStar) >= 1) {
      InAStar <- cbind(matrix("", length(inAStar), 1), matrix(classAStar[inAStar, 1], length(inAStar), 1), matrix("", length(inAStar), idx.thres - 1))
      InAStar <- cbind(InAStar, matrix("", length(inAStar), 2))
      InAStar <- data.frame(InAStar); colnames(InAStar) <- colnames(List); List <- rbind(List, InAStar)
    }
    
    noInA <- which(corrAAStar[, 1] == "" & corrAAStar[, 2] != "")
    if (length(noInA) >= 1) {
      NoInA <- cbind(matrix("", length(noInA), 1), matrix(corrAAStar[noInA, 2], length(noInA), 1), matrix("", length(noInA), idx.thres - 1))
      NoInA <- cbind(NoInA, matrix("", length(noInA), 2))
      NoInA <- data.frame(NoInA); colnames(NoInA) <- colnames(List); List <- rbind(List, NoInA)
    }
    
    inB <- which(is.na(match(classB[, 1], corrAB[, 2])) == TRUE)
    if (length(inB) >= 1) {
      InB <- cbind(matrix("", length(inB), 2), matrix(classB[inB, 1], length(inB), 1), matrix("", length(inB), idx.thres - 2))
      InB <- cbind(InB, matrix("", length(inB), 2))
      InB <- data.frame(InB); colnames(InB) <- colnames(List); List <- rbind(List, InB)
    }
    
    noInB <- which(corrAB[, 1] == "" & corrAB[, 2] != "")
    if (length(noInB) >= 1) {
      NoInB <- cbind(matrix("", length(noInB), 2), matrix(corrAB[noInB, 2], length(noInB), 1), matrix("", length(noInB), idx.thres - 2))
      NoInB <- cbind(NoInB, matrix("", length(noInB), 2))
      NoInB <- data.frame(NoInB); colnames(NoInB) <- colnames(List); List <- rbind(List, NoInB)
    }
    
    yesAstarClass <- which(!is.na(match(List[, 2], classAStar[, 1])) == TRUE)
    yesAstarCorr  <- which(!is.na(match(List[, 2], corrAAStar[, 2])) == TRUE)
    noAstarCorr   <- which(is.na(match(List[, 2], corrAAStar[, 2])) == TRUE)
    List$NoMatchFromAStar[intersect(yesAstarClass, yesAstarCorr)] <- 0
    List$NoMatchFromAStar[intersect(yesAstarClass, noAstarCorr)]  <- 1
    
    yesBClass <- which(!is.na(match(List[, 3], classB[, 1])) == TRUE)
    yesBCorr  <- which(!is.na(match(List[, 3], corrAB[, 2])) == TRUE)
    noBCorr   <- which(is.na(match(List[, 3], corrAB[, 2])) == TRUE)
    List$NoMatchFromB[intersect(yesBClass, yesBCorr)] <- 0
    List$NoMatchFromB[intersect(yesBClass, noBCorr)]  <- 1
    
    # --- NoMatchTo flags -----------------------------------------------------
    noA <- which(List[, 1] == ""); yesA <- which(List[, 1] != "")
    noAstar <- which(List[, 2] == ""); yesAstar <- which(List[, 2] != "")
    noB <- which(List[, 3] == ""); yesB <- which(List[, 3] != "")
    
    List$NoMatchToAStar <- 1
    List$NoMatchToAStar[intersect(intersect(noA, noB), yesAstar)] <- ""
    List$NoMatchToAStar[intersect(intersect(yesA, yesAstar), noB)] <- 0
    List$NoMatchToAStar[intersect(intersect(yesA, yesAstar), yesB)] <- 0
    
    List$NoMatchToB <- 1
    List$NoMatchToB[intersect(intersect(noA, noAstar), yesB)] <- ""
    List$NoMatchToB[intersect(intersect(yesA, yesB), noAstar)] <- 0
    List$NoMatchToB[intersect(intersect(yesA, yesAstar), yesB)] <- 0
    
    # --- Final Review/Redundancy flags consistency --------------------------
    if (Reference %in% c("A", "B")) {
      List$Review[which(List[, 2] == "")] <- ""
      List$Review[which(List[, 3] == "")] <- ""
    }
    
    List$Redundancy <- 0
    f1 <- stats::aggregate(List[, 2:3], by = List[, 2:3], length)[1:(ncol(List[, 2:3]) + 1)][
      which(stats::aggregate(List[, 2:3], by = List[, 2:3], length)[1:(ncol(List[, 2:3]) + 1)][, 3] >= 2), 1:2
    ]
    List$Redundancy[which(apply(List[, 2:3], 1, paste, collapse = " ") %in% apply(f1, 1, paste, collapse = " "))] <- 1
    List$Redundancy[intersect(which(List[, 2] == ""), which(List[, 3] == ""))] <- ""
    
  }, error = function(e) {
    stop(simpleError("An error has occurred and execution needs to stop. Please check the input data."))
  })
  
  # --- Mismatch tolerance checks --------------------------------------------
  if (length(which(List[, idx.thres] == 1)) / length(which(List[, idx.thres] != "")) > MismatchToleranceAStar) {
    StopAAStar <- 1
  } else StopAAStar <- 0
  
  if (length(which(List[, idx.thres + 1] == 1)) / length(which(List[, idx.thres + 1] != "")) > MismatchToleranceB) {
    StopAB <- 1
  } else StopAB <- 0
  
  if (StopAAStar == 1 && StopAB == 0) {
    stop("Too many missing values for A* in the updated correspondence table (NoMatchToAStar).")
  } else if (StopAAStar == 0 && StopAB == 1) {
    stop("Too many missing values for B in the updated correspondence table (NoMatchToB).")
  } else if (StopAAStar == 1 && StopAB == 1) {
    stop("Too many missing values for both A* and B in the updated correspondence table.")
  } else {
    # --- Enrich with label/supp columns --------------------------------------
    tryCatch({
      if (ncol(classA) >= 2) {
        A1 <- as.matrix(classA[match(List[, 1], unlist(classA[, 1])), 2:ncol(classA)])
        A1[is.na(A1)] <- ""
        colnames(A1) <- paste(colnames(classA)[1], colnames(classA)[2:ncol(classA)], sep = "_")
        List <- cbind(List, A1)
      }
      if (ncol(classAStar) >= 2) {
        A2 <- as.matrix(classAStar[match(List[, 2], unlist(classAStar[, 1])), 2:ncol(classAStar)])
        A2[is.na(A2)] <- ""
        colnames(A2) <- paste(colnames(classAStar)[1], colnames(classAStar)[2:ncol(classAStar)], sep = "_")
        List <- cbind(List, A2)
      }
      if (ncol(classB) >= 2) {
        B1 <- as.matrix(classB[match(List[, 3], unlist(classB[, 1])), 2:ncol(classB)])
        B1[is.na(B1)] <- ""
        colnames(B1) <- paste(colnames(classB)[1], colnames(classB)[2:ncol(classB)], sep = "_")
        List <- cbind(List, B1)
      }
      if (ncol(corrAAStar) >= 3) {
        AA1 <- as.matrix(corrAAStar[match(data.frame(t(List[, c(1, 2)])), data.frame(t(corrAAStar[, 1:2]))), 3:ncol(corrAAStar)])
        AA1[is.na(AA1)] <- ""
        colnames(AA1) <- paste(paste(colnames(corrAAStar)[1], colnames(corrAAStar)[2], sep = " - "), colnames(corrAAStar)[3:ncol(corrAAStar)], sep = "_")
        List <- cbind(List, AA1)
      }
      if (ncol(corrAB) >= 3) {
        AB1 <- as.matrix(corrAB[match(data.frame(t(List[, c(1, 3)])), data.frame(t(corrAB[, 1:2]))), 3:ncol(corrAB)])
        AB1[is.na(AB1)] <- ""
        colnames(AB1) <- paste(paste(colnames(corrAB)[1], colnames(corrAB)[2], sep = " - "), colnames(corrAB)[3:ncol(corrAB)], sep = "_")
        List <- cbind(List, AB1)
      }
    }, error = function(e) {
      stop(simpleError("An error has occurred and execution needs to stop. Please check the input data."))
    })
  }
  
  # --- LabelChange -----------------------------------------------------------
  tryCatch({
    if (ncol(classA) >= 2 && ncol(classAStar) >= 2) {
      LabelChange <- rep(1, nrow(List))
      LabelChange[which(
        tolower(gsub("[[:punct:] ]+", " ", List[, idx.thres + 4])) ==
          tolower(gsub("[[:punct:] ]+", " ", List[, idx.thres + 4 + ncol(classA) - 1]))
      )] <- 0
      LabelChange[which(List[, idx.thres + 4] == "")] <- ""
      LabelChange[which(List[, idx.thres + 4 + ncol(classA) - 1] == "")] <- ""
      List <- cbind(List[, 1:(idx.thres + 3)], LabelChange, List[, (idx.thres + 4):ncol(List)])
    }
  }, error = function(e) {
    stop(simpleError("An error has occurred when computing LabelChange."))
  })
  
  # --- Redundancy trimming / keeping ----------------------------------------
  if (isTRUE(Redundancy_trim)) {
    col_multiple <- c(grep(colnames(List)[1], colnames(List), value = TRUE),
                      grep("LabelChange", colnames(List), value = TRUE))
    
    uniqueAstarB <- unique(List[which(List$Redundancy == 1), c(2, 3)])
    uniqueAstarB$id_to_use <- seq_len(nrow(uniqueAstarB))
    
    List <- merge(List, uniqueAstarB, by = colnames(List)[c(2, 3)], sort = FALSE, all.x = TRUE)[, union(names(List), names(uniqueAstarB))]
    col_link <- grep("id_to_use", colnames(List), value = TRUE)
    
    x_temp <- split(List[which(List$Redundancy == 1), col_multiple], List[which(List$Redundancy == 1), col_link])
    
    for (i in seq_len(nrow(uniqueAstarB))) {
      multiple_values <- apply(x_temp[[i]], 2, function(x) length(unique(x)))
      x_change <- which(multiple_values != 1)
      col_multiple_temp <- c(col_multiple)[x_change]
      List[which(List$Redundancy == 1 & List[, col_link] == i), col_multiple_temp] <- "Multiple"
    }
    
    List <- List[, !names(List) %in% "id_to_use"]
    
    dup <- as.numeric(duplicated(List[, 1:3]))
    List <- List[which(dup == 0), ]
    
    List[which(List$LabelChange == "Multiple"), grep("LabelChange", colnames(List), value = TRUE)] <- 1
    
  } else {
    dup <- as.numeric(duplicated(List[, c(2, 3)]))
    red_col <- which(colnames(List) == "Redundancy")
    List$Redundancy_keep <- rep(0, nrow(List))
    List$Redundancy_keep[which(dup == "0" & List$Redundancy == "1")] <- 1
    List <- List[, c(1:red_col, ncol(List), (red_col + 1):(ncol(List) - 1))]
  }
  
  # --- Classification names data frame --------------------------------------
  CsvNames <- data.frame(matrix(0, 3, 1))
  CsvNames[1, 1] <- paste("A:",     colnames(List)[1])
  CsvNames[2, 1] <- paste("B:",     colnames(List)[3])
  CsvNames[3, 1] <- paste("AStar:", colnames(List)[2])
  CsvNames <- data.frame(CsvNames)
  
  # --- Normalize spaces in Final (kept for parity) ---------------------------
  Final <- apply(List, 2, function(x) { gsub(" ", " ", x) })
  if (is.null(dim(Final))) {
    Final <- t(data.frame(Final)); rownames(Final) <- 1
  }
  
  # --- Return ---------------------------------------------------------------
  FinalResults <- list()
  FinalResults[[1]] <- data.frame(Final, check.names = FALSE, row.names = NULL)
  
  cols_to_convert <- c(
    "CodeChange", "Redundancy", "Redundancy_keep",
    "NoMatchToAStar", "NoMatchToB",
    "NoMatchFromAStar", "NoMatchFromB",
    "LabelChange", "Review"
  )
  
  for (col in cols_to_convert) {
    if (col %in% names(FinalResults[[1]])) {
      FinalResults[[1]][[col]] <- suppressWarnings(as.numeric(FinalResults[[1]][[col]]))
    }
  }
  FinalResults[[2]] <- CsvNames
  names(FinalResults) <- c("updateCorrespondenceTable", "classificationNames")
  FinalResults
}