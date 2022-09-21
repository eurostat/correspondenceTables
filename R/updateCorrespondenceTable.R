updateCorrespondenceTable <- function(A, B, AStar, AB, AAStar, CSVout = NULL, Reference = "none",
    MismatchToleranceB = 0.2, MismatchToleranceAStar = 0.2) {

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
            answer <- menu(c("Yes", "No"), title = "Do you want to overwrite it?")
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

    classA <- read.csv(A, sep = ",", header = TRUE, check.names = FALSE, colClasses = c("character"),
        encoding = "UTF-8")
    colnames(classA) <- removeBOM(colnames(classA))

    classAStar <- read.csv(AStar, sep = ",", header = TRUE, check.names = FALSE,
        colClasses = c("character"), encoding = "UTF-8")
    colnames(classAStar) <- removeBOM(colnames(classAStar))

    classB <- read.csv(B, sep = ",", header = TRUE, check.names = FALSE, colClasses = c("character"),
        encoding = "UTF-8")
    colnames(classB) <- removeBOM(colnames(classB))

    # The following code lines read the correspondence tables AAStar and AB.

    corrAAStar <- read.csv(AAStar, sep = ",", header = TRUE, check.names = FALSE,
        colClasses = c("character"), encoding = "UTF-8")
    colnames(corrAAStar) <- removeBOM(colnames(corrAAStar))

    corrAB <- read.csv(AB, sep = ",", header = TRUE, check.names = FALSE, colClasses = c("character"),
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
        cat(paste("WARNING: there is no code of ", colnames(classA)[1], " that appears in both ",
            test.names[1], " and ", test.names[5], ". When the execution of the function is over, please check the files to ensure that this is not the result of a mistake in their preparation or declaration.",
            sep = ""))
    }

    if (sum(!is.na(match(classA[, 1], corrAB[, 1]))) == 0) {
        cat(paste("WARNING: there is no code of ", colnames(classA)[1], " that appears in both ",
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
            f <- aggregate(unique(F1[, 2:3])[, idx], list(num = unique(F1[, 2:3])[,
                idx]), length)[which(aggregate(unique(F1[, 2:3])[, idx], list(num = unique(F1[,
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

            f1 <- aggregate(F1[, 2:3], by = F1[, 2:3], length)[1:(ncol(F1[, 2:3]) +
                1)][which(aggregate(F1[, 2:3], by = F1[, 2:3], length)[1:(ncol(F1[,
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
            f1 <- aggregate(F1[, 2:3], by = F1[, 2:3], length)[1:(ncol(F1[, 2:3]) +
                1)][which(aggregate(F1[, 2:3], by = F1[, 2:3], length)[1:(ncol(F1[,
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
        f1 <- aggregate(List[, 2:3], by = List[, 2:3], length)[1:(ncol(List[, 2:3]) +
            1)][which(aggregate(List[, 2:3], by = List[, 2:3], length)[1:(ncol(List[,
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

        # A data frame that contains the names of the classifications A, AStar,
        # and B is constructed.

        CsvNames <- data.frame(matrix(0, 3, 1))

        CsvNames[1, 1] <- paste("A:", colnames(List)[1], sep = " ")

        CsvNames[2, 1] <- paste("B:", colnames(List)[3], sep = " ")

        CsvNames[3, 1] <- paste("AStar:", colnames(List)[2], sep = " ")

        CsvNames <- data.frame(CsvNames)

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
            readr::write_excel_csv(data.frame(Final, check.names = FALSE), file = CSVout,
                col_names = TRUE)
            write.csv(CsvNames, file = paste0(Name1, "classificationNames_", Name2),
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
