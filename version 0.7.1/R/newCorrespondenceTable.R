newCorrespondenceTable <- function(Tables, CSVout = NULL, Reference = "none", MismatchTolerance = 0.2) {

    # Check if the file that contains the names of both classifications and
    # correspondence tables exists in working directory
    if (!file.exists(Tables)) {
        stop(simpleError(paste("There is no file with name", Tables, "in your working directory.")))
    } else {
        x <- as.matrix(read.csv(Tables, sep = ",", header = FALSE, colClasses = c("character"),
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
        read.csv(x, sep = ",", check.names = FALSE, colClasses = c("character"),
            encoding = "UTF-8")
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
    R <- RR[tail(c(1:length(RR)), (length(RR) - 1)/2)]

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
        if (sum(!is.na(match(RRR[[1]][, 1], R[[1]][, 1]))) == 0) {
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
        if (sum(!is.na(match(R[[length(R)]][, 1], RRR[[nrow(x)]][, 1]))) == 0) {
            stop(simpleError(paste("There is no code of ", colnames(RRR[[length(R) +
                nrow(x)]])[1], " that appears in both ", inputs[nrow(x)], " and ",
                inputs[length(R) + nrow(x)], ". This is an error. The files should have at least one code of ",
                colnames(RRR[[length(R) + nrow(x)]])[1], " in common to allow the generation of the candidate correspondence table.",
                sep = "")))
        }

    }

    if (k >= 2) {

        # A in A appears in A:C1
        if (sum(!is.na(match(RRR[[1]][, 1], R[[1]][, 1]))) == 0) {
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
        if (sum(!is.na(match(R[[length(R)]][, 1], RRR[[nrow(x)]][, 1]))) == 0) {
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
        if (sum(!is.na(match(RRR[[2]][, 1], R[[1]][, 2]))) == 0) {
            cat(paste("WARNING: there is no code of ", colnames(RRR[[2]])[1], " that appears in both ",
                inputs[2], " and ", inputs[1 + nrow(x)], ". When the execution of the function is over, please check the files to ensure that this is not the result of a mistake in their preparation or declaration.\n",
                sep = ""))
        }

        # C1 in C1 appears in B:C1
        if (sum(!is.na(match(RRR[[2]][, 1], R[[2]][, 2]))) == 0) {
            cat(paste("WARNING: there is no code of ", colnames(RRR[[2]])[1], " that appears in both ",
                inputs[2], " and ", inputs[2 + nrow(x)], ". When the execution of the function is over, please check the files to ensure that this is not the result of a mistake in their preparation or declaration.\n",
                sep = ""))
        }

    }

    if (k == 2) {

        for (i in 2:k) {

            # C1 in C1 appears in A:C1 C2 in C2 appears in C1:C2 C3 in C3
            # appears in C2:C3
            if (sum(!is.na(match(RRR[[i]][, 1], R[[i - 1]][, 2]))) == 0) {
                cat(paste("WARNING: there is no code of ", colnames(RRR[[i]])[1],
                  " that appears in both ", inputs[i], " and ", inputs[i - 1 + nrow(x)],
                  ". When the execution of the function is over, please check the files to ensure that this is not the result of a mistake in their preparation or declaration.\n",
                  sep = ""))
            }

            # C1 in C1 appears in C1:C2 C2 in C2 appears in C2:C3 C3 in C3
            # appears in C3:C4
            if (sum(!is.na(match(RRR[[i]][, 1], R[[i]][, 1]))) == 0) {
                cat(paste("WARNING: there is no code of ", colnames(RRR[[i]])[1],
                  " that appears in both ", inputs[i], " and ", inputs[i + nrow(x)],
                  ". When the execution of the function is over, please check the files to ensure that this is not the result of a mistake in their preparation or declaration.\n",
                  sep = ""))
            }
        }

        # Ck in Ck appears in C(k-1):Ck
        if (sum(!is.na(match(RRR[[k + 1]][, 1], R[[k]][, 2]))) == 0) {
            cat(paste("WARNING: there is no code of ", colnames(RRR[[k + 1]])[1],
                " that appears in both ", inputs[k + 1], " and ", inputs[k + nrow(x)],
                ". When the execution of the function is over, please check the files to ensure that this is not the result of a mistake in their preparation or declaration.\n",
                sep = ""))
        }

        # Ck in Ck appears in B:Ck
        if (sum(!is.na(match(RRR[[k + 1]][, 1], R[[k + 1]][, 2]))) == 0) {
            cat(paste("WARNING: there is no code of ", colnames(RRR[[k + 1]])[1],
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

            # The following for loop creates the desirable correspondence
            # table.  The operations are conducted for each unique element of
            # classification A of the correspondence table A:C1.
            for (i in unique(R[[1]][, 1])) {

                # Print the percentage of codes that have been processed.
                counter <- counter + 1
                cat("\r", "Percentage of codes of", colnames(RRR[[1]][1]), "processed:",
                  paste(round(counter/length(unique(R[[1]][, 1])) * 100, digits = 0),
                    "%", sep = ""), sep = " ")
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

            # The following for loop creates the desirable correspondence
            # table.  The operations are conducted for each unique element of
            # classification A of the correspondence table A:C1.
            for (i in unique(R[[1]][, 1])) {
                counter <- counter + 1
                cat("\r", "Percentage of codes of", colnames(RRR[[1]][1]), "processed:",
                  paste(round(counter/length(unique(R[[1]][, 1])) * 100, digits = 0),
                    "%", sep = ""), sep = " ")

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

            # The following for loop creates the desirable correspondence
            # table.  The operations are conducted for each unique element of
            # classification A of the correspondence table A:C1.
            for (i in unique(R[[1]][, 1])) {

                counter <- counter + 1
                cat("\r", "Percentage of codes of", colnames(RRR[[1]][1]), "processed:",
                  paste(round(counter/length(unique(R[[1]][, 1])) * 100, digits = 0),
                    "%", sep = ""), sep = " ")

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
        RRR_BtoA <- RRR[c(rev(1:(k + 2)), rev(tail(c(1:length(RRR)), (length(RRR) -
            1)/2)))]
        if (length(rev(tail(c(1:length(RR)), (length(RR) - 1)/2))) >= 3) {
            for (rev in (k + 4):(length(RRR_BtoA) - 1)) {
                column_2 <- RRR_BtoA[[rev]][, 2]
                RRR_BtoA[[rev]][, 2] <- RRR_BtoA[[rev]][, 1]
                RRR_BtoA[[rev]][, 1] <- column_2
            }
        }

        RR <- lapply(RRR_BtoA, function(x) {
            matrix(unlist(x), ncol = ncol(x))
        })

        R <- RR[tail(c(1:length(RR)), (length(RR) - 1)/2)]

        F_BtoA <- list()

        counter <- 0
        cat("\n")
        if (length(R) == 2) {

            for (i in unique(R[[1]][, 1])) {

                counter <- counter + 1
                cat("\r", "Percentage of codes of", colnames(RRR_BtoA[[1]][1]), "processed:",
                  paste(round(counter/length(unique(R[[1]][, 1])) * 100, digits = 0),
                    "%", sep = ""), sep = " ")

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

            for (i in unique(R[[1]][, 1])) {

                counter <- counter + 1
                cat("\r", "Percentage of codes of", colnames(RRR_BtoA[[1]][1]), "processed:",
                  paste(round(counter/length(unique(R[[1]][, 1])) * 100, digits = 0),
                    "%", sep = ""), sep = " ")

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

            for (i in unique(R[[1]][, 1])) {

                counter <- counter + 1
                cat("\r", "Percentage of codes of", colnames(RRR_BtoA[[1]][1]), "processed:",
                  paste(round(counter/length(unique(R[[1]][, 1])) * 100, digits = 0),
                    "%", sep = ""), sep = " ")

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
                f <- aggregate(matrix(unique(F1[, c(1, ncol(F1))]), ncol = 2)[, 2],
                  list(num = matrix(unique(F1[, c(1, ncol(F1))]), ncol = 2)[, 2]),
                  length)[which(aggregate(matrix(unique(F1[, c(1, ncol(F1))]), ncol = 2)[,
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
                f1 <- aggregate(F1[, c(1, ncol(F1) - 1)], by = F1[, c(1, ncol(F1) -
                  1)], length)[1:(ncol(F1[, c(1, ncol(F1) - 1)]) + 1)][which(aggregate(F1[,
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
                f <- aggregate(matrix(unique(F1[, c(1, ncol(F1))]), ncol = 2)[, 1],
                  list(num = matrix(unique(F1[, c(1, ncol(F1))]), ncol = 2)[, 1]),
                  length)[which(aggregate(matrix(unique(F1[, c(1, ncol(F1))]), ncol = 2)[,
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
                f1 <- aggregate(F1[, c(1, ncol(F1) - 1)], by = F1[, c(1, ncol(F1) -
                  1)], length)[1:(ncol(F1[, c(1, ncol(F1) - 1)]) + 1)][which(aggregate(F1[,
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
                f1 <- aggregate(F1[, c(1, ncol(F1))], by = F1[, c(1, ncol(F1))],
                  length)[1:(ncol(F1[, c(1, ncol(F1))]) + 1)][which(aggregate(F1[,
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

        inA <- which(is.na(match(RRR[[1]][, 1], correspondenceAB[, 1])) == TRUE)
        if (length(inA) >= 1) {
            InA <- cbind(matrix(RRR[[1]][inA, 1], length(inA), 1), matrix("", length(inA),
                idx - 1))
            InA <- cbind(InA, matrix("", length(inA), 2))
            InA <- data.frame(InA)
            colnames(InA) <- colnames(correspondenceAB)
            correspondenceAB <- rbind(correspondenceAB, InA)
        }

        inB <- which(is.na(match(RRR[[nrow(x)]][, 1], correspondenceAB[, k + 2])) ==
            TRUE)
        if (length(inB) >= 1) {
            InB <- cbind(matrix("", length(inB), k + 1), matrix(RRR[[nrow(x)]][inB,
                1], length(inB), 1), matrix("", length(inB), idx - k - 2))
            InB <- cbind(InB, matrix("", length(inB), 2))
            InB <- data.frame(InB)
            colnames(InB) <- colnames(correspondenceAB)
            correspondenceAB <- rbind(correspondenceAB, InB)
        }

        yesA <- which(!is.na(match(correspondenceAB[, 1], RRR[[1]][, 1])) == TRUE)
        yesAC1 <- which(!is.na(match(correspondenceAB[, 1], RRR[[nrow(x) + 1]][,
            1])) == TRUE)
        noAC1 <- which(is.na(match(correspondenceAB[, 1], RRR[[nrow(x) + 1]][, 1])) ==
            TRUE)

        correspondenceAB$NoMatchFromA[intersect(yesA, yesAC1)] <- 0
        correspondenceAB$NoMatchFromA[intersect(yesA, noAC1)] <- 1

        yesB <- which(!is.na(match(correspondenceAB[, k + 2], RRR[[nrow(x)]][, 1])) ==
            TRUE)
        yesBCk <- which(!is.na(match(correspondenceAB[, k + 2], RRR[[length(RRR)]][,
            1])) == TRUE)
        noBCk <- which(is.na(match(correspondenceAB[, k + 2], RRR[[length(RRR)]][,
            1])) == TRUE)

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
        f1 <- aggregate(correspondenceAB[, c(1, k + 2)], by = correspondenceAB[,
            c(1, k + 2)], length)[1:(ncol(correspondenceAB[, c(1, k + 2)]) + 1)][which(aggregate(correspondenceAB[,
            c(1, k + 2)], by = correspondenceAB[, c(1, k + 2)], length)[1:(ncol(correspondenceAB[,
            c(1, k + 2)]) + 1)][, 3] >= 2), 1:2]
        correspondenceAB$Redundancy[which(apply(correspondenceAB[, c(1, k + 2)],
            1, paste, collapse = " ") %in% apply(f1, 1, paste, collapse = " "))] <- 1

    }, error = function(e) {
        stop(simpleError("An error has occurred and execution needs to stop. Please check the input data."))
    })

    cat("\n")

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
            Tail <- tail(c(1:length(RRR)), (length(RRR) - 1)/2)

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
            stop(simpleError("An error has occurred and execution needs to stop. Please check the input data."))
        })

    } else {
        # Error message in case the percentage of unmatched codes between A and
        # B is larger than the desired threshold.
        stop("Too many codes in either of classifications A and B cannot be mapped to any code in the other one.")
    }

    tryCatch({
        # The final correspondence table A:B is sorted, firstly, based on
        # classification A, and then, based on classification B.
        correspondenceAB <- correspondenceAB[order(correspondenceAB[, 1], correspondenceAB[,
            (length(RRR) + 1)/2], decreasing = FALSE), ]

        # Create a data frame that contains the names of the classifications.
        CsvNames <- data.frame(matrix(0, k + 2, 1))

        CsvNames[1, 1] <- paste("A:", colnames(correspondenceAB)[1], sep = " ")

        CsvNames[k + 2, 1] <- paste("B:", colnames(correspondenceAB)[k + 2], sep = " ")

        for (i3 in seq(k) + 1) {
            CsvNames[i3, 1] <- paste(paste("C", i3 - 1, ":", sep = ""), colnames(correspondenceAB)[i3],
                sep = " ")
        }

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
        stop(simpleError("An error has occurred and execution needs to stop. Please check the input data."))
    })

    # Check so as to write (or not) the final correspondence table (final
    # desired table) as well as the names of classifications in two seperate
    # csv files.
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
        stop(simpleError("An error has occurred and execution needs to stop. Please check the input data."))
    })

}

