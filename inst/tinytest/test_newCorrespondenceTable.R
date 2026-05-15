
read_test_csv <- function(fname) {
  utils::read.csv(
    system.file("extdata/test", fname, package = "correspondenceTables"),
    sep = ",", header = TRUE, check.names = FALSE,
    colClasses = c("character"), encoding = "UTF-8",
    stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM"
  )
}

# Helper: materialize names.csv by expanding fixture file names to absolute paths
# Writes to tempdir() to avoid modifying the installed package directory.
materialize_names_csv <- function(src_csv_name, out_csv_name = "names.csv") {
  templ <- system.file("extdata/test", src_csv_name, package = "correspondenceTables")
  
  A <- read.csv(templ, header = FALSE, sep = ",", stringsAsFactors = FALSE)
  for (i in seq_len(nrow(A))) {
    for (j in seq_len(ncol(A))) {
      if (!is.na(A[i, j]) && A[i, j] != "") {
        A[i, j] <- system.file("extdata/test", A[i, j], package = "correspondenceTables")
      }
    }
  }
  out <- file.path(tempdir(), out_csv_name)
  write.table(A, file = out, row.names = FALSE, col.names = FALSE, sep = ",")
  invisible(out)
}

cleanup_names_csv <- function() {
  p <- file.path(tempdir(), "names.csv")
  if (file.exists(p)) file.remove(p)
}

############################################################
# TEST N1 — Basic pipeline, no trimming
############################################################
{
  on.exit(cleanup_names_csv(), add = TRUE)
  
  names_path <- materialize_names_csv("names12.csv", "names.csv")
  
  TEST <- newCorrespondenceTable(
    Tables = names_path,
    Reference = "none",
    MismatchTolerance = 0.96,
    Redundancy_trim = FALSE,
    Progress = FALSE
  ) 
  TEST_trim <- newCorrespondenceTable(
    Tables = names_path,
    Reference = "none",
    MismatchTolerance = 0.96,
    Redundancy_trim = TRUE,
    Progress = FALSE
  ) 
  
  
  
  ABC      <- read_test_csv("ABC.csv")
  ABC_Trim <- read_test_csv("ABC_trim.csv")
  
  expect_equal(TEST[[1]][1:5],      ABC)
  expect_equal(TEST_trim[[1]][1:4], ABC_Trim)
}

############################################################
# TEST N2 — Legend merge check on “new” fixtures
############################################################
{
  on.exit(cleanup_names_csv(), add = TRUE)
  
  names_path <- materialize_names_csv("names2.csv", "names.csv")
  
  TEST2 <- newCorrespondenceTable(
    Tables = names_path,
    Reference = "none",
    MismatchTolerance = 0.96,
    Redundancy_trim = FALSE,
    Progress = FALSE
  )
  
  
  TEST2[[1]]$sort <- seq_len(nrow(TEST2[[1]]))
  TEST2_T <- read_test_csv("nomatch_test_new.csv")
  TEST2_T$sort <- seq_len(nrow(TEST2_T))
  
  legend <- read_test_csv("legend_nomatch_new.csv")
  
  x <- as.data.frame(TEST2[[1]])
  y <- as.data.frame(legend)
  
  merged <- merge(x, y, by = c("Unmatched","NoMatchFromA","NoMatchFromB"), all.x = TRUE)
  merged  <- merged[order(as.numeric(merged$sort)), ]
  TEST2_T <- TEST2_T[order(as.numeric(TEST2_T$sort)), ]
  
  ## Preserve original index references
  expect_equal(merged[, 15], TEST2_T[, 13])
}

