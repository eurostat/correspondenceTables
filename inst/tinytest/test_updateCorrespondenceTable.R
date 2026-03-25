
read_extdata_csv <- function(fname) {
  utils::read.csv(
    system.file("extdata/test", fname, package = "correspondenceTables"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

############################################################
# TEST 1 — Redundancy trimming scenarios
############################################################

{
  A_df      <- read_extdata_csv("NACE2_Red.csv")
  AStar_df  <- read_extdata_csv("NACE21_Red.csv")
  B_df      <- read_extdata_csv("CPA_Red.csv")
  AB_df     <- read_extdata_csv("NACECPA_Red.csv")
  AAStar_df <- read_extdata_csv("NACE221_Red.csv")
  
  CT_nullT <- updateCorrespondenceTable(
    A = A_df, B = B_df, AStar = AStar_df, AB = AB_df, AAStar = AAStar_df,
    Reference = "none",
    MismatchToleranceB = 0.4, MismatchToleranceAStar = 0.4,
    Redundancy_trim = TRUE
  )
  CT_nullS <- updateCorrespondenceTable(
    A = A_df, B = B_df, AStar = AStar_df, AB = AB_df, AAStar = AAStar_df,
    Reference = "none",
    MismatchToleranceB = 0.4, MismatchToleranceAStar = 0.4,
    Redundancy_trim = FALSE
  )
  
  CT_AT <- updateCorrespondenceTable(
    A = A_df, B = B_df, AStar = AStar_df, AB = AB_df, AAStar = AAStar_df,
    Reference = "A",
    MismatchToleranceB = 0.4, MismatchToleranceAStar = 0.4,
    Redundancy_trim = TRUE
  )
  CT_AS <- updateCorrespondenceTable(
    A = A_df, B = B_df, AStar = AStar_df, AB = AB_df, AAStar = AAStar_df,
    Reference = "A",
    MismatchToleranceB = 0.4, MismatchToleranceAStar = 0.4,
    Redundancy_trim = FALSE
  )
  
  CT_BT <- updateCorrespondenceTable(
    A = A_df, B = B_df, AStar = AStar_df, AB = AB_df, AAStar = AAStar_df,
    Reference = "B",
    MismatchToleranceB = 0.4, MismatchToleranceAStar = 0.4,
    Redundancy_trim = TRUE
  )
  CT_BS <- updateCorrespondenceTable(
    A = A_df, B = B_df, AStar = AStar_df, AB = AB_df, AAStar = AAStar_df,
    Reference = "B",
    MismatchToleranceB = 0.4, MismatchToleranceAStar = 0.4,
    Redundancy_trim = FALSE
  )
  
  Ctext_nullT <- read_extdata_csv("LblYY_RednullTrim.csv")
  Ctext_nullS <- read_extdata_csv("LblYY_RednullShow.csv")
  
  Ctext_AT <- read_extdata_csv("LblYY_RedATrim.csv")
  Ctext_AS <- read_extdata_csv("LblYY_RedAShow.csv")
  
  Ctext_BT <- read_extdata_csv("LblYY_RedBTrim.csv")
  Ctext_BS <- read_extdata_csv("LblYY_RedBShow.csv")
  
  #transformation needed just for the test
  CT_nullT$updateCorrespondenceTable$NACE21code = as.numeric(CT_nullT$updateCorrespondenceTable$NACE21code)
  CT_nullT$updateCorrespondenceTable$CPAcode = as.numeric(CT_nullT$updateCorrespondenceTable$CPAcode)
  expect_equal(CT_nullT[[1]], Ctext_nullT)
  
  CT_AT$updateCorrespondenceTable$NACE21code = as.numeric(CT_AT$updateCorrespondenceTable$NACE21code)
  CT_AT$updateCorrespondenceTable$CPAcode = as.numeric(CT_AT$updateCorrespondenceTable$CPAcode)
  expect_equal(CT_AT[[1]],   Ctext_AT)
  
  CT_BT$updateCorrespondenceTable$NACE21code = as.numeric(CT_BT$updateCorrespondenceTable$NACE21code)
  CT_BT$updateCorrespondenceTable$CPAcode = as.numeric(CT_BT$updateCorrespondenceTable$CPAcode)
  expect_equal(CT_BT[[1]],   Ctext_BT)
  
  CT_nullS$updateCorrespondenceTable$NACE21code = as.numeric(CT_nullS$updateCorrespondenceTable$NACE21code)
  CT_nullS$updateCorrespondenceTable$NACE2code = as.numeric(CT_nullS$updateCorrespondenceTable$NACE2code)
  CT_nullS$updateCorrespondenceTable$CPAcode = as.numeric(CT_nullS$updateCorrespondenceTable$CPAcode)  
  expect_equal(CT_nullS[[1]], Ctext_nullS)
  
  CT_AS$updateCorrespondenceTable$NACE21code = as.numeric(CT_AS$updateCorrespondenceTable$NACE21code)
  CT_AS$updateCorrespondenceTable$NACE2code = as.numeric(CT_AS$updateCorrespondenceTable$NACE2code)
  CT_AS$updateCorrespondenceTable$CPAcode = as.numeric(CT_AS$updateCorrespondenceTable$CPAcode)  
  expect_equal(CT_AS[[1]],    Ctext_AS)
  
  CT_BS$updateCorrespondenceTable$NACE21code = as.numeric(CT_BS$updateCorrespondenceTable$NACE21code)
  CT_BS$updateCorrespondenceTable$NACE2code = as.numeric(CT_BS$updateCorrespondenceTable$NACE2code)
  CT_BS$updateCorrespondenceTable$CPAcode = as.numeric(CT_BS$updateCorrespondenceTable$CPAcode)    
  expect_equal(CT_BS[[1]],    Ctext_BS)
}


############################################################
# TEST 2 — Baseline (Reference = "A"), no trimming
############################################################
{
  A_df = read.csv(system.file("extdata/test", "NACE2.csv", package = "correspondenceTables"))
  AStar_df = read.csv(system.file("extdata/test", "NACE21.csv", package = "correspondenceTables"))
  B_df = read.csv(system.file("extdata/test", "CPA.csv", package = "correspondenceTables"))
  AB_df = read.csv(system.file("extdata/test", "NACECPA.csv", package = "correspondenceTables"))
  AAStar_df = read.csv(system.file("extdata/test", "NACE221.csv", package = "correspondenceTables"))
  
  CT_nullT <- updateCorrespondenceTable(
    A = A_df, B = B_df, AStar = AStar_df, AB = AB_df, AAStar = AAStar_df,
    Reference = "A",
    MismatchToleranceB = 0.4,
    MismatchToleranceAStar = 0.4,
    Redundancy_trim = FALSE
  )
  
  Ctext_nullT <- read_extdata_csv("LblYY.csv")
  
  CT_nullT$updateCorrespondenceTable$NACE21code = as.numeric(CT_nullT$updateCorrespondenceTable$NACE21code)
  CT_nullT$updateCorrespondenceTable$NACE2code = as.numeric(CT_nullT$updateCorrespondenceTable$NACE2code)
  CT_nullT$updateCorrespondenceTable$CPAcode = as.numeric(CT_nullT$updateCorrespondenceTable$CPAcode)    
  expect_equal(CT_nullT[[1]], Ctext_nullT)
}


############################################################
# TEST 3 — “no” variants; compare first 10 columns
############################################################
{
  Ano_df = read.csv(system.file("extdata/test", "NACE2no.csv", package = "correspondenceTables"))
  AStarno_df = read.csv(system.file("extdata/test", "NACE21no.csv", package = "correspondenceTables"))
  Bno_df = read.csv(system.file("extdata/test", "CPA.csv", package = "correspondenceTables"))
  ABno_df = read.csv(system.file("extdata/test", "NACECPA.csv", package = "correspondenceTables"))
  AAStarno_df = read.csv(system.file("extdata/test", "NACE221.csv", package = "correspondenceTables"))
  
  CT_nullTno <- updateCorrespondenceTable(
    A = Ano_df, B = Bno_df, AStar = AStarno_df, AB = ABno_df, AAStar = AAStarno_df,
    Reference = "A",
    MismatchToleranceB = 0.4,
    MismatchToleranceAStar = 0.4,
    Redundancy_trim = FALSE
  )
  
  Ctext_nullTno <- read_extdata_csv("LblNY.csv")
  
  CT_nullTno$updateCorrespondenceTable$NACE21code = as.numeric(CT_nullTno$updateCorrespondenceTable$NACE21code)
  CT_nullTno$updateCorrespondenceTable$NACE2code = as.numeric(CT_nullTno$updateCorrespondenceTable$NACE2code)
  CT_nullTno$updateCorrespondenceTable$CPAcode = as.numeric(CT_nullTno$updateCorrespondenceTable$CPAcode)    
  expect_equal(CT_nullTno[[1]][, 1:10], Ctext_nullTno[, 1:10])
}

############################################################
# TEST 4 — Upper-case fixtures; legend merge check
############################################################
{
  A_df      <- read_extdata_csv("UcaseA.csv")
  AStar_df  <- read_extdata_csv("UcaseAstar.csv")
  B_df      <- read_extdata_csv("UcaseB.csv")
  AB_df     <- read_extdata_csv("UcaseAB.csv")
  AAStar_df <- read_extdata_csv("UcaseAAstar.csv")
  
  UCT <- updateCorrespondenceTable(
    A = A_df, B = B_df, AStar = AStar_df, AB = AB_df, AAStar = AAStar_df,
    Reference = "B",
    MismatchToleranceB = 0.8,
    MismatchToleranceAStar = 0.8,
    Redundancy_trim = FALSE
  )
  UCT[[1]]$sort <- seq_len(nrow(UCT[[1]]))
  
  UTC_T <- read_extdata_csv("nomatch_test.csv")
  UTC_T$sort <- seq_len(nrow(UTC_T))
  
  legend <- read_extdata_csv("legend_nomatch.csv")
  colnames(legend)[2:5] <- c("NoMatchToAStar","NoMatchToB","NoMatchFromAStar","NoMatchFromB")
  
  x <- as.data.frame(UCT[[1]])
  y <- as.data.frame(legend)
  
  merged <- merge(
    x, y,
    by = c("NoMatchToAStar","NoMatchToB","NoMatchFromAStar","NoMatchFromB"),
    all.x = TRUE
  )
  
  merged <- merged[order(as.numeric(merged$sort)), ]
  UTC_T  <- UTC_T[order(as.numeric(UTC_T$sort)), ]
  
  
  expect_equal(merged[,19], UTC_T[,8])
  
}

