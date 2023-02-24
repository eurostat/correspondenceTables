
# TEST 1a
A = system.file("extdata/test", "NACE2.csv", package = "correspondenceTables")
AStar = system.file("extdata/test", "NACE21.csv", package = "correspondenceTables")
B = system.file("extdata/test", "CPA.csv", package = "correspondenceTables")
AB = system.file("extdata/test", "NACECPA.csv", package = "correspondenceTables")
AAStar = system.file("extdata/test", "NACE221.csv", package = "correspondenceTables")

CT_nullT = updateCorrespondenceTable(A, B, AStar, AB, AAStar, NULL, "A", 0.4, 0.4, FALSE)

Ctext_nullT = system.file("extdata/test", "LblYY.csv", package = "correspondenceTables")
Ctext_nullT = utils::read.csv(Ctext_nullT, sep = ",", header = TRUE, check.names = FALSE, colClasses = c("character"), encoding = "UTF-8")

expect_equal(CT_nullT[[1]], Ctext_nullT)

# TEST 1b
Ano = system.file("extdata/test", "NACE2no.csv", package = "correspondenceTables")
AStarno = system.file("extdata/test", "NACE21no.csv", package = "correspondenceTables")
Bno = system.file("extdata/test", "CPA.csv", package = "correspondenceTables")
ABno = system.file("extdata/test", "NACECPA.csv", package = "correspondenceTables")
AAStarno = system.file("extdata/test", "NACE221.csv", package = "correspondenceTables")

CT_nullTno = updateCorrespondenceTable(Ano, Bno, AStarno, ABno, AAStarno, NULL, "A", 0.4, 0.4, FALSE)

Ctext_nullTno = system.file("extdata/test", "LblNY.csv", package = "correspondenceTables")
Ctext_nullTno = utils::read.csv(Ctext_nullTno, sep = ",", header = TRUE, check.names = FALSE, colClasses = c("character"), encoding = "UTF-8")

expect_equal(CT_nullTno[[1]][,1:10], Ctext_nullTno[,1:10])


# TEST 1c
A = system.file("extdata/test", "UcaseA.csv", package = "correspondenceTables")
AStar = system.file("extdata/test", "UcaseAstar.csv", package = "correspondenceTables")
B = system.file("extdata/test", "UcaseB.csv", package = "correspondenceTables")
AB = system.file("extdata/test", "UcaseAB.csv", package = "correspondenceTables")
AAStar = system.file("extdata/test", "UcaseAAstar.csv", package = "correspondenceTables")

UCT = updateCorrespondenceTable(A, B, AStar, AB, AAStar, NULL, "B", 0.8, 0.8, FALSE)
UCT[[1]]$sort = 1:nrow(UCT[[1]])

UTC_T = system.file("extdata/test", "nomatch_test.csv", package = "correspondenceTables")
UTC_T = utils::read.csv(UTC_T, sep = ",", header = TRUE, check.names = FALSE, colClasses = c("character"), encoding = "UTF-8")
UTC_T$sort = 1:nrow(UTC_T)

legend = system.file("extdata/test", "legend_nomatch.csv", package = "correspondenceTables")
legend = utils::read.csv(legend, sep = ",", header = TRUE, check.names = FALSE, colClasses = c("character"), encoding = "UTF-8")
colnames(legend)[2:5] = c("NoMatchToAStar","NoMatchToB", "NoMatchFromAStar", "NoMatchFromB")

x = as.data.frame(UCT[[1]])
y = as.data.frame(legend)

test = merge(x,y, by=c("NoMatchToAStar","NoMatchToB", "NoMatchFromAStar", "NoMatchFromB"), all.x = TRUE)

test = test[order(as.numeric(test$sort)),]
UTC_T = UTC_T[order(as.numeric(UTC_T$sort)),]

expect_equal(test[,19], UTC_T[,8])

# TEST 2 - Redundancy
A = system.file("extdata/test", "NACE2_Red.csv", package = "correspondenceTables")
AStar = system.file("extdata/test", "NACE21_Red.csv", package = "correspondenceTables")
B = system.file("extdata/test", "CPA_Red.csv", package = "correspondenceTables")
AB = system.file("extdata/test", "NACECPA_Red.csv", package = "correspondenceTables")
AAStar = system.file("extdata/test", "NACE221_Red.csv", package = "correspondenceTables")

CT_nullT = updateCorrespondenceTable(A, B, AStar, AB, AAStar, NULL, "none", 0.4, 0.4, TRUE)
CT_nullS = updateCorrespondenceTable(A, B, AStar, AB, AAStar, NULL, "none", 0.4, 0.4, FALSE)

CT_AT = updateCorrespondenceTable(A, B, AStar, AB, AAStar, NULL, "A", 0.4, 0.4, TRUE)
CT_AS = updateCorrespondenceTable(A, B, AStar, AB, AAStar, NULL, "A", 0.4, 0.4, FALSE)

CT_BT = updateCorrespondenceTable(A, B, AStar, AB, AAStar, NULL, "B", 0.4, 0.4, TRUE)
CT_BS = updateCorrespondenceTable(A, B, AStar, AB, AAStar, NULL, "B", 0.4, 0.4, FALSE)

Ctext_nullT = system.file("extdata/test", "LblYY_RednullTrim.csv", package = "correspondenceTables")
Ctext_nullT = utils::read.csv(Ctext_nullT, sep = ",", header = TRUE, check.names = FALSE, colClasses = c("character"), encoding = "UTF-8")
Ctext_nullS = system.file("extdata/test", "LblYY_RednullShow.csv", package = "correspondenceTables")
Ctext_nullS = utils::read.csv(Ctext_nullS, sep = ",", header = TRUE, check.names = FALSE, colClasses = c("character"), encoding = "UTF-8")

Ctext_AT = system.file("extdata/test", "LblYY_RedATrim.csv", package = "correspondenceTables")
Ctext_AT = utils::read.csv(Ctext_AT, sep = ",", header = TRUE, check.names = FALSE, colClasses = c("character"), encoding = "UTF-8")
Ctext_AS = system.file("extdata/test", "LblYY_RedAShow.csv", package = "correspondenceTables")
Ctext_AS = utils::read.csv(Ctext_AS, sep = ",", header = TRUE, check.names = FALSE, colClasses = c("character"), encoding = "UTF-8")

Ctext_BT = system.file("extdata/test", "LblYY_RedBTrim.csv", package = "correspondenceTables")
Ctext_BT = utils::read.csv(Ctext_BT, sep = ",", header = TRUE, check.names = FALSE, colClasses = c("character"), encoding = "UTF-8")
Ctext_BS = system.file("extdata/test", "LblYY_RedBShow.csv", package = "correspondenceTables")
Ctext_BS = utils::read.csv(Ctext_BS, sep = ",", header = TRUE, check.names = FALSE, colClasses = c("character"), encoding = "UTF-8")


expect_equal(CT_nullT[[1]], Ctext_nullT)
expect_equal(CT_AT[[1]], Ctext_AT)
expect_equal(CT_BT[[1]], Ctext_BT)

expect_equal(CT_nullS[[1]], Ctext_nullS)
expect_equal(CT_AS[[1]], Ctext_AS)
expect_equal(CT_BS[[1]], Ctext_BS)


# TEST 3 - new correspondence table
dir = system.file("extdata/test", package = "correspondenceTables")
csv_files = list.files(dir, pattern = ".csv")
if (length(csv_files)>0) unlink(csv_files)

fullPath = function(CSVraw, CSVappended){
  NamesCsv = system.file("extdata/test", CSVraw, package = "correspondenceTables")
  A = read.csv(NamesCsv, header = FALSE, sep = ",")
  for (i in 1:nrow(A)) {
    for (j in 1:ncol(A)) {
      if (A[i,j]!="") {
        A[i, j] = system.file("extdata/test", A[i, j], package = "correspondenceTables")
      }}}
  write.table(x = A, file = file.path(dir,CSVappended), row.names = FALSE, col.names = FALSE, sep = ",")
  return(A)
}

##new 1
fullPath("names1.csv", "names.csv")
TEST = newCorrespondenceTable(file.path(dir,"names.csv"), CSVout = NULL, "none", 0.96, Redundancy_trim = FALSE)
TEST_trim = newCorrespondenceTable(file.path(dir,"names.csv"), CSVout = NULL, "none", 0.96, Redundancy_trim = TRUE)

ABC = system.file("extdata/test", "ABC.csv", package = "correspondenceTables")
ABC = utils::read.csv(ABC, sep = ",", header = TRUE, check.names = FALSE, colClasses = c("character"), encoding = "UTF-8")

ABC_Trim = system.file("extdata/test", "ABC_trim.csv", package = "correspondenceTables")
ABC_Trim = utils::read.csv(ABC_Trim, sep = ",", header = TRUE, check.names = FALSE, colClasses = c("character"), encoding = "UTF-8")

expect_equal(TEST[[1]][1:5], ABC)
expect_equal(TEST_trim[[1]][1:4], ABC_Trim)

file.remove(file.path(dir,"names.csv"))

##new 2
fullPath("names2.csv", "names.csv")

TEST2 = newCorrespondenceTable(file.path(dir,"names.csv"), CSVout = NULL, "none", 0.96, Redundancy_trim = FALSE)
TEST2[[1]]$sort = 1:nrow(TEST2[[1]])

TEST2_T = system.file("extdata/test", "nomatch_test_new.csv", package = "correspondenceTables")
TEST2_T = utils::read.csv(TEST2_T, sep = ",", header = TRUE, check.names = FALSE, colClasses = c("character"), encoding = "UTF-8")
TEST2_T$sort = 1:nrow(TEST2_T)

legend = system.file("extdata/test", "legend_nomatch_new.csv", package = "correspondenceTables")
legend = utils::read.csv(legend, sep = ",", header = TRUE, check.names = FALSE, colClasses = c("character"), encoding = "UTF-8")

x = as.data.frame(TEST2[[1]])
y = as.data.frame(legend)

test = merge(x,y, by=c("Unmatched","NoMatchFromA", "NoMatchFromB"), all.x = TRUE)

test = test[order(as.numeric(test$sort)),]
TEST2_T = TEST2_T[order(as.numeric(TEST2_T$sort)),]

expect_equal(test[,15], TEST2_T[,13])

file.remove(file.path(dir,"names.csv"))