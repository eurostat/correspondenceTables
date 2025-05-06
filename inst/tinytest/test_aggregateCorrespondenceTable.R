#TEST 1
AB = system.file("extdata/test", "ab_data.csv", package = "correspondenceTables")
A = system.file("extdata/test", "a_data.csv", package = "correspondenceTables")
B = system.file("extdata/test", "b_data.csv", package = "correspondenceTables")

ACT_normalT = aggregateCorrespondenceTable(AB, A, B, NULL)

ACText_normalT = system.file("extdata/test", "AGTresult1.csv", package = "correspondenceTables")
ACText_normalT = utils::read.csv(ACText_normalT, sep = ",", header = TRUE, check.names = FALSE, colClasses = c("character"), encoding = "UTF-8")

expect_equal(ACT_normalT, ACText_normalT)

#TEST 2a
AB = system.file("extdata/test", "ab_data_duplicate.csv", package = "correspondenceTables")
A = system.file("extdata/test", "a_data.csv", package = "correspondenceTables")
B = system.file("extdata/test", "b_data.csv", package = "correspondenceTables")

expect_error(ACT_dupT= aggregateCorrespondenceTable(AB, A, B, NULL))

#TEST 2b
AB = system.file("extdata/test", "ab_data_empty.csv", package = "correspondenceTables")
A = system.file("extdata/test", "a_data.csv", package = "correspondenceTables")
B = system.file("extdata/test", "b_data.csv", package = "correspondenceTables")

expect_error(ACT_emptyT= aggregateCorrespondenceTable(AB, A, B, NULL))


#TEST 2c

AB = system.file("extdata/test", "ab_data_missingcode.csv", package = "correspondenceTables")
A = system.file("extdata/test", "a_data.csv", package = "correspondenceTables")
B = system.file("extdata/test", "b_data.csv", package = "correspondenceTables")

expect_error(ACT_missingT= aggregateCorrespondenceTable(AB, A, B, NULL))

#TEST 2d

AB = system.file("extdata/test", "ab_data.csv", package = "correspondenceTables")
A = system.file("extdata/test", "a_data_textsup.csv", package = "correspondenceTables")
B = system.file("extdata/test", "b_data.csv", package = "correspondenceTables")

expect_error(ACT_missingT= aggregateCorrespondenceTable(AB, A, B, NULL))