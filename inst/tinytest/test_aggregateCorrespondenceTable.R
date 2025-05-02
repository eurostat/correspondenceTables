#TEST 1a
AB = system.file("extdata", "ab_data.csv", package = "correspondenceTables")
A = system.file("extdata", "a_data.csv", package = "correspondenceTables")
B = system.file("extdata", "b_data.csv", package = "correspondenceTables")

ACT_normalT = aggregateCorrespondenceTable(AB, A, B, NULL)

ACText_normalT = system.file("extdata/test", "AGTresult1.csv", package = "correspondenceTables")
ACText_normalT = utils::read.csv(ACText_normalT, sep = ",", header = TRUE, check.names = FALSE, colClasses = c("character"), encoding = "UTF-8")

expect_equal(ACT_normalT, ACText_normalT)

#TEST 1b
AB = system.file("extdata", "ab_data_duplicate.csv", package = "correspondenceTables")
A = system.file("extdata", "a_data.csv", package = "correspondenceTables")
B = system.file("extdata", "b_data.csv", package = "correspondenceTables")

expect_error(ACT_normalT= aggregateCorrespondenceTable(AB, A, B, NULL))

#TEST 1c
AB = system.file("extdata", "ab_data_empty.csv", package = "correspondenceTables")
A = system.file("extdata", "a_data.csv", package = "correspondenceTables")
B = system.file("extdata", "b_data.csv", package = "correspondenceTables")

expect_error(ACT_emptylT= aggregateCorrespondenceTable(AB, A, B, NULL))


#TEST 1d
