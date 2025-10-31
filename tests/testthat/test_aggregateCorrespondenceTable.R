
test_that("aggregateCorrespondenceTable returns correct result for normal input", {
  AB <- system.file("extdata/test", "ab_data.csv", package = "correspondenceTables")
  A <- system.file("extdata/test", "a_data.csv", package = "correspondenceTables")
  B <- system.file("extdata/test", "b_data.csv", package = "correspondenceTables")
  
  ACT_normalT <- aggregateCorrespondenceTable(AB, A, B, NULL)
  
  ACText_normalT <- system.file("extdata/test", "AGTresult1.csv", package = "correspondenceTables")
  ACText_normalT <- utils::read.csv(ACText_normalT, sep = ",", header = TRUE, check.names = FALSE,
                                    colClasses = c("character"), encoding = "UTF-8")
  
  expect_equal(ACT_normalT, ACText_normalT)
})

test_that("aggregateCorrespondenceTable errors on duplicate data", {
  AB <- system.file("extdata/test", "ab_data_duplicate.csv", package = "correspondenceTables")
  A <- system.file("extdata/test", "a_data.csv", package = "correspondenceTables")
  B <- system.file("extdata/test", "b_data.csv", package = "correspondenceTables")
  
  expect_error(aggregateCorrespondenceTable(AB, A, B, NULL))
})
 
test_that("aggregateCorrespondenceTable errors on empty data", {
  AB <- system.file("extdata/test", "ab_data_empty.csv", package = "correspondenceTables")
  A <- system.file("extdata/test", "a_data.csv", package = "correspondenceTables")
  B <- system.file("extdata/test", "b_data.csv", package = "correspondenceTables")
  
  expect_error(aggregateCorrespondenceTable(AB, A, B, NULL))
})

test_that("aggregateCorrespondenceTable errors on missing code", {
  AB <- system.file("extdata/test", "ab_data_missingcode.csv", package = "correspondenceTables")
  A <- system.file("extdata/test", "a_data.csv", package = "correspondenceTables")
  B <- system.file("extdata/test", "b_data.csv", package = "correspondenceTables")
  
  expect_error(aggregateCorrespondenceTable(AB, A, B, NULL))
})

test_that("aggregateCorrespondenceTable errors on text suppression issues", {
  AB <- system.file("extdata/test", "ab_data.csv", package = "correspondenceTables")
  A <- system.file("extdata/test", "a_data_textsup.csv", package = "correspondenceTables")
  B <- system.file("extdata/test", "b_data.csv", package = "correspondenceTables")
  
  expect_error(aggregateCorrespondenceTable(AB, A, B, NULL))
})
