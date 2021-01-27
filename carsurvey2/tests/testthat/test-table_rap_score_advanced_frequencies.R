library(testthat)

dummy_data <-  carsurvey2::data_rename_cols(carsurvey2::dummy_data) %>%
  carsurvey2::data_derive_rap_scores()

rap_score_advanced_frequencies_dummy <- carsurvey2::table_rap_score_advanced_frequencies(dummy_data)

test_that("Funtion checks frequency of advanced RAP score", {
  expect_identical(c(as.character(rap_score_advanced_frequencies_dummy[1,1]),as.numeric(rap_score_advanced_frequencies_dummy[1,2])), c("0", 476))
  expect_identical(c(as.character(rap_score_advanced_frequencies_dummy[2,1]),as.numeric(rap_score_advanced_frequencies_dummy[2,2])), c("2", 220))
  expect_identical(c(as.character(rap_score_advanced_frequencies_dummy[3,1]),as.numeric(rap_score_advanced_frequencies_dummy[3,2])), c("5", 252))
  expect_identical(c(as.character(rap_score_advanced_frequencies_dummy[4,1]),as.numeric(rap_score_advanced_frequencies_dummy[4,2])), c("7", 112))
})

test_that("Function checks column names", {
  expect_identical(colnames(rap_score_advanced_frequencies_dummy), c("Advanced RAP score", "Count"))
})

test_that("Function checks number of rows and columns", {
  expect_equal(nrow(rap_score_advanced_frequencies_dummy),4)
  expect_equal(ncol(rap_score_advanced_frequencies_dummy),2)
})