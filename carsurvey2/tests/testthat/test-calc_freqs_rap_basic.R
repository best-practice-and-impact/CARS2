dummy_data <- data.frame(basic_rap_score = c(0,1,6,8,"other",NA,0,0,1,6,6,6))

rap_score_basic_frequencies_dummy <- carsurvey2::calc_freqs_rap_basic(dummy_data)

test_that("Funtion checks frequency of basic RAP score", {
  expect_identical(c(as.character(rap_score_basic_frequencies_dummy[1,1]),as.numeric(rap_score_basic_frequencies_dummy[1,2])), c("0", 3))
  expect_identical(c(as.character(rap_score_basic_frequencies_dummy[2,1]),as.numeric(rap_score_basic_frequencies_dummy[2,2])), c("1", 2))
  expect_identical(c(as.character(rap_score_basic_frequencies_dummy[3,1]),as.numeric(rap_score_basic_frequencies_dummy[3,2])), c("6", 4))
  expect_identical(c(as.character(rap_score_basic_frequencies_dummy[4,1]),as.numeric(rap_score_basic_frequencies_dummy[4,2])), c("8", 1))
  expect_identical(c(as.character(rap_score_basic_frequencies_dummy[5,1]),as.numeric(rap_score_basic_frequencies_dummy[5,2])), c("other", 1))
})

test_that("Function checks column names", {
  expect_identical(colnames(rap_score_basic_frequencies_dummy), c("Basic RAP score", "Count"))
})

test_that("Function checks number of rows and columns", {
  expect_equal(nrow(rap_score_basic_frequencies_dummy),5)
  expect_equal(ncol(rap_score_basic_frequencies_dummy),2)
})