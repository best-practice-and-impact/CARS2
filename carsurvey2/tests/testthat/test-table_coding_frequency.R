library(testthat)

dummy_data <- carsurvey2::data_rename_cols(carsurvey2::dummy_data)

dummy_freq_table <- carsurvey2::table_coding_frequency(dummy_data)


test_that("Function checks each factor and frequency" , {
  expect_identical(c(as.character(dummy_freq_table[1,1]),as.numeric(dummy_freq_table[1,2])), c("Never", 205))
  expect_identical(c(as.character(dummy_freq_table[2,1]),as.numeric(dummy_freq_table[2,2])), c("Rarely", 194))
  expect_identical(c(as.character(dummy_freq_table[3,1]),as.numeric(dummy_freq_table[3,2])), c("Sometimes", 224))
  expect_identical(c(as.character(dummy_freq_table[4,1]),as.numeric(dummy_freq_table[4,2])), c("Regularly", 216))
  expect_identical(c(as.character(dummy_freq_table[5,1]),as.numeric(dummy_freq_table[5,2])), c("All the time", 221))
})

test_that("Function checks column names of frequency table" , {
  expect_identical(colnames(dummy_freq_table[1]), "Coding frequency")
  expect_identical(colnames(dummy_freq_table[2]), "Count")
})

test_that("function checks only 5 factors" , {
  expect_equal(nrow(dummy_freq_table), 5)
})