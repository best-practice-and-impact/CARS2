dummy_data <- data.frame(code_freq = c("Never", "Sometimes", "All the time", "other", 3) )

dummy_freq_table <- carsurvey2::calc_freqs_coding(dummy_data)


test_that("Function checks each factor and frequency" , {
  expect_identical(c(as.character(dummy_freq_table[1,1]),as.numeric(dummy_freq_table[1,2])), c("Never", 1))
  expect_identical(c(as.character(dummy_freq_table[2,1]),as.numeric(dummy_freq_table[2,2])), c("Rarely", 0))
  expect_identical(c(as.character(dummy_freq_table[3,1]),as.numeric(dummy_freq_table[3,2])), c("Sometimes", 1))
  expect_identical(c(as.character(dummy_freq_table[4,1]),as.numeric(dummy_freq_table[4,2])), c("Regularly", 0))
  expect_identical(c(as.character(dummy_freq_table[5,1]),as.numeric(dummy_freq_table[5,2])), c("All the time", 1))
})

test_that("Function checks column names of frequency table" , {
  expect_identical(colnames(dummy_freq_table[1]), "Coding frequency")
  expect_identical(colnames(dummy_freq_table[2]), "Count")
})

test_that("function checks number of columns and rows" , {
  expect_equal(nrow(dummy_freq_table), 5)
  expect_equal(ncol(dummy_freq_table), 2)
})