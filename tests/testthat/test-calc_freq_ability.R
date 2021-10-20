dummy <- data.frame(ability_change = c("Significantly worse","Significantly worse","No change","No change","No change","Significantly better","other",1,NA))

ability_test <- carsurvey2::calc_freq_ability(dummy)

test_that("Function that tests the freqency",{
  expect_identical(c(as.character(ability_test[1,1]),as.numeric(ability_test[1,2])), c("Significantly worse",2))
  expect_identical(c(as.character(ability_test[2,1]),as.numeric(ability_test[2,2])), c("Slightly worse",0))
  expect_identical(c(as.character(ability_test[3,1]),as.numeric(ability_test[3,2])), c("No change",3))
  expect_identical(c(as.character(ability_test[4,1]),as.numeric(ability_test[4,2])), c("Slightly better",0))
  expect_identical(c(as.character(ability_test[5,1]),as.numeric(ability_test[5,2])), c("Significantly better",1))
})

test_that("Function to test column names", {
  expect_identical(colnames(ability_test), c("Coding ability changes", "Count"))
})

test_that("function to test number of rows and column", {
  expect_equal(nrow(ability_test), 5)
  expect_equal(ncol(ability_test), 2)
})