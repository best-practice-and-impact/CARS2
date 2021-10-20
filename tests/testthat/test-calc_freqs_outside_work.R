dummy <- data.frame(code_experience = c("Yes", "No", "other", 1, "Yes", "Yes"))

outside_work_test <- carsurvey2::calc_freqs_outside_work(dummy)

test_that("Function that tests the freqency",{
  expect_identical(c(as.character(outside_work_test[1,1]),as.numeric(outside_work_test[1,2])), c("Yes",3))
  expect_identical(c(as.character(outside_work_test[2,1]),as.numeric(outside_work_test[2,2])), c("No",1))
  })

test_that("Function to test column names", {
  expect_identical(colnames(outside_work_test), c("code experience outside of work", "count"))
})

test_that("function to test number of rows and column", {
  expect_equal(nrow(outside_work_test), 2)
  expect_equal(ncol(outside_work_test), 2)
})