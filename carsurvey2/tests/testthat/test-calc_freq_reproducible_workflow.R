dummy <- data.frame(use_reprod_workflow = c("I don't know what reproducible workflows are", "I don't know what reproducible workflows are", "Yes", "Other", 1, NA))

calc_freq_rep_work_test <- carsurvey2::calc_freq_reproducible_workflow(dummy)

test_that("Function to test reproducible workflow frequency", {
  expect_identical(c(as.character(calc_freq_rep_work_test[1,1]), as.numeric(calc_freq_rep_work_test[1,2])), c("Yes",1))
  expect_identical(c(as.character(calc_freq_rep_work_test[2,1]), as.numeric(calc_freq_rep_work_test[2,2])), c("No",0))
  expect_identical(c(as.character(calc_freq_rep_work_test[3,1]), as.numeric(calc_freq_rep_work_test[3,2])), c("Don't know what they are",2))
})

test_that("Function to test names of columns", {
  expect_identical(colnames(calc_freq_rep_work_test), c("Use reproducible workflow packages","Count"))
})

test_that("Function that checks number of columns and rows", {
  expect_equal(nrow(calc_freq_rep_work_test), 3)
  expect_equal(ncol(calc_freq_rep_work_test), 2)
})