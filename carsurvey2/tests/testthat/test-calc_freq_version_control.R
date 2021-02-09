dummy <- data.frame(use_github = c("Yes","No",1,NA,"Other"),
                    use_bitbucket = c("Yes","Yes","Yes","No","Yes"),
                    use_googlecloud= c("No","No","No","No","No"))

calc_freq_version_control_test <- carsurvey2::calc_freq_version_control(dummy)

test_that("Function checks frequency of version control use", {
  expect_identical(c(as.character(calc_freq_version_control_test[1,1]), as.numeric(calc_freq_version_control_test[1,2:3])), c("GitHub", 1, 1))
  expect_identical(c(as.character(calc_freq_version_control_test[2,1]), as.numeric(calc_freq_version_control_test[2,2:3])), c("BitBucket", 4, 1))
  expect_identical(c(as.character(calc_freq_version_control_test[3,1]), as.numeric(calc_freq_version_control_test[3,2:3])), c("Cloud Source Repository (Google Cloud)", 0, 5))
})

test_that("Function that checks column names", {
  expect_identical(colnames(calc_freq_version_control_test), c("Question", "Yes", "No"))
})

test_that("Function that checks number of rows and columns", {
  expect_equal(nrow(calc_freq_version_control_test),3)
  expect_equal(NCOL(calc_freq_version_control_test),3)
})