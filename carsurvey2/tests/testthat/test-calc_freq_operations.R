dummy <- data.frame(data_cleaning = c("I don't do this","other", 2,"I do this without coding"),
                    QA = c(2,"I do this without coding",NA,"I do this without coding"),
                    data_transfer = c("I do some or all of this by coding","I do some or all of this by coding","I don't do this","I do this without coding"))

operations_test <- carsurvey2::calc_freq_operations(dummy)

test_that("Function that tests the freqency",{
  expect_identical(c(as.character(operations_test[1,1]),as.numeric(operations_test[1,2:3])), c("Data Cleaning",1,0))
  expect_identical(c(as.character(operations_test[2,1]),as.numeric(operations_test[2,2:3])), c("Quality Assurance",2,0))
  expect_identical(c(as.character(operations_test[3,1]),as.numeric(operations_test[3,2:3])), c("Data Trasnfer / Migration",1,2))
})

test_that("Function to test column names", {
  expect_identical(colnames(operations_test), c("Data operation", "I do this without coding", "I do some or all of this by coding"))
  expect_identical(colnames(operations_test), c("Data operation",  "I do this without coding", "I do some or all of this by coding"))
})

test_that("function to test number of rows and column", {
  expect_equal(nrow(operations_test), 3)
  expect_equal(ncol(operations_test), 3)
})