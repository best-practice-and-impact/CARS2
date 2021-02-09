dummy <- data.frame(code_learn_where = c("In education", "In public sector employment", "Self-taught","Self-taught","Self-taught", "other_1","other_2",1,NA),
                    learn_before = c("Yes","Yes","Yes","No","Yes","Yes","Yes","No","Yes"))

learn_code_test <- carsurvey2::calc_freq_learn_code(dummy)

test_that("Function that tests frequency", {
  expect_identical(c(as.character(learn_code_test[1,1]), as.numeric(learn_code_test[1,2])), c("In current role", 2))
  expect_identical(c(as.character(learn_code_test[2,1]), as.numeric(learn_code_test[2,2])), c("In education", 1))
  expect_identical(c(as.character(learn_code_test[5,1]), as.numeric(learn_code_test[3,2])), c("Self-taught", 0))
  expect_identical(c(as.character(learn_code_test[4,1]), as.numeric(learn_code_test[4,2])), c("In public sector employment", 1))
  expect_identical(c(as.character(learn_code_test[3,1]), as.numeric(learn_code_test[5,2])), c("In private sector employment", 3))
  expect_identical(c(as.character(learn_code_test[6,1]), as.numeric(learn_code_test[6,2])), c("Other", 3))
})

test_that("Function that checks column names", {
  expect_identical(colnames(learn_code_test), c("First learn code", "Count"))
})

test_that("Function that checks number of rows and columns", {
  expect_equal(nrow(learn_code_test), 6)
  expect_equal(ncol(learn_code_test), 2)
})