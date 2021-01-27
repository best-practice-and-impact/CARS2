library(testthat)

dummy_data <-  carsurvey2::data_rename_cols(carsurvey2::dummy_data) 


code_prac_levels = c("I don't understand this question",
                     "Never",
                     "Rarely",
                     "Sometimes",
                     "Regularly",
                     "All the time")

coding_practices_dummy <- carsurvey2::coding_practices(dummy_data,code_prac_levels)


test_that("Function checks percentages of factors", {
  expect_identical(c(as.character(coding_practices_dummy[1,1]),round(as.numeric(coding_practices_dummy[1,2:7]),7)), c("I use open source software when programming"	,0.1590643	,0.1532164	,0.1894737	,0.1614035	,0.1695906	,0.1672515))
  expect_identical(c(as.character(coding_practices_dummy[2,1]),round(as.numeric(coding_practices_dummy[2,2:7]),7)), c("I follow a standard directory structure when programming"	,0.1590643	,0.1532164	,0.1894737	,0.1614035	,0.1695906	,0.1672515))
  expect_identical(c(as.character(coding_practices_dummy[3,1]),round(as.numeric(coding_practices_dummy[3,2:7]),7)), c("I follow coding guidelines or style guides when programming"	,0.1590643	,0.1532164	,0.1894737	,0.1614035	,0.1695906	,0.1672515))
  expect_identical(c(as.character(coding_practices_dummy[4,1]),round(as.numeric(coding_practices_dummy[4,2:7]),7)), c("I use a source code version control system e.g. Git"	,0.1590643	,0.1532164	,0.1894737	,0.1614035	,0.1695906	,0.1672515))
  expect_identical(c(as.character(coding_practices_dummy[5,1]),round(as.numeric(coding_practices_dummy[5,2:7]),7)), c("Code my team writes is reviewed by a colleague"	,0.0000000	,0.0000000	,0.2269006	,0.2619883	,0.2526316	,0.2584795))
  expect_identical(c(as.character(coding_practices_dummy[6,1]),round(as.numeric(coding_practices_dummy[6,2:7]),7)), c("I write repetitive elements in my code as functions"	,0.1590643	,0.1532164	,0.1894737	,0.1614035	,0.1695906	,0.1672515))
  expect_identical(c(as.character(coding_practices_dummy[7,1]),round(as.numeric(coding_practices_dummy[7,2:7]),7)), c("I collect my code and supporting material into packages"	,0.1590643	,0.1532164	,0.1894737	,0.1614035	,0.1695906	,0.1672515))
  expect_identical(c(as.character(coding_practices_dummy[8,1]),round(as.numeric(coding_practices_dummy[8,2:7]),7)), c("I unit test my code"	,0.1590643	,0.1532164	,0.1894737	,0.1614035	,0.1695906	,0.1672515))
  expect_identical(c(as.character(coding_practices_dummy[9,1]),round(as.numeric(coding_practices_dummy[9,2:7]),7)), c("I write code to automatically quality assure data"	,0.1590643	,0.1532164	,0.1894737	,0.1614035	,0.1695906	,0.1672515))
  expect_identical(c(as.character(coding_practices_dummy[10,1]),round(as.numeric(coding_practices_dummy[10,2:7]),7)), c("My team open sources its code"	,0.1590643	,0.1532164	,0.1894737	,0.1614035	,0.1695906	,0.1672515))
})

test_that("Function to check colum names", {
  expect_identical(colnames(coding_practices_dummy), c("question", "I don't understand this question", "Never", "Rarely", "Sometimes", "Regularly", "All the time"))
})

test_that("Function to test number of columns and rows", {
  expect_equal(nrow(coding_practices_dummy),10)
  expect_equal(ncol(coding_practices_dummy),7)
})

