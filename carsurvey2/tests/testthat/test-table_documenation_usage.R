library(testthat)

dummy_data <-  carsurvey2::data_rename_cols(carsurvey2::dummy_data) 


code_prac_levels = c("I don't understand this question",
                     "Never",
                     "Rarely",
                     "Sometimes",
                     "Regularly",
                     "All the time")

documenation_usage_dummy <- carsurvey2::table_documenation_usage(dummy_data,code_prac_levels)

test_that("Function checks percentages of factors", {
  expect_identical(c(as.character(documenation_usage_dummy[1,1]),round(as.numeric(documenation_usage_dummy[1,2:7]),7)), c("Analytical Quality Assurance (AQA) logs",0.1590643,	0.1532164,	0.1894737,	0.1614035,	0.1695906,	0.1672515))
  expect_identical(c(as.character(documenation_usage_dummy[2,1]),round(as.numeric(documenation_usage_dummy[2,2:7]),7)), c("Data or assumptions registers",	0.1590643,	0.1532164,	0.1894737,	0.1614035,	0.1695906,	0.1672515))
  expect_identical(c(as.character(documenation_usage_dummy[3,1]),round(as.numeric(documenation_usage_dummy[3,2:7]),7)), c("Documentation for each function or class" ,0.1590643	,0.1532164	,0.1894737	,0.1614035	,0.1695906	,0.1672515))
  expect_identical(c(as.character(documenation_usage_dummy[4,1]),round(as.numeric(documenation_usage_dummy[4,2:7]),7)), c("Code comments"	,0.0000000	,0.0000000	,0.2269006	,0.2619883	,0.2526316	,0.2584795))
  expect_identical(c(as.character(documenation_usage_dummy[5,1]),round(as.numeric(documenation_usage_dummy[5,2:7]),7)), c("Flow charts"	,0.2584795	,0.0000000	,0.2269006	,0.2619883	,0.2526316	,0.0000000))
  expect_identical(c(as.character(documenation_usage_dummy[6,1]),round(as.numeric(documenation_usage_dummy[6,2:7]),7)), c("README files"	,0.1590643	,0.1532164	,0.1894737	,0.1614035	,0.1695906	,0.1672515))
  expect_identical(c(as.character(documenation_usage_dummy[7,1]),round(as.numeric(documenation_usage_dummy[7,2:7]),7)), c("Desk notes"	,0.1590643	,0.1532164	,0.1894737	,0.1614035	,0.1695906	,0.1672515))
})

test_that("Function to check colum names", {
  expect_identical(colnames(documenation_usage_dummy), c("Question", "Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree", "All the time"))
})

test_that("Function to test number of columns and rows", {
  expect_equal(nrow(documenation_usage_dummy),7)
  expect_equal(ncol(documenation_usage_dummy),7)
})