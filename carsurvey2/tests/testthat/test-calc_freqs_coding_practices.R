dummy_data <- data.frame(gp_function = c("Never","Sometimes","All the time",3,"Sometimes"),
                         gp_open_source = c("I don't understand this question","Rarely","Regularly",NA,"Rarely"),
                         gp_unit_test = c(1,"other","Rarely", "Regularly", 3) ,
                         code_freq = c("other","other","other","other","Never"), 
                         gp_other = c("Never","Sometimes","All the time",3,"Sometimes"))


code_prac_levels = c("I don't understand this question",
                     "Never",
                     "Rarely",
                     "Sometimes",
                     "Regularly",
                     "All the time")

coding_practices_dummy <- carsurvey2::calc_freqs_coding_practices(dummy_data,code_prac_levels)


test_that("Function checks percentages of factors", {
  expect_identical(c(as.character(coding_practices_dummy[1,1]),round(as.numeric(coding_practices_dummy[1,2:7]),7)), c("I write repetitive elements in my code as functions",0.0000000, 0.3333333,0.0000000,0.3333333,0.0000000,0.3333333))
  expect_identical(c(as.character(coding_practices_dummy[2,1]),round(as.numeric(coding_practices_dummy[2,2:7]),7)), c("I use open source software when programming"	,0.3333333	,0.0000000	,0.3333333	,0.0000000	,0.3333333	,0.0000000))
  expect_identical(c(as.character(coding_practices_dummy[3,1]),round(as.numeric(coding_practices_dummy[3,2:7]),7)), c("I unit test my code"	,0.0000000	,0.0000000	,0.5000000	,0.0000000	,0.5000000	,0.0000000))
  expect_identical(c(as.character(coding_practices_dummy[4,1]),round(as.numeric(coding_practices_dummy[4,2:7]),7)), c("gp_other"	,0.0000000, 0.3333333,0.0000000,0.3333333,0.0000000,0.3333333))
})

test_that("Function to check colum names", {
  expect_identical(colnames(coding_practices_dummy), c("question", "I don't understand this question", "Never", "Rarely", "Sometimes", "Regularly", "All the time"))
})

test_that("Function to test number of columns and rows", {
  expect_equal(nrow(coding_practices_dummy),4)
  expect_equal(ncol(coding_practices_dummy),7)
})

