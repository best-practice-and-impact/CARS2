dummy_data <- data.frame(doc_AQA_log = c("I don't understand this question","Rarely","I don't understand this question","Sometimes", 1),
                         doc_desk = c("Regularly","Sometimes","All the time",NA,"Never"),
                         gp_function = c("Never","Sometimes","All the time",3,"Sometimes"),
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

documenation_usage_dummy <- carsurvey2::calc_freqs_documenation_usage(dummy_data,code_prac_levels)

test_that("Percentages match expected values", {
  expect_identical(c(as.character(documenation_usage_dummy[1,1]),round(as.numeric(documenation_usage_dummy[1,2:7]),7)), c("Analytical Quality Assurance (AQA) logs",0.5,	0,	0.25,	0.2500000,	0.0000000,	0.0000000))
  expect_identical(c(as.character(documenation_usage_dummy[2,1]),round(as.numeric(documenation_usage_dummy[2,2:7]),7)), c("Desk notes"	,0.0	,0	,0.00	,0.3333333	,0.3333333	,0.3333333))
})

test_that("Column names match expected values", {
  expect_identical(colnames(documenation_usage_dummy), c("Question", "I don't understand this question", "Never", "Rarely", "Sometimes", "Regularly", "All the time"))
})

test_that("Data frame matches expected values", {
  expect_equal(nrow(documenation_usage_dummy),2)
  expect_equal(ncol(documenation_usage_dummy),7)
})