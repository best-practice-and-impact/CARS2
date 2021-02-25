dummy_data <- data.frame(gp_function = c("Never", "Sometimes", "All the time", 3, "Sometimes"),
                         gp_open_source = c("I don't understand this question", "Rarely", "Regularly", NA, "Rarely"),
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

expected_values <- data.frame("Question" = c("I write repetitive elements in my code as functions",
                                             "I use open source software when programming",
                                             "I unit test my code",
                                             "gp_other"),
                              "I don't understand this question" = c(0, 1/3, 0, 0),
                              "Never" = c(1/3, 0, 0, 1/3),
                              "Rarely" = c(0, 1/3, 1/2, 0),
                              "Sometimes" = c(1/3, 0, 0, 1/3),
                              "Regularly" = c(0, 1/3, 1/2, 0),
                              "All the time" = c(1/3, 0, 0, 1/3))
colnames(expected_values) <- c("Question", code_prac_levels)

test_that("Output matches expected values", {
  expect_identical(c(as.character(coding_practices_dummy[1, 1]), round(as.numeric(coding_practices_dummy[1, 2:7]), 7)), c("I write repetitive elements in my code as functions", 0.0000000, 0.3333333,0.0000000,0.3333333,0.0000000,0.3333333))
})


