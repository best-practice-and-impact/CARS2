dummy <- data.frame(code_learn_where = c("In education", "In public sector employment", "Self-taught","Self-taught","Self-taught", "other_1","other_2",1,NA),
                    learn_before = c("Yes","Yes","Yes","No","Yes","Yes","Yes","No","Mo"),
                    code_freq = c("Never", "Always", "Always", "Always", "Always", "Always", "Always", "Always", "Always"))

learn_code_test <- carsurvey2::calc_freq_learn_code(dummy)

expected_values <- data.frame("First coding experience" = c("In current role",
                                                     "In education",
                                                     "In private sector employment",
                                                     "In public sector employment",
                                                     "Self-taught",
                                                     "Other"),
                              "Count" = c(2, 1, 0, 1, 3, 3))
expected_values[[1]] <- factor(expected_values[[1]], levels = expected_values[[1]])
colnames(expected_values) <- c("First coding experience", "Count")

test_that("Output matches expected values", {
  expect_identical(learn_code_test, expected_values)
})
