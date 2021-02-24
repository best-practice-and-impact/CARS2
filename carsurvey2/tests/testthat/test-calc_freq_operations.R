dummy <- data.frame(data_cleaning = c("I don't do this" ,
                                      "other", 
                                      2, 
                                      "I do this without coding"),
                    QA = c(2,
                           "I do this without coding", 
                           NA, 
                           "I do this without coding"),
                    data_transfer = c("I do some or all of this by coding", 
                                      "I do some or all of this by coding", 
                                      "I don't do this", 
                                      "I do this without coding")
                    )

expected_output <- data.frame("Data operation" = c("Data Cleaning",
                                                   "Quality Assurance",
                                                   "Data Transfer / Migration"),
                              "I do this without coding" = as.integer(c(1, 2, 1)),
                              "I do some or all of this by coding" = as.integer(c(0, 0, 2))) 
colnames(expected_output) <- c("Data operation", "I do this without coding", "I do some or all of this by coding")

dummy_output <- carsurvey2::calc_freq_operations(dummy)

test_that("Frequencies match expected outputs", {
  expect_identical(dummy_output, expected_output)
})

