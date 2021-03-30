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
                                                   "Data Transfer / Migration",
                                                   "Quality Assurance"),
                              "I do this without coding" = as.integer(c(1, 1, 2)),
                              "I do some or all of this by coding" = as.integer(c(0, 2, 0))) 
colnames(expected_output) <- c("Data operation", "I do this without coding", "I do some or all of this by coding")

dummy_output <- carsurvey2::calc_freq_operations(dummy)
rownames(dummy_output) <- NULL

test_that("Frequencies match expected outputs", {
  expect_identical(dummy_output, expected_output)
})

