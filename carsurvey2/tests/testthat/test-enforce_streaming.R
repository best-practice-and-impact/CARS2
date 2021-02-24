dummy <- data.frame(
  Q6 = c("Bachelor's degree (or equivalent)", "Master's degree (or equivalent)", "Doctoral degree (or equivalent)", "other", NA),
  Q7 = c("Yes", "Yes", "Yes", "Yes", NA),
  Q7.1 = c("Yes", "Yes", "Yes", "Yes", NA),
  Q7.16 = c("Yes", "Yes", "Yes", "Yes", NA),
  Q8 = c("Never", "Rarely", "Sometimes", "Regularly", "All the time"),
  Q11 = c("Yes", "No", "Yes", NA, NA),
  Q12 = c("dummy", "dummy", NA, NA, NA),
  Q13 = c("Yes", "dummy", "No", NA, NA),
  Q14 = c("dummy", "dummy", "dummy", NA, NA),
  Q15 = c("Yes", "No", "Yes", NA, NA),
  Q16 = c("Yes", "Yes", "No", NA, NA),
  Q17 = c("dummy", "dummy", "dummy", NA, NA),
  Q18.6 = c("dummy", "dummy", NA, NA, NA),
  Q19 = c("All the time", "Never", "I don't understand this question", "Sometimes", "Rarely"),
  Q19.3 = c("All the time", "Never", "I don't understand this question", "Sometimes", "Rarely"),
  Q20 = c("All the time", "Never", "I don't understand this question", "Sometimes", "Rarely"),
  Q24 = c("All the time", "Never", "I don't understand this question", "Sometimes", "Rarely"),
  Q25 = c("dummy","dummy", "dummy", "dummy", "dummy"),
  Q25.1 = c("dummy","dummy", "dummy", "dummy", "dummy"),
  Q25.5 = c("dummy","dummy", "dummy", "dummy", "dummy")
)

expected_output <- data.frame(
  Q6 = dummy$Q6,
  Q7 = c("Yes", "Yes", "Yes", NA, NA),
  Q7.1 = c("Yes", "Yes", "Yes", NA, NA),
  Q7.16 = c("Yes", "Yes", "Yes", NA, NA),
  Q8 = dummy$Q8,
  Q11 = dummy$Q11,
  Q12 = c("dummy", NA, NA, NA, NA),
  Q13 = c("Yes", NA, "No", NA, NA),
  Q14 = c("dummy", NA, NA, NA, NA),
  Q15 = dummy$Q15,
  Q16 = c("Yes", NA, "No", NA, NA),
  Q17 = c(NA, NA, "dummy", NA, NA),
  Q18.6 = c("dummy", NA, NA, NA, NA),
  Q19 = c(NA, "Never", "I don't understand this question", "Sometimes", "Rarely"),
  Q19.3 = c(NA, "Never", "I don't understand this question", "Sometimes", "Rarely"),
  Q20 = c(NA, "Never", "I don't understand this question", "Sometimes", "Rarely"),
  Q24 = c(NA, "Never", "I don't understand this question", "Sometimes", "Rarely"),
  Q25 = c("dummy", NA, NA, "dummy", "dummy"),
  Q25.1 = c("dummy", NA, NA, "dummy", "dummy"),
  Q25.5 = c("dummy",  NA, NA, "dummy", "dummy")
)

dummy_output <- carsurvey2::enforce_streaming(dummy)


test_that("Shape of cleaned values matches shape of original data", {
  expect_equal(nrow(dummy_output), nrow(dummy))
  expect_equal(ncol(dummy_output), ncol(dummy))
})

test_that("output values match expected values", {
  expect_true(identical(dummy_output, expected_output))
})
