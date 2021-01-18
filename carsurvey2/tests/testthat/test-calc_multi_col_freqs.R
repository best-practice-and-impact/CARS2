# No nested survey questions
dummy_data0 <- data.frame(
  id = c(1, 2),
  IP.address = c("754", "42"),
  Q1..question.name = c("yes", "no"),
  Q2..question.name = c("no", "no")
)

dummy_data <- data.frame(columnA = c("Yes", "Yes", "No"),
                         columnB = c("No", "No", "No"),
                         columnC = c("Yes", "No", "No"))


freqs <- calc_multi_col_freqs(dummy_data, factor_levels = c("Yes", "No"))
props <- calc_multi_col_freqs(dummy_data, factor_levels = c("Yes", "No"), calc_props = TRUE)



test_that("function returns correct frequencies", {
  expect_equal(freqs$question, c("columnA", "columnB", "columnC"))
  expect_equal(freqs$Yes, c(2, 0, 1))
  expect_equal(freqs$No, c(1, 3, 2))
})

test_that("function returns correct proportions", {
  expect_equal(props$Yes, c(2/3, 0, 1/3))
  expect_equal(props$No, c(1/3, 1, 2/3))
})
