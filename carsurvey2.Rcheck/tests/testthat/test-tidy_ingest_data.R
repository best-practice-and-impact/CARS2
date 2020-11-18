# No nested survey questions
dummy_data0 <- data.frame(
  id = c(1, 2),
  IP.address = c("754", "42"),
  Q1..question.name = c("yes", "no"),
  Q2..question.name = c("no", "no")
)

# Nested survey questions (1 level)
dummy_data1 <- data.frame(
  id = c(NA, NA, 1, 2),
  IP.address = c(NA, NA, "754", "42"),
  Q1..question.name = c(NA, NA, "yes", "no"),
  X.1 = c("Q.1.1..question.name", NA, "yes", "yes"),
  X.2 = c(NA, "Q.1.1.1..question.name", "no", "no"),
  X.3 = c("Q.1.2..question.name", "NA", "no", "yes"),
  Q2..question.name = c(NA, NA, "no", "no")
)

# Nested survey questions (2 levels)
dummy_data2 <- data.frame(
  id = c(NA, NA, 1, 2),
  IP.address = c(NA, NA, "754", "42"),
  Q1..question.name = c(NA, NA, "yes", "no"),
  X.1 = c("Q.1.1..question.name", NA, "yes", "yes"),
  X.2 = c(NA, "Q.1.1.1..question.name", "no", "no"),
  X.3 = c("Q.1.2..question.name", "NA", "no", "yes"),
  Q2..question.name = c(NA, NA, "no", "no")
)

# Nested survey questions (3 levels)
dummy_data3 <- data.frame(
  id = c(NA, NA, NA, 1, 2),
  IP.address = c(NA, NA, NA, "754", "42"),
  Q1..question.name = c(NA, NA, NA, "yes", "no"),
  X.1 = c("Q.1.1..question.name", NA, NA, "yes", "yes"),
  X.2 = c(NA, "Q.1.1.1..question.name", NA, "no", "no"),
  X.3 = c(NA, NA, "Q.1.2.3..question.name", "no", "yes"),
  Q2..question.name = c(NA, NA, NA, "no", "no")
)

test_that("function returns correct numbered column names", {
  expect_identical(colnames(tidy_ingest_data(dummy_data0)), c("userID", "IP.address", "Q1", "Q2"))
  expect_identical(colnames(tidy_ingest_data(dummy_data1)), c("userID", "IP.address", "Q1", "Q1.1", "Q1.2", "Q1.3", "Q2"))
  expect_identical(colnames(tidy_ingest_data(dummy_data2)), c("userID", "IP.address", "Q1", "Q1.1", "Q1.2", "Q1.3", "Q2"))
  expect_identical(colnames(tidy_ingest_data(dummy_data3)), c("userID", "IP.address", "Q1", "Q1.1", "Q1.2", "Q1.3", "Q2"))
})


test_that("function returns correct number of rows", {
  expect_equal(nrow(tidy_ingest_data(dummy_data0)), 2)
  expect_equal(nrow(tidy_ingest_data(dummy_data1)), 2)
  expect_equal(nrow(tidy_ingest_data(dummy_data2)), 2)
  expect_equal(nrow(tidy_ingest_data(dummy_data3)), 2)
})