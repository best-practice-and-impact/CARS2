dummy_q_names <- c(
  "question number 1",
  "question number 2",
  "question number 3",
  "question number 4",
  "question number 5"
)

dummy_variable_lengths <- c(
  "this is question number 1",
  "question number 2",
  "question3"
)

dummy_long_q <- "question number 1: this is a long description of question number 1"

test_that("Function returns the correct number of strings", {
  expect_equal(length(break_q_names(dummy_q_names)), length(dummy_q_names))
  expect_equal(length(break_q_names(dummy_variable_lengths)), length(dummy_variable_lengths))
})

test_that("Function returns the correct number of line breaks for a group of strings", {
  expect_equal(sum(grepl("<br>", break_q_names(dummy_q_names))), 5)
  expect_equal(sum(grepl("<br>", break_q_names(dummy_variable_lengths))), 2)
})

test_that("Function returns the correct number of line breaks for a single string", {
  expect_equal(sum(stringr::str_count(break_q_names(dummy_long_q, 2), "<br>")), 1)
  expect_equal(sum(stringr::str_count(break_q_names(dummy_long_q, 5), "<br>")), 4)
})