test_that("function returns the correct number of colours", {
  expect_error(get_3colour_scale(n = 2))
  expect_length(get_3colour_scale(n = 3), 3)
  expect_error(get_3colour_scale(n = 4))
})

