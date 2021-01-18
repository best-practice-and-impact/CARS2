test_that("function returns the correct number of colours", {
  expect_length(get_2colour_scale(n = 2), 2)
  expect_length(get_2colour_scale(n = 3), 3)
  expect_length(get_2colour_scale(n = 4), 4)
  expect_length(get_2colour_scale(n = 5), 5)
  expect_length(get_2colour_scale(n = 6), 6)
  expect_length(get_2colour_scale(n = 7), 7)
  expect_length(get_2colour_scale(n = 8), 8)
})


