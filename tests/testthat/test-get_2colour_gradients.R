test_that("function returns the correct number of colours", {
  expect_length(get_2colour_gradients(n = 2), 2)
  expect_length(get_2colour_gradients(n = 3), 3)
  expect_length(get_2colour_gradients(n = 4), 4)
  expect_length(get_2colour_gradients(n = 5), 5)
  expect_length(get_2colour_gradients(n = 6), 6)
  expect_length(get_2colour_gradients(n = 7), 7)
  expect_length(get_2colour_gradients(n = 8), 8)
})


