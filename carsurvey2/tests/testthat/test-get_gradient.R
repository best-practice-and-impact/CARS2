test_that("function returns the correct number of colours", {
  expect_length(get_gradient(n = 1), 1)
  expect_length(get_gradient(n = 2), 2)
  expect_length(get_gradient(n = 3), 3)
  expect_length(get_gradient(n = 4), 4)
  expect_length(get_gradient(n = 5), 5)
  expect_length(get_gradient(n = 6), 6)
  expect_length(get_gradient(n = 7), 7)
  expect_length(get_gradient(n = 8), 8)
})


