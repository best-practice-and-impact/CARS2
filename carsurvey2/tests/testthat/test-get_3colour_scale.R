test_that("function returns the correct number of colours", {
  expect_error(carsurvey2::get_3colour_scale(n = 2))
  expect_length(carsurvey2::get_3colour_scale(n = 3), 3)
  expect_error(carsurvey2::get_3colour_scale(n = 4))
})

