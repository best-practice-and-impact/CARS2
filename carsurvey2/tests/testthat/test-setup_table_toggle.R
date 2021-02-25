test_that("function returns character string", {
  expect_type(setup_table_toggle(), "character")
  expect_equal(length(setup_table_toggle()), 1)
})


