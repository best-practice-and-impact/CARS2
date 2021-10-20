
test_that("function returns character string", {
  expect_type(insert_table_toggle("valid-id"), "character")
  expect_type(insert_table_toggle("valid.id"), "character")
  expect_type(insert_table_toggle("valid_id"), "character")
  expect_equal(length(insert_table_toggle("valid-id")), 1)
  expect_equal(length(insert_table_toggle("valid.id")), 1)
  expect_equal(length(insert_table_toggle("valid_id")), 1)
})

test_that("function rejects invalid html ids", {
  expect_error(insert_table_toggle("4name"))
  expect_error(insert_table_toggle("_name"))
  expect_error(insert_table_toggle("name 1"))
  expect_error(insert_table_toggle("name~2"))
})

