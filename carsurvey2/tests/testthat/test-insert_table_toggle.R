
test_that("function returns raw html", {
  expect_s3_class(html_insert_table_toggle("valid-id"), "knit_asis")
  expect_s3_class(html_insert_table_toggle("valid.id"), "knit_asis")
  expect_s3_class(html_insert_table_toggle("valid_id"), "knit_asis")
})

test_that("function rejects invalid html ids", {
  expect_error(html_insert_table_toggle("4name"))
  expect_error(html_insert_table_toggle("_name"))
  expect_error(html_insert_table_toggle("name 1"))
  expect_error(html_insert_table_toggle("name~2"))
})

