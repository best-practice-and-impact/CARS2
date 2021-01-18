test_that("function returns raw html", {
  expect_s3_class(html_setup_table_toggle(), "knit_asis")
})

