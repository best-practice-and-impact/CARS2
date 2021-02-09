dummy_widget <- htmlwidgets::createWidget("dummy_name", "x")
dummy_table <- knitr::kable(data.frame(x = c("a", "b", "c"), y = c(1, 2, 3)))

test_that("function rejects invalid html ids", {
  expect_error(wrap_outputs("4name", dummy_widget, dummy_table))
  expect_error(wrap_outputs("_name", dummy_widget, dummy_table))
  expect_error(wrap_outputs("name 1", dummy_widget, dummy_table))
  expect_error(wrap_outputs("name~2", dummy_widget, dummy_table))
})

test_that("function returns html widget", {
  expect_s3_class(wrap_outputs("name", dummy_widget, dummy_table), "htmlwidget")
})

