data_1col <- data.frame(bars = c("bar1"),
                        vals = c(10))

data_2col <- data.frame(bars = c("bar1", "bar2"),
                        vals = c(10, 6))

data_3col <- data.frame(bars = c("bar1", "bar2", "bar3"),
                        vals = c(10, 6, 2))

data_4col <- data.frame(bars = c("bar1", "bar2", "bar3", "bar4"),
                        vals = c(10, 6, 3, 4))

data_5col <- data.frame(bars = c("bar1", "bar2", "bar3", "bar4", "bar5"),
                        vals = c(10, 4, 5, 2, 3))

data_6col <- data.frame(bars = c("bar1", "bar2", "bar3", "bar4", "bar5", "bar6"),
                        vals = c(10, 5, 3, 7, 8, 4))


test_that("function returns plotly html widget", {
  expect_identical(class(plot_freqs(data_1col, "axis1", "axis2")), c("plotly", "htmlwidget"))
  expect_identical(class(plot_freqs(data_2col, "axis1", "axis2")), c("plotly", "htmlwidget"))
  expect_identical(class(plot_freqs(data_3col, "axis1", "axis2")), c("plotly", "htmlwidget"))
  expect_identical(class(plot_freqs(data_4col, "axis1", "axis2")), c("plotly", "htmlwidget"))
  expect_identical(class(plot_freqs(data_5col, "axis1", "axis2")), c("plotly", "htmlwidget"))
  expect_identical(class(plot_freqs(data_6col, "axis1", "axis2")), c("plotly", "htmlwidget"))
})
