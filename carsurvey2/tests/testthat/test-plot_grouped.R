data_2col_2group <- data.frame(bars = c("bar1", "bar2"),
                               groups = c("groupA", "groupB"),
                               val = c(3, 5))

data_3col_2group <- data.frame(bars = c("bar1", "bar2", "bar3"),
                               groups = c("groupA", "groupA", "groupB"),
                               val = c(3, 5, 4))

data_4col_2group <- data.frame(bars = c("bar1", "bar2", "bar3", "bar4"),
                               groups = c("groupA", "groupA", "groupB", "groupB"),
                               val = c(3, 5, 5, 2))

data_5col_3group <- data.frame(bars = c("bar1", "bar2", "bar3", "bar4", "bar5"),
                               groups = c("groupA", "groupA", "groupB", "groupB", "groupC"),
                               val = c(3, 5, 7, 4, 3))

data_6col_3group <- data.frame(bars = c("bar1", "bar2", "bar3", "bar4", "bar5", "bar6"),
                               groups = c("groupA", "groupB", "groupC", "groupA", "groupB", "groupA"),
                               val = c(3, 5, 5, 7, 2, 4))


test_that("function returns plotly html widget", {
  expect_identical(class(plot_grouped(data_2col_2group, "axis1", "axis2", n = 10)), c("plotly", "htmlwidget"))
  expect_identical(class(plot_grouped(data_3col_2group, "axis1", "axis2", n = 10)), c("plotly", "htmlwidget"))
  expect_identical(class(plot_grouped(data_4col_2group, "axis1", "axis2", n = 10)), c("plotly", "htmlwidget"))
  expect_identical(class(plot_grouped(data_5col_3group, "axis1", "axis2", n = 10)), c("plotly", "htmlwidget"))
  expect_identical(class(plot_grouped(data_6col_3group, "axis1", "axis2", n = 10)), c("plotly", "htmlwidget"))
})
