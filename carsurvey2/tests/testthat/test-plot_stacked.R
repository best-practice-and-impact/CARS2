data_1question_3answer <- data.frame(questions = c("Q1"),
                                     disagree = c(4),
                                     neutral = c(5),
                                     agree = c(5))

data_10question_3answer <- data.frame(questions = c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10"),
                                      disagree = c(4, 4, 6, 3, 1, 2, 3, 4, 5, 3),
                                      neutral = c(5, 5, 6, 4, 3, 2, 5, 3, 2, 3),
                                      agree = c(5, 4, 4, 3, 2, 5, 4, 3, 5, 4))


test_that("function returns plotly html widget", {
  expect_identical(class(plot_stacked(data_1question_3answer, xlab = "axis1", ylab = "axis2", n = 10)), c("plotly", "htmlwidget"))
  expect_identical(class(plot_stacked(data_10question_3answer, xlab = "axis1", ylab = "axis2", n = 10)), c("plotly", "htmlwidget"))
})
