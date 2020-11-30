data_1question_3answer <- data.frame(questions = c("Q1"),
                                     disagree = c(.4),
                                     neutral = c(.2),
                                     agree = c(.4))

data_2question_3answer <- data.frame(questions = c("Q1", "Q2"),
                                     disagree = c(.4, .2),
                                     neutral = c(.2, .5),
                                     agree = c(.4, .3))

data_3question_3answer <- data.frame(questions = c("Q1", "Q2", "Q3"),
                                     disagree = c(.4, .3, .1),
                                     neutral = c(.2, .4, .6),
                                     agree = c(.4, .3, .3))

data_2question_4answer <- data.frame(questions = c("Q1", "Q2"),
                                     disagree = c(.2, .3),
                                     nautral = c(.1, .2),
                                     agree = c(.3, .4),
                                     strongy_agree = c(.4, .1))

data_2question_5answer <- data.frame(questions = c("Q1", "Q2"),
                                     strongly_disagree = c(.1, .1),
                                     disagree = c(.2, .3),
                                     nautral = c(.1, .1),
                                     agree = c(.2, .4),
                                     strongy_agree = c(.4, .1))

data_2question_6answer <- data.frame(questions = c("Q1", "Q2"),
                                     strongly_disagree = c(.1, .1),
                                     disagree = c(.1, .2),
                                     nautral = c(.1, .1),
                                     agree = c(.2, .4),
                                     strongy_agree = c(.3, .1),
                                     not_applicable = c(.2, .1))

test_that("function returns plotly html widget", {
  expect_identical(class(plot_likert(data_1question_3answer, mid = 2, "axis1", "axis2")), c("plotly", "htmlwidget"))
  expect_identical(class(plot_likert(data_2question_3answer, mid = 2, "axis1", "axis2")), c("plotly", "htmlwidget"))
  expect_identical(class(plot_likert(data_3question_3answer, mid = 2, "axis1", "axis2")), c("plotly", "htmlwidget"))
  expect_identical(class(plot_likert(data_2question_4answer, mid = 2, "axis1", "axis2")), c("plotly", "htmlwidget"))
  expect_identical(class(plot_likert(data_2question_5answer, mid = 3, "axis1", "axis2")), c("plotly", "htmlwidget"))
  expect_identical(class(plot_likert(data_2question_6answer, mid = 3, "axis1", "axis2")), c("plotly", "htmlwidget"))
})
