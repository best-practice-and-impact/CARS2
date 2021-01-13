
data_2question_3answer <- data.frame(questions = c("Q1", "Q2"),
                                     disagree = c(.4, .2),
                                     neutral = c(.2, .5),
                                     agree = c(.4, .3))


data_2question_6answer <- data.frame(questions = c("Q1", "Q2"),
                                     strongly_disagree = c(.1, .1),
                                     disagree = c(.1, .2),
                                     neutral = c(.1, .1),
                                     agree = c(.2, .4),
                                     strongy_agree = c(.3, .1),
                                     not_applicable = c(.2, .1))

test_that("function returns plotly kableExtra table", {
  expect_identical(class(output_likert_table(data_2question_3answer)), c("kableExtra", "knitr_kable"))
  expect_identical(class(output_likert_table(data_2question_6answer)), c("kableExtra", "knitr_kable"))
})
