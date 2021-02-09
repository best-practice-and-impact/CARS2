dummy_data <- data.frame(RAP_heard_of = c("Yes","Yes","Yes","Yes","No"),
                         RAP_understand = c("Strongly Disagree","Disagree", "Neutral", "Disagree", "Disagree"),
                         RAP_other = c("Neutral","Agree", NA, "Agree", "Strongly Agree"),
                         RAP_using = c(1,2,3,4,5)
                         )

opinion_of_rap_dummy <- carsurvey2::calc_freqs_opinion_of_rap(dummy_data)

test_that("Function checks percentages of factors", {
  expect_identical(c(as.character(opinion_of_rap_dummy[1,1]),round(as.numeric(opinion_of_rap_dummy[1,2:6]),7)), c("I understand what the key components of the RAP methodology are",	0.25,	0.5,	0.2500000,	0.0000000,	0))
  expect_identical(c(as.character(opinion_of_rap_dummy[2,1]),round(as.numeric(opinion_of_rap_dummy[2,2:6]),7)), c("RAP_other",	0.00	,0.00	,0.3333333	,0.6666667	,0))
  expect_identical(c(as.character(opinion_of_rap_dummy[3,1]),round(as.numeric(opinion_of_rap_dummy[3,2:6]),7)), c("I and/or my team are currently implementing RAP","NaN"	,"NaN"	,"NaN"	,"NaN"	,"NaN"))
}) 

test_that("Function to check colum names", {
  expect_identical(colnames(opinion_of_rap_dummy), c("Question", "Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree"))
})

test_that("Function to test number of columns and rows", {
  expect_equal(nrow(opinion_of_rap_dummy),3)
  expect_equal(ncol(opinion_of_rap_dummy),6)
})

