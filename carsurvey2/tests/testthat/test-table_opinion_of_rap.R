library(testthat)

dummy_data <-  carsurvey2::data_rename_cols(carsurvey2::dummy_data)

opinion_of_rap_dummy <- carsurvey2::table_opinion_of_rap(dummy_data)

test_that("Function checks percentages of factors", {
  expect_identical(c(as.character(opinion_of_rap_dummy[1,1]),round(as.numeric(opinion_of_rap_dummy[1,2:6]),7)), c("I understand what the key components of the RAP methodology are",	0.2226415,	0.2075472,	0.1735849,	0.2169811,	0.1792453))
  expect_identical(c(as.character(opinion_of_rap_dummy[2,1]),round(as.numeric(opinion_of_rap_dummy[2,2:6]),7)), c("I feel confident implementing RAP in my work",	0.2226415	,0.2075472	,0.1735849	,0.2169811	,0.1792453))
  expect_identical(c(as.character(opinion_of_rap_dummy[3,1]),round(as.numeric(opinion_of_rap_dummy[3,2:6]),7)), c("I think it is important to implement RAP in my work",	0.2226415	,0.2075472	,0.1735849	,0.2169811	,0.1792453))
  expect_identical(c(as.character(opinion_of_rap_dummy[4,1]),round(as.numeric(opinion_of_rap_dummy[4,2:6]),7)), c("I feel supported to implement RAP in my work",	0.2226415	,0.2075472	,0.1735849	,0.2169811	,0.1792453))
  expect_identical(c(as.character(opinion_of_rap_dummy[5,1]),round(as.numeric(opinion_of_rap_dummy[5,2:6]),7)), c("I know where to find resources to help me implement RAP",	0.2226415	,0.2075472	,0.1735849	,0.2169811	,0.1792453))
  expect_identical(c(as.character(opinion_of_rap_dummy[6,1]),round(as.numeric(opinion_of_rap_dummy[6,2:6]),7)), c("I and/or my team are currently implementing RAP",	0.2226415	,0.2075472	,0.1735849	,0.2169811	,0.1792453))
})

test_that("Function to check colum names", {
  expect_identical(colnames(opinion_of_rap_dummy), c("Question", "Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree"))
})

test_that("Function to test number of columns and rows", {
  expect_equal(nrow(opinion_of_rap_dummy),6)
  expect_equal(ncol(opinion_of_rap_dummy),6)
})

