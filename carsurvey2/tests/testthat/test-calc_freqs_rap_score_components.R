dummy_data <-  data.frame(peer_review_score = c(1,0,0,0,0,0,2),
                          function_score = c(1,0,1,0,0,0,2),
                          other_score = c(1,1,1,0,0,0,0),
                          other = c(1,1,1,0,0,0,NA))

rap_score_components_dummy <- carsurvey2::calc_freqs_rap_score_components(dummy_data)

test_that("Function checks row names and their frequencies", {
  expect_identical(c(as.character(rownames(rap_score_components_dummy[1,])),as.numeric(rap_score_components_dummy[1,3])), c("peer_review_score",3))
  expect_identical(c(as.character(rownames(rap_score_components_dummy[2,])),as.numeric(rap_score_components_dummy[2,3])), c("function_score",4))
  expect_identical(c(as.character(rownames(rap_score_components_dummy[3,])),as.numeric(rap_score_components_dummy[3,3])), c("other_score",3))
})

test_that("Function checks Components and group", {
  expect_identical(c(as.character(rap_score_components_dummy[3,1]), as.character(rap_score_components_dummy[3,2])), c("other_score","Advanced"))
  expect_identical(c(as.character(rap_score_components_dummy[2,1]), as.character(rap_score_components_dummy[2,2])), c("Functions","Advanced"))
  expect_identical(c(as.character(rap_score_components_dummy[1,1]), as.character(rap_score_components_dummy[1,2])), c("Peer review","Basic"))
})

test_that("Function check column names", {
  expect_identical(colnames(rap_score_components_dummy), c("Component","Group","Count"))
})

test_that("Function checks length of rows and columns" ,{
  expect_equal(nrow(rap_score_components_dummy), 3)
  expect_equal(ncol(rap_score_components_dummy), 3)
})