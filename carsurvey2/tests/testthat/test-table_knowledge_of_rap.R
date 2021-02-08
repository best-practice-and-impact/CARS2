library(testthat)

dummy_data <-  carsurvey2::data_rename_cols(carsurvey2::dummy_data)

set.seed(1)
dummy_data$RAP_heard_of <- sample(dummy_data$RAP_heard_of)

knowledge_of_rap_dummy <- carsurvey2::table_knowledge_of_rap(dummy_data)


test_that("Function checks frequency of RAP knowledge", {
  expect_identical(c(as.character(knowledge_of_rap_dummy[1,1]),as.numeric(knowledge_of_rap_dummy[1,2])), c("Have not heard of RAP",530))
  expect_identical(c(as.character(knowledge_of_rap_dummy[2,1]),as.numeric(knowledge_of_rap_dummy[2,2])), c("Heard of RAP, have not heard of RAP champions",140))
  expect_identical(c(as.character(knowledge_of_rap_dummy[3,1]),as.numeric(knowledge_of_rap_dummy[3,2])), c("Heard of RAP, does not know department champion",123))
  expect_identical(c(as.character(knowledge_of_rap_dummy[4,1]),as.numeric(knowledge_of_rap_dummy[4,2])), c("Heard of RAP champions, no champion in department",136))
  expect_identical(c(as.character(knowledge_of_rap_dummy[5,1]),as.numeric(knowledge_of_rap_dummy[5,2])), c("Knows department RAP champion",131))
})

test_that("Function checks column headings" , { 
  expect_equal(colnames(knowledge_of_rap_dummy[1]), "RAP knowledge")
  expect_equal(colnames(knowledge_of_rap_dummy[2]), "Count")
})

test_that("Function checks number of rows and columns", { 
  expect_equal(nrow(knowledge_of_rap_dummy), 5)
  expect_equal(ncol(knowledge_of_rap_dummy), 2)
})