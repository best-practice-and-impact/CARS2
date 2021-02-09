dummy_data <- data.frame(RAP_heard_of = c("Yes","Yes","Yes","Yes","No","Yes"),
                         RAP_champ_known = c("I know who the RAP champion in my department is","I know who the RAP champion in my department is","I know what a RAP champion is but don't know who the RAP champion in my department is","I don't know what a RAP champion is","I don't know what a RAP champion is",NA))

knowledge_of_rap_dummy <- carsurvey2::calc_freqs_knowledge_of_rap(dummy_data)


test_that("Function checks frequency of RAP knowledge", {
  expect_identical(c(as.character(knowledge_of_rap_dummy[1,1]),as.numeric(knowledge_of_rap_dummy[1,2])), c("Have not heard of RAP",1))
  expect_identical(c(as.character(knowledge_of_rap_dummy[2,1]),as.numeric(knowledge_of_rap_dummy[2,2])), c("Heard of RAP, have not heard of RAP champions",1))
  expect_identical(c(as.character(knowledge_of_rap_dummy[3,1]),as.numeric(knowledge_of_rap_dummy[3,2])), c("Heard of RAP, does not know department champion",1))
  expect_identical(c(as.character(knowledge_of_rap_dummy[4,1]),as.numeric(knowledge_of_rap_dummy[4,2])), c("Heard of RAP champions, no champion in department",0))
  expect_identical(c(as.character(knowledge_of_rap_dummy[5,1]),as.numeric(knowledge_of_rap_dummy[5,2])), c("Knows department RAP champion",2))
})

test_that("Function checks column headings" , { 
  expect_equal(colnames(knowledge_of_rap_dummy[1]), "RAP knowledge")
  expect_equal(colnames(knowledge_of_rap_dummy[2]), "Count")
})

test_that("Function checks number of rows and columns", { 
  expect_equal(nrow(knowledge_of_rap_dummy), 5)
  expect_equal(ncol(knowledge_of_rap_dummy), 2)
})