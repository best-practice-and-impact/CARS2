dummy <- data.frame(Q6 = c("Bachelor's degree (or equivalent)", "other", "other", "Master's degree (or equivalent)", "other", "Doctoral degree (or equivalent)", "other"), 
                    Q7 = c("Yes","No","Yes","No","Yes","No","No"),
                    other = c("Yes","No","Yes","No","Yes","No","No"),
                    Q7.16 = c("Yes","No","Yes","No","Yes","No","No"),
                    Q8 = c("Never","All the time","Sometimes","Regularly","Rarely","Rarely","Never"),
                    Q13 = c("No","Yes","Yes","No","Yes","Yes","Yes"),
                    Q14 = c("test1","test2","test3","test4","test5","test6","test7"),
                    Q15 = c("No","Yes","Yes","No","Yes","Yes","Yes"),
                    Q17 = c("test1","test2","test3","test4","test5","test6","test7"),
                    Q16 = c("No","No","No","No","Yes","Yes","Yes"),
                    Q18.6 = c("test1","test2","test3","test4","test5","test6","test7"),
                    Q19 = c("test1","test2","Never","test4","test5","I don't understand this question","test7"),
                    Q24 = c("test1","test2","test3","test4","test5","test6","test7"),
                    Q25 = c("test1","test2","test3","test4","test5","test6","test7"),
                    Q25.5 = c("test1","test2","test3","test4","test5","test6","test7")
                    )

streaming_data <- carsurvey2::streaming(dummy)


test_that("Function to check Q6 streaming ", {
  expect_identical(as.character(streaming_data[1,2:4]), c("Yes","Yes","Yes"))
  expect_identical(as.logical(streaming_data[2,2:4]), c(NA,NA,NA)) 
  expect_identical(as.logical(streaming_data[3,2:4]), c(NA,NA,NA))
  expect_identical(as.character(streaming_data[4,2:4]), c("No","No","No"))
  expect_identical(as.logical(streaming_data[5,2:4]), c(NA,NA,NA))
  expect_identical(as.character(streaming_data[6,2:4]), c("No","No","No"))
  expect_identical(as.logical(streaming_data[7,2:4]), c(NA,NA,NA) )
})

test_that("Function to check Q8 streaming ", {
  expect_identical(as.logical(streaming_data[1,12:13]), c(NA,NA))
  expect_identical(as.character(streaming_data[2,12:13]), c("test2","test2")) 
  expect_identical(as.character(streaming_data[3,12:13]), c("Never","test3"))
  expect_identical(as.character(streaming_data[4,12:13]), c("test4","test4"))
  expect_identical(as.character(streaming_data[5,12:13]), c("test5","test5"))
  expect_identical(as.character(streaming_data[6,12:13]), c("I don't understand this question","test6"))
  expect_identical(as.logical(streaming_data[7,12:13]), c(NA,NA) )
})

test_that("Function to check Q13 streaming ", {
  expect_identical(as.logical(streaming_data[1,7]), NA)
  expect_identical(as.character(streaming_data[2,7]), "test2")
  expect_identical(as.character(streaming_data[3,7]), "test3")
  expect_identical(as.logical(streaming_data[4,7]), NA)
  expect_identical(as.character(streaming_data[5,7]), "test5")
  expect_identical(as.character(streaming_data[6,7]), "test6")
  expect_identical(as.character(streaming_data[7,7]), "test7" )
})

test_that("Function to check Q15 streaming ", {
  expect_identical(as.logical(streaming_data[1,10:11]), c(NA,NA))
  expect_identical(as.character(streaming_data[2,10:11]), c("No","test2")) 
  expect_identical(as.character(streaming_data[3,10:11]), c("No","test3"))
  expect_identical(as.logical(streaming_data[4,10:11]), c(NA,NA))
  expect_identical(as.character(streaming_data[5,10:11]), c("Yes","test5"))
  expect_identical(as.character(streaming_data[6,10:11]), c("Yes","test6"))
  expect_identical(as.character(streaming_data[7,10:11]), c("Yes","test7") )
})

test_that("Function to check Q17 streaming ", {
  expect_identical(as.logical(streaming_data[1,9]), NA)
  expect_identical(as.character(streaming_data[2,9]), "test2")
  expect_identical(as.character(streaming_data[3,9]), "test3")
  expect_identical(as.logical(streaming_data[4,9]), NA)
  expect_identical(as.logical(streaming_data[5,9]), NA)
  expect_identical(as.logical(streaming_data[6,9]), NA)
  expect_identical(as.logical(streaming_data[7,9]), NA)
})

test_that("Function to check Q19 streaming ", {
  expect_identical(as.character(streaming_data[1,14:15]), c("test1","test1"))
  expect_identical(as.character(streaming_data[2,14:15]), c("test2","test2")) 
  expect_identical(as.logical(streaming_data[3,14:15]), c(NA,NA))
  expect_identical(as.character(streaming_data[4,14:15]), c("test4","test4"))
  expect_identical(as.character(streaming_data[5,14:15]), c("test5","test5"))
  expect_identical(as.logical(streaming_data[6,14:15]), c(NA,NA))
  expect_identical(as.character(streaming_data[7,14:15]), c("test7","test7") )
})