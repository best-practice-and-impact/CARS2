library(testthat)

langs <- c(
  C = "C++ / C#",
  java = "Java / Scala",	
  JS = "javascript / Typescript",
  python = "Python",
  R = "R",
  SAS = "SAS",	
  SPSS = "SPSS",	
  SQL = "SQL",	
  stata = "Stata",	
  VBA = "VBA"
)

dummy_data <- carsurvey2::data_rename_cols(carsurvey2::dummy_data) %>%
              carsurvey2::data_derive_code_status()   #Maybe remove this bit

knowledge_of_languages_dummy <- carsurvey2::table_knowledge_of_languages(dummy_data,langs)

test_that("Function checks frequency of programming languages", {
  expect_identical(c(as.character(knowledge_of_languages_dummy[1,1]),as.numeric(knowledge_of_languages_dummy[1,2:4])), c("R",332,371,357))
  expect_identical(c(as.character(knowledge_of_languages_dummy[2,1]),as.numeric(knowledge_of_languages_dummy[2,2:4])), c("SQL",332,371,357))
  expect_identical(c(as.character(knowledge_of_languages_dummy[3,1]),as.numeric(knowledge_of_languages_dummy[3,2:4])), c("SAS",332,371,357))
  expect_identical(c(as.character(knowledge_of_languages_dummy[4,1]),as.numeric(knowledge_of_languages_dummy[4,2:4])), c("VBA",332,371,357))
  expect_identical(c(as.character(knowledge_of_languages_dummy[5,1]),as.numeric(knowledge_of_languages_dummy[5,2:4])), c("Python",332,371,357))
  expect_identical(c(as.character(knowledge_of_languages_dummy[6,1]),as.numeric(knowledge_of_languages_dummy[6,2:4])), c("SPSS",332,371,357))
  expect_identical(c(as.character(knowledge_of_languages_dummy[7,1]),as.numeric(knowledge_of_languages_dummy[7,2:4])), c("Stata",332,371,357))
  expect_identical(c(as.character(knowledge_of_languages_dummy[8,1]),as.numeric(knowledge_of_languages_dummy[8,2:4])), c("javascript / Typescript",332,371,357))
  expect_identical(c(as.character(knowledge_of_languages_dummy[9,1]),as.numeric(knowledge_of_languages_dummy[9,2:4])), c("Java / Scala",332,371,357))
  expect_identical(c(as.character(knowledge_of_languages_dummy[10,1]),as.numeric(knowledge_of_languages_dummy[10,2:4])), c("C++ / C#",332,371,357))
})

test_that("Function checks column headings" , { 
  expect_equal(colnames(knowledge_of_languages_dummy[1]), "Programming language")
  expect_equal(colnames(knowledge_of_languages_dummy[2]), "Yes")
  expect_equal(colnames(knowledge_of_languages_dummy[3]), "Don't know")
  expect_equal(colnames(knowledge_of_languages_dummy[4]), "No")
})

test_that("Function checks number of rows", { 
  expect_equal(nrow(knowledge_of_languages_dummy), 10)
})