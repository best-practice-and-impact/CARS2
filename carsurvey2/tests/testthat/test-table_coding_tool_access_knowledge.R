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

dummy_data <-  carsurvey2::data_rename_cols(carsurvey2::dummy_data)

#Set seed
set.seed(1)

#Shuffle responses
dummy_data$knowledge_SAS <- sample(dummy_data$knowledge_SAS)
dummy_data$knowledge_R <- sample(dummy_data$knowledge_R)
dummy_data$knowledge_SQL <- sample(dummy_data$knowledge_SQL)
dummy_data$knowledge_VBA <- sample(dummy_data$knowledge_VBA)
dummy_data$knowledge_python <- sample(dummy_data$knowledge_python)
dummy_data$knowledge_SPSS <- sample(dummy_data$knowledge_SPSS)
dummy_data$knowledge_stata <- sample(dummy_data$knowledge_stata)
dummy_data$knowledge_JS <- sample(dummy_data$knowledge_JS)
dummy_data$knowledge_java <- sample(dummy_data$knowledge_java)
dummy_data$knowledge_C <- sample(dummy_data$knowledge_C)


dummy_data <-  carsurvey2::data_derive_code_status(dummy_data) 


coding_tool_access_knowledge_dummy  <- carsurvey2::table_coding_tool_access_knowledge(dummy_data,langs)



test_that("Function checks frequency of programming languages", {
  expect_identical(c(as.character(coding_tool_access_knowledge_dummy[1,1]),as.numeric(coding_tool_access_knowledge_dummy[1,2:4])), c("R",229,103,229))
  expect_identical(c(as.character(coding_tool_access_knowledge_dummy[2,1]),as.numeric(coding_tool_access_knowledge_dummy[2,2:4])), c("SQL",232,100,232))
  expect_identical(c(as.character(coding_tool_access_knowledge_dummy[3,1]),as.numeric(coding_tool_access_knowledge_dummy[3,2:4])), c("SAS",224,108,224))
  expect_identical(c(as.character(coding_tool_access_knowledge_dummy[4,1]),as.numeric(coding_tool_access_knowledge_dummy[4,2:4])), c("VBA",225,107,225))
  expect_identical(c(as.character(coding_tool_access_knowledge_dummy[5,1]),as.numeric(coding_tool_access_knowledge_dummy[5,2:4])), c("Python",230,102,230))
  expect_identical(c(as.character(coding_tool_access_knowledge_dummy[6,1]),as.numeric(coding_tool_access_knowledge_dummy[6,2:4])), c("SPSS",228,104,228))
  expect_identical(c(as.character(coding_tool_access_knowledge_dummy[7,1]),as.numeric(coding_tool_access_knowledge_dummy[7,2:4])), c("Stata",212,120,212))
  expect_identical(c(as.character(coding_tool_access_knowledge_dummy[8,1]),as.numeric(coding_tool_access_knowledge_dummy[8,2:4])), c("javascript / Typescript",229,103,229))
  expect_identical(c(as.character(coding_tool_access_knowledge_dummy[9,1]),as.numeric(coding_tool_access_knowledge_dummy[9,2:4])), c("Java / Scala",235,97,235))
  expect_identical(c(as.character(coding_tool_access_knowledge_dummy[10,1]),as.numeric(coding_tool_access_knowledge_dummy[10,2:4])), c("C++ / C#",223,109,223))
})

test_that("Function checks column headings" , { 
  expect_equal(colnames(coding_tool_access_knowledge_dummy[1]), "Programming language")
  expect_equal(colnames(coding_tool_access_knowledge_dummy[2]), "Access only")
  expect_equal(colnames(coding_tool_access_knowledge_dummy[3]), "Access and knowledge")
  expect_equal(colnames(coding_tool_access_knowledge_dummy[4]), "Knowledge only")
})

test_that("Function checks number of rows and columns", { 
  expect_equal(nrow(coding_tool_access_knowledge_dummy), 10)
  expect_equal(ncol(coding_tool_access_knowledge_dummy), 4)
})