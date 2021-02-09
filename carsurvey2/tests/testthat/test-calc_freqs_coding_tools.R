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

dummy_data <- data.frame(status_VBA = c("Access and knowledge","Access only","Access only","Access and knowledge",1),
                         status_SPSS = c("Access only",NA,1, "Access and knowledge", "No access or knowledge"),
                         status_other = c("No access or knowledge","Knowledge only","Access only", "Access only", "Access and knowledge"),
                         other = c(1,2,3,4,5))


coding_tool_access_knowledge_dummy  <- carsurvey2::calc_freqs_coding_tools(dummy_data,langs)



test_that("Function checks frequency of programming languages", {
  expect_identical(c(as.character(coding_tool_access_knowledge_dummy[1,1]),as.numeric(coding_tool_access_knowledge_dummy[1,2:4])), c("VBA",2,2,0))
  expect_identical(c(as.character(coding_tool_access_knowledge_dummy[2,1]),as.numeric(coding_tool_access_knowledge_dummy[2,2:4])), c("SPSS",1,1,0))
  expect_identical(c(as.character(coding_tool_access_knowledge_dummy[3,1]),as.numeric(coding_tool_access_knowledge_dummy[3,2:4])), c("other",2,1,1))
})

test_that("Function checks column headings" , { 
  expect_equal(colnames(coding_tool_access_knowledge_dummy[1]), "Programming language")
  expect_equal(colnames(coding_tool_access_knowledge_dummy[2]), "Access only")
  expect_equal(colnames(coding_tool_access_knowledge_dummy[3]), "Access and knowledge")
  expect_equal(colnames(coding_tool_access_knowledge_dummy[4]), "Knowledge only")
})

test_that("Function checks number of rows and columns", { 
  expect_equal(nrow(coding_tool_access_knowledge_dummy), 3)
  expect_equal(ncol(coding_tool_access_knowledge_dummy), 4)
})