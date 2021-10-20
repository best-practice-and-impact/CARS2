
dummy_data <- data.frame(
  available_R = c("Yes"),  
  available_SQL = c("Yes"),
  available_SAS = c("No"), 
  available_VBA = c("No"),
  available_python = c("Don't know"),
  available_SPSS = c("Don't know"),
  available_stata = c("Yes"),
  available_JS = c("Yes"),
  available_java = c("No"),
  available_C = c("No"),
  
  knowledge_R = c("Yes"),      
  knowledge_SQL = c("No"),
  knowledge_SAS = c("Yes"), 
  knowledge_VBA = c("No"),
  knowledge_python = c("Yes"),
  knowledge_SPSS = c("No"),
  knowledge_stata = c("No"),
  knowledge_JS = c("Yes"),
  knowledge_java = c("Don't know"),
  knowledge_C = c("Don't know")
)

comparison_data <- data.frame(
  available_R = c("Yes"),  
  available_SQL = c("Yes"),
  available_SAS = c("No"), 
  available_VBA = c("No"),
  available_python = c("Don't know"),
  available_SPSS = c("Don't know"),
  available_stata = c("Yes"),
  available_JS = c("Yes"),
  available_java = c("No"),
  available_C = c("No"),
  
  knowledge_R = c("Yes"),      
  knowledge_SQL = c("No"),
  knowledge_SAS = c("Yes"), 
  knowledge_VBA = c("No"),
  knowledge_python = c("Yes"),
  knowledge_SPSS = c("No"),
  knowledge_stata = c("No"),
  knowledge_JS = c("Yes"),
  knowledge_java = c("Don't know"),
  knowledge_C = c("Don't know"),
  
  status_R = c("Access and knowledge"),
  status_SQL = c("Access only"),
  status_SAS = c("Knowledge only"),
  status_VBA = c("No access or knowledge"),
  status_python = c("Knowledge only"),
  status_SPSS = c("No access or knowledge"),
  status_stata = c("Access only"),
  status_JS = c("Access and knowledge"),
  status_java = c("No access or knowledge"),
  status_C = c("No access or knowledge")
)

test_that("function returns correct rap scores", {
  expect_identical(derive_code_status(dummy_data), comparison_data)
})