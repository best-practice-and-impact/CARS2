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

dummy_data <- data.frame(status_VBA = c("Access and knowledge", "Access only", "Access only", "Access and knowledge", 1),
                         status_SPSS = c("Access only", NA, 1, "Access and knowledge", "No access or knowledge"),
                         status_other = c("No access or knowledge", "Knowledge only", "Access only", "Access only", "Access and knowledge"),
                         other = c(1, 2, 3, 4, 5))


calc_freqs_coding_tools  <- carsurvey2::calc_freqs_coding_tools(dummy_data,langs)
rownames(calc_freqs_coding_tools) <- NULL

expected_values <- data.frame("Programming language" = factor(c("other","SPSS","VBA"), levels = c("other","SPSS","VBA")),
                              "Access only" = as.integer(c(2, 1, 2)),
                              "Access and knowledge" = as.integer(c(1, 1, 2)),
                              "Knowledge only" = as.integer(c(1, 0, 0)))
colnames(expected_values) <- c("Programming language", "Access only", "Access and knowledge", "Knowledge only")

test_that("Output matches expected values", {
  expect_identical(calc_freqs_coding_tools, expected_values)
})
