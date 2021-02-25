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

dummy_data <-  data.frame(knowledge_R = c("Yes", "Don't Know", 1, "Other"),
                          knowledge_java = c("No", "Yes", 2, NA),
                          other_column = c(1,2,3,4))

access_to_languages_dummy  <- carsurvey2::calc_freqs_knowledge(dummy_data, langs)

expected_values <- data.frame("Programming language" = c("R", "Java / Scala"),
                              "Yes" = as.integer(c(1, 1)),
                              "Don't know" = as.integer(c(1, 0)),
                              "No" = as.integer(c(0, 1)))
colnames(expected_values) <- c("Programming language", "Yes", "Don't know", "No")

test_that("Output matches expected values", {
  expect_identical(access_to_languages_dummy, expected_values)
})
