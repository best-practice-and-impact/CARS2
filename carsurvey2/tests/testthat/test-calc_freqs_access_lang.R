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

dummy_data <-  data.frame(available_C = c("Yes", "Don't Know", 1, "Other"),
                          available_SQL = c("No", "Yes", 2, NA),
                          other_column = c(1,2,3,4))

expected_output <- data.frame("Programming language" = factor(c("C++ / C#", "SQL"), levels = c("C++ / C#", "SQL")),
                              "Yes" = as.integer(c(1, 1)), 
                              "Don't know" = as.integer(c(1, 0)),
                              "No" = as.integer(c(0, 1)))
colnames(expected_output) <- c("Programming language", "Yes", "Don't know", "No")

access_to_languages_dummy  <- carsurvey2::calc_freqs_access_lang(dummy_data,langs)
rownames(access_to_languages_dummy) <- NULL

test_that("Function checks frequency of programming languages", {
  expect_equal(access_to_languages_dummy, expected_output)
})
