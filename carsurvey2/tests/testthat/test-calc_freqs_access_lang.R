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
                          other_column = c(1,2,3,4),
                          available_other = c("Yes","Yes",NA,"No") )

Access_to_languages_dummy  <- carsurvey2::calc_freqs_access_lang(dummy_data,langs)


test_that("Function checks frequency of programming languages", {
  expect_identical(c(as.character(Access_to_languages_dummy[1,1]),as.numeric(Access_to_languages_dummy[1,2:4])), c("C++ / C#",1,1,0))
  expect_identical(c(as.character(Access_to_languages_dummy[2,1]),as.numeric(Access_to_languages_dummy[2,2:4])), c("other",2,0,1))
  expect_identical(c(as.character(Access_to_languages_dummy[3,1]),as.numeric(Access_to_languages_dummy[3,2:4])), c("SQL",1,0,1))
})

test_that("Function checks column headings" , { 
  expect_equal(colnames(Access_to_languages_dummy[1]), "Programming language")
  expect_equal(colnames(Access_to_languages_dummy[2]), "Yes")
  expect_equal(colnames(Access_to_languages_dummy[3]), "Don't know")
  expect_equal(colnames(Access_to_languages_dummy[4]), "No")
})

test_that("Function checks number of rows", { 
  expect_equal(nrow(Access_to_languages_dummy), 3)
  expect_equal(ncol(Access_to_languages_dummy), 4)
})