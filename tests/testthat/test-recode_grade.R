grade_data <- data.frame(
  grade = c(
    "test nhs lower case", 
    "test NHS UPPER CASE", 
    "test NhS mixed case",
    "DSTL level 4",
    "l4", 
    "Level 5", 
    "l6",
    "level 7", 
    "Other",
    "Other"
  ), 
  dept = c(
    "NHS", 
    "NHS", 
    "NHS",
    "Defence Science and Technology Laboratory",
    "Defence Science and Technology Laboratory",
    "Defence Science and Technology Laboratory",
    "Defence Science and Technology Laboratory",
    "Defence Science and Technology Laboratory",
    "Defence Science and Technology Laboratory",
    "Other department"
  )
)

test_that("Function returns a data frame", {
  expect_s3_class(recode_grade(grade_data, grade_col = "grade", dep_col = "dept"), "data.frame")
})

test_that("Function returns the correct values", {
  expect_equal(recode_grade(grade_data, grade_col = "grade", dep_col = "dept")[["grade"]], c("Other - NHS",
                                                                                             "Other - NHS",
                                                                                             "Other - NHS",
                                                                                             "Higher Executive Officer (or equivalent)",
                                                                                             "Higher Executive Officer (or equivalent)",
                                                                                             "Senior Executive Officer (or equivalent)", 
                                                                                             "Grade 7 (or equivalent)",
                                                                                             "Grade 7 (or equivalent)",
                                                                                             "Other - DSTL",
                                                                                             "Other"))
})