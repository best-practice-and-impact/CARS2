dummy_response <- list(status_code = 200,
                       content = charToRaw('columnA, columnB, columnC \n \"1\", \"-\" , \"3\" \n \"4\", \"5\", \"6\"'))

class(dummy_response) <- "response"

dummy_failed_response <- dummy_response
dummy_failed_response$status_code <- 401


dummy_df <-  data.frame(columnA = c("1", "4"),
                        columnB = c(NA, "5"),
                        columnC = c("3", "6"))


test_that("function returns data.frame from raw character string", {
  expect_s3_class(convert_raw(dummy_response), "data.frame")
})

test_that("function returns correct column headers and rows", {
  expect_identical(colnames(convert_raw(dummy_response)), colnames(dummy_df))
})

test_that("function rejects unsuccessful API responses", {
  expect_error(convert_raw(dummy_failed_response))
})