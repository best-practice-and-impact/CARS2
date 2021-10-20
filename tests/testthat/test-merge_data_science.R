test_data <- data.frame(
  datasci_GSG = c(
    "Yes",
    "Yes",
    "No",
    "No",
    "Other"
  ),
  datasci_non = c(
      "Yes",
      "No",
      "No",
      "Yes",
      "Other"
    )
  )

test_that("Functions return a data frame",{
  expect_s3_class(carsurvey2::merge_data_scientist(test_data), "data.frame")
})

test_that("Function return correct values",{
  expect_equal(carsurvey2::merge_data_scientist(test_data)[["datasci"]],  c("Yes",
                                                                            "Yes",
                                                                            "No",
                                                                            "Yes",
                                                                            "No"))
})
