test_that("function returns an error if API call unsuccessful", {
  expect_error(carsurvey2::ingest(token = "faketoken"))
})

test_that("function returns an error when the hashed data does not match check_hash", {
  expect_error(carsurvey2:ingest(check_hash = "fakehash"))
})