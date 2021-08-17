set.seed(1)

dummy_data <- data.frame(
  outcome = as.factor(sample(c("a", "b"), 100, replace = TRUE)),
  predictor = c(runif(100, -10, 10))
)

test_output <- fit_log_reg("outcome ~ predictor", dummy_data)

test_that("fit_log_reg returns a glm object", {
  expect_s3_class(test_output, "glm")
})

dummy_data$predictor_2 <- c(runif(100, -10, 10))

test_output_multi_predictors <- fit_log_reg("outcome ~ predictor + predictor_2", dummy_data)

test_that("fit_log_reg returns a glm object when given multiple predictor variables", {
  expect_s3_class(test_output_multi_predictors, "glm")
})

test_that("fit_log_reg returns the correct coefficients", {
  expect_equal(test_output$coefficients, c("(Intercept)" = 0.036550486, "predictor" = 0.009969555), tolerance = 0.000001)
})
