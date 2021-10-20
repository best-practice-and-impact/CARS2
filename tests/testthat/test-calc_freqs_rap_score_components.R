dummy_data <-  data.frame(peer_review_score = c(1, 0, 0, 0, 0, 0, 2),
                          function_score = c(1, 0, 1, 0, 0, 0, 2),
                          other_score = c(1, 1, 1, 0, 0, 0, 0),
                          other = c(1, 1, 1, 0, 0, 0, NA))

rap_score_components_dummy <- carsurvey2::calc_freqs_rap_score_components(dummy_data)

expected_output <- data.frame(Component = factor(c("Peer review", "Functions", "other_score"), levels = c("Peer review", "Functions", "other_score")),
                              Type = c("Basic", "Advanced", "Advanced"),
                              Count = c(sum(dummy_data$peer_review_score),
                                        sum(dummy_data$function_score),
                                        sum(dummy_data$other_score)))

test_that("Dummy values match expected values", {
  expect_identical(rap_score_components_dummy, expected_output)
})
