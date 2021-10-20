
dummy_data <- data.frame(gp_code_review = c("Regularly", "Sometimes"),
                        gp_version_control = c("All the time", "Rarely"),
                        gp_open_source = c("Never", "Regularly"),
                        gp_team_open_source = c("Never", "Never"),
                        doc_readme = c("Regularly", "Sometimes"),
                        doc_comments = c("All the time", "All the time"),
                        gp_function = c("Rarely", "Regularly"),
                        gp_unit_test = c("Rarely", "All the time"),
                        doc_func = c("Never", "All the time"),
                        gp_packages = c("All the time", "Regularly"),
                        gp_guidelines = c("Never", "Never"),
                        use_cont_integration = c("Yes", 0),
                        use_dependency_management = c(0, "Yes"))

comparison_data <- data.frame(peer_review_score = c(1, 0), 
                              version_control_score = c(1, 0),
                              use_open_source_score = c(0, 1),
                              open_code_score = c(0, 0),
                              doc_score = c(1, 0),
                              basic_rap_score = c(3, 1),
                              function_score = c(0, 1), 
                              test_score = c(0, 1),
                              function_doc_score = c(0, 1),
                              package_score = c(1, 1),
                              code_style_score = c(0, 0),
                              cont_integreation_score = c(1, 0),
                              dep_management_score = c(0, 1),
                              advanced_rap_score = c(2, 5))

test_that("function returns correct rap scores", {
  expect_identical(derive_rap_scores(dummy_data)[c(14:27)], comparison_data)
})