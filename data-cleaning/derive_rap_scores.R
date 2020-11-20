# Calculate basic and advanced rap scores

high_vals <- c("Regularly", "All the time")

# Basic components
data$peer_review_score <- ifelse(data$gp_code_review %in% high_vals, 1, 0)
data$version_control_score <- ifelse(data$gp_version_control %in% high_vals, 1, 0)
data$use_open_source_score <- ifelse(data$gp_open_source %in% high_vals, 1, 0)
data$open_code_score <- ifelse(data$gp_team_open_source %in% high_vals, 1, 0)
data$doc_score <- ifelse(data$doc_readme %in% high_vals & data$doc_comments %in% high_vals, 1, 0)

data$basic_rap_score <- rowSums(data[,c("peer_review_score", 
                                     "version_control_score",
                                     "use_open_source_score",
                                     "open_code_score",
                                     "doc_score")])

# Advanced components
data$function_score <- ifelse(data$gp_function %in% high_vals, 1, 0)
data$test_score <- ifelse(data$gp_unit_test %in% high_vals, 1, 0)
data$function_doc_score <- ifelse(data$doc_func %in% high_vals, 1, 0)
data$package_score <- ifelse(data$gp_packages %in% high_vals, 1, 0)
data$code_style_score <- ifelse(data$gp_guidelines %in% high_vals, 1, 0)
data$cont_integreation_score <- ifelse(data$use_cont_integration == "Yes", 1, 0)
data$dep_management_score <- ifelse(data$use_dependency_management == "Yes", 1, 0)

data$advanced_rap_score <- rowSums(data[,c("function_score", 
                                           "test_score", 
                                           "function_doc_score", 
                                           "package_score", 
                                           "code_style_score", 
                                           "cont_integreation_score", 
                                           "dep_management_score")])