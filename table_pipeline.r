# This script is designed to be used with the  project root as the working directory (../)

# Coding frequency table
data$code_freq <- factor(data$code_freq, levels = c("Never",
                                                    "Rarely",
                                                    "Sometimes",
                                                    "Regularly",
                                                    "All the time"))
freq_table <- data.frame(table(data$code_freq))
colnames(freq_table) <- c("Coding frequency", "Count")

# Programming tools

knowledge <- data[grepl("knowledge_", colnames(data))]
knowledge <- apply(knowledge, 2, table)
knowledge <- data.frame(apply(knowledge, 1, function(x) x))
knowledge$lang <- as.vector(stringr::str_split(rownames(knowledge), "_", simplify = TRUE)[,2])
knowledge <- knowledge[c(4, 3, 1, 2)]
colnames(knowledge) <- c("Programming language", "Yes", "Don't know", "No")

access <- data[grepl("available_", colnames(data))]
access <- apply(access, 2, table)
access <- data.frame(apply(access, 1, function(x) x))
access$lang <- as.vector(stringr::str_split(rownames(access), "_", simplify = TRUE)[,2])
access <- access[c(4, 3, 1, 2)]
colnames(access) <- c("Programming language", "Yes", "Don't know", "No")

code_tool_status <- data[grepl("status_", colnames(data))]
code_tool_status <- apply(code_tool_status, 2, table)
code_tool_status <- data.frame(apply(code_tool_status, 1, function(x) x))
code_tool_status$lang <- as.vector(stringr::str_split(rownames(code_tool_status), "_", simplify = TRUE)[,2])
code_tool_status <- code_tool_status[c(5, 2, 1, 3)]
colnames(code_tool_status) <- c("Programming language", "Access only", "Access and knowledge", "Knowledge only")

# RAP knowledge and opinions

data$RAP_champ_known[data$RAP_heard_of == "No"] <- "Have not heard of RAP"
rap_knowledge <- data.frame(table(data$RAP_champ_known))
rap_knowledge <- rap_knowledge[c(1, 2, 4, 3, 5), ]
colnames(rap_knowledge) <- c("RAP knowledge", "Count")
rap_knowledge_chart <- rap_knowledge
rap_knowledge[1] <- c("Have not heard of RAP",
                      "Heard of RAP, have not heard of RAP champions",
                      "Heard of RAP champions, no champion in department",
                      "Heard of RAP, does not know department champion",
                      "Knows department RAP champion")

rap_knowledge_chart[1] <- c("Have not heard of RAP",
                            "Heard of RAP, have not <br>heard of RAP champions",
                            "Heard of RAP champions, <br>no champion in department",
                            "Heard of RAP, does not <br>know department champion",
                            "Knows department RAP <br>champion")
rap_knowledge_chart$`RAP knowledge` <- as.factor(rap_knowledge_chart$`RAP knowledge`)

know_rap_data <- data[data$RAP_heard_of == "Yes", ]
know_rap_data <- dplyr::select(know_rap_data, RAP_understand:RAP_using)
rap_opinions <- data.frame(apply(know_rap_data, 2, table))
rap_opinions <- rap_opinions[c(5, 2, 3, 1, 4), ]
colnames(rap_opinions) <- c("I understand what the key components of the RAP methodology are",
                            "I feel confident implementing RAP in my work",
                            "I think it is important to implement RAP in my work",
                            "I feel supported to implement RAP in my work",
                            "I know where to find resources to help me implement RAP",
                            "I and/or my team are currently implementing RAP")

rap_opinions <- rap_opinions/nrow(know_rap_data)
rap_opinions <- data.frame(apply(rap_opinions, 1, function(x) x))
rap_opinions$Question <- rownames(rap_opinions)
rap_opinions <- rap_opinions[c(6, 1, 2, 3, 4, 5)]
colnames(rap_opinions) <- c("Question",
                            "Strongly disagree",
                            "Disagree",
                            "Neutral",
                            "Agree",
                            "Strongly agree")

rap_opinions_chart <- rap_opinions
rap_opinions_chart$Question <- c("I understand what the key components <br>of the RAP methodology are",
                            "I feel confident implementing RAP in <br>my work",
                            "I think it is important to implement <br>RAP in my work",
                            "I feel supported to implement RAP in <br>my work",
                            "I know where to find resources to help<br> me implement RAP",
                            "I and/or my team are currently <br>implementing RAP")

# Rap scores

rap_score_data <- data[grepl("_score", colnames(data))]

components_data <- rap_score_data[!colnames(rap_score_data) %in% c("basic_rap_score", "advanced_rap_score")]
components_data[is.na(components_data)] <- 0
components <- data.frame(colSums(components_data))
components <- data.frame(Component = rownames(components), Count = unname(components[1]))

basic_comps <-c(
  "peer_review_score",
  "version_control_score",
  "use_open_source_score",
  "doc_score",
  "open_code_score"
)
components$Group <- ifelse(components$Component %in% basic_comps, "Basic", "Advanced")
components$Component <- dplyr::recode(components$Component, 
              "peer_review_score" = "Peer review",
              "version_control_score" = "Version control",
              "use_open_source_score" = "Use open source software",
              "open_code_score" = "Team open source code",
              "doc_score" = "Documentation",
              "function_score" = "Functions",
              "test_score" = "Unit testing",
              "function_doc_score" = "Function documentation",
              "package_score" = "Code packages",
              "code_style_score" = "Follow code style guidelines",
              "cont_integreation_score" = "Continuous integration",
              "dep_management_score" = "Dependency management")
components <- dplyr::arrange(components, Group, Count)
components$Component <- factor(components$Component, levels = components$Component)
components <- components[c(1, 3, 2)]


basic_freqs <- data.frame(table(data$basic_rap_score))
colnames(basic_freqs) <- c("Basic RAP score", "Count")

advanced_freqs <- data.frame(table(data$advanced_rap_score))
colnames(advanced_freqs) <- c("Advanced RAP score", "Count")
carsurvey2::plot_freqs(advanced_freqs, "Basic RAP score", "Count", font_size = 14)


# Coding practices

code_prac_data <- data[grepl("gp_", colnames(data))]
code_prac_data <- code_prac_data[data$code_freq != "Never", ]
code_prac <- data.frame(apply(code_prac_data, 2, table))
code_prac <- data.frame(apply(code_prac, 1, function(x) x))
code_prac$Question <- rownames(code_prac)
colnames(code_prac) <- c("All the time",
                         "I don't understand this question",
                         "Never",
                         "Rarely",
                         "Regularly",
                         "Sometimes",
                         "Question")
code_prac <- code_prac[c(7, 2, 3, 4, 5, 6, 1)]
code_prac$Question <- dplyr::recode(code_prac$Question, 
                                    gp_open_source = "I use open source software when programming",
                                    gp_dir_structure = "I follow a standard directory structure when programming",
                                    gp_guidelines = "I follow coding guidelines or style guides when programming",
                                    gp_version_control = "I use a source code version control system e.g. Git",
                                    gp_code_review = "Code my team writes is reviewed by a colleague",
                                    gp_function = "I write repetitive elements in my code as functions",
                                    gp_packages = "I collect my code and supporting material into packages",
                                    gp_unit_test = "I unit test my code",
                                    gp_auto_QA = "I write code to automatically quality assure data",
                                    gp_team_open_source = "My team open sources its code")
code_prac[2:7] <- code_prac[2:7] / nrow(code_prac_data)                                    

code_prac_chart <- code_prac
code_prac_chart$Question <- dplyr::recode(code_prac$Question, 
                                          "I use open source software when programming" =  "I use open source software <br>when programming",
                                          "I follow a standard directory structure when programming" = "I follow a standard directory <br>structure when programming",
                                          "I follow coding guidelines or style guides when programming" = "I follow coding guidelines or <br>style guides when programming",
                                          "I use a source code version control system e.g. Git" = "I use a source code version <br>control system e.g. Git",
                                          "Code my team writes is reviewed by a colleague" = "Code my team writes is <br>reviewed by a colleague",
                                          "I write repetitive elements in my code as functions" = "I write repetitive elements <br>in my code as functions",
                                          "I collect my code and supporting material into packages" = "I collect my code and supporting <br>material into packages",
                                          "I unit test my code" = "I unit test my code",
                                          "I write code to automatically quality assure data" = "I write code to automatically <br>quality assure data",
                                          "My team open sources its code" = "My team open sources its <br>code")

# Doc

doc_data <- data[grepl("doc_", colnames(data))]
doc_data <- doc_data[data$code_freq != "Never", ]
doc_data <- doc_data[!colnames(doc_data) %in% c("doc_score", "function_doc_score", "doc_other")]
doc <- data.frame(apply(doc_data, 2, table))
doc <- data.frame(apply(doc, 1, function(x) x))
doc$Question <- rownames(doc)
colnames(doc) <- c("All the time",
                  "I don't understand this question",
                  "Never",
                  "Rarely",
                  "Regularly",
                  "Sometimes",
                  "Question")
doc <- doc[c(7, 2, 3, 4, 5, 6, 1)]
doc$Question <- dplyr::recode(doc$Question,
                              doc_AQA_log = "Analytical Quality Assurance (AQA) logs",
                              doc_assumption_reg = "Data or assumptions registers",
                              doc_func = "Documentation for each function or class",
                              doc_comments = "Code comments",
                              doc_flow = "Flow charts",
                              doc_readme = "README files",
                              doc_desk  = "Desk notes")
doc[2:7] <- doc[2:7] / nrow(doc_data)                                    

doc_chart <- doc
doc_chart$Question <- dplyr::recode(doc_chart$Question,
                                    "Analytical Quality Assurance (AQA) logs" = "Analytical Quality <br>Assurance (AQA) logs",
                                    "Data or assumptions registers" = "Data or assumptions <br>registers",
                                    "Documentation for each function or class" ="Documentation for each <br>function or class")
carsurvey2::plot_likert(doc_chart, 4, "Frequency", "Question", font_size = 14)
