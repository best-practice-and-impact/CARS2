#' @title generate_tables
#' 
#' @description 
#'
#' @param smart_survey_data This is generated using the carsurvey2::smart_survey_data_ functions.
#'
#' @return
#' 
#' @export

generate_tables <- function(smart_survey_data) {
  
  # List of tables to be returned by the function
  # Add all generated tables to this list
  tables = list()
  
  ############################
  # Coding frequency table
  smart_survey_data$code_freq <- factor(smart_survey_data$code_freq, levels = c("Never",
                                                      "Rarely",
                                                      "Sometimes",
                                                      "Regularly",
                                                      "All the time"))
  freq_table <- data.frame(table(smart_survey_data$code_freq))
  colnames(freq_table) <- c("Coding frequency", "Count")
  tables[["freq_table"]] <- freq_table
  
  # Programming tools
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
  
  ###################
  # knowledge table
  knowledge <- smart_survey_data[grepl("knowledge_", colnames(smart_survey_data))]
  knowledge <- carsurvey2::calc_multi_col_freqs(cols = knowledge, factor_levels = c("Yes", "Don't Know", "No"))
  colnames(knowledge) <- c("Programming language", "Yes", "Don't know", "No")
  knowledge[[1]] <- stringr::str_split(knowledge[[1]], "_", simplify = TRUE)[,2 ]%>% dplyr::recode(!!!langs) # Rename questions
  tables[["knowledge"]] <- knowledge
  
  #################
  # Access table
  access <- smart_survey_data[grepl("available_", colnames(smart_survey_data))]
  access <- carsurvey2::calc_multi_col_freqs(cols = access, factor_levels = c("Yes", "Don't Know", "No"))
  colnames(access) <- c("Programming language", "Yes", "Don't know", "No")
  access[[1]] <- stringr::str_split(access[[1]], "_", simplify = TRUE)[,2] %>% dplyr::recode(!!!langs) # Rename questions
  tables[["access"]] <- access
  
  ###########################
  # code_tool_status table
  code_tool_status <- smart_survey_data[grepl("status_", colnames(smart_survey_data))]
  code_tool_status <- carsurvey2::calc_multi_col_freqs(cols = code_tool_status, factor_levels = c("Access only", "Access and knowledge", "Knowledge only"))
  colnames(code_tool_status) <- c("Programming language", "Access only", "Access and knowledge", "Knowledge only") 
  code_tool_status[[1]] <- stringr::str_split(code_tool_status[[1]], "_", simplify = TRUE)[,2] %>% dplyr::recode(!!!langs) # Rename questions
  tables[["code_tool_status"]] <- code_tool_status
  
  ############################
  # RAP knowledge table
  smart_survey_data$RAP_champ_known[smart_survey_data$RAP_heard_of == "No"] <- "Have not heard of RAP"
  smart_survey_data$RAP_champ_known <- factor(smart_survey_data$RAP_champ_known, levels = c(
    "Have not heard of RAP",                                     
    "I don't know what a RAP champion is",                          
    "I know what a RAP champion is but don't know who the RAP champion in my department is",
    "I know what a RAP champion is and there is no RAP champion in my department",
    "I know who the RAP champion in my department is"
  ))
  rap_knowledge <- data.frame(table(smart_survey_data$RAP_champ_known))
  colnames(rap_knowledge) <- c("RAP knowledge", "Count")
  rap_knowledge[1] <- c("Have not heard of RAP",
                        "Heard of RAP, have not heard of RAP champions",
                        "Heard of RAP, does not know department champion",
                        "Heard of RAP champions, no champion in department",
                        "Knows department RAP champion")
  rap_knowledge_chart <- rap_knowledge
  rap_knowledge_chart[[1]] <- carsurvey2::break_q_names(rap_knowledge_chart[[1]], 2)
  rap_knowledge_chart[[1]] <- factor(rap_knowledge_chart[[1]], levels = rap_knowledge_chart[[1]])
  tables[["rap_knowledge_chart"]] <- rap_knowledge_chart
  
  #################################
  # rap_opinions_chart table
  know_rap_smart_survey_data <- smart_survey_data[smart_survey_data$RAP_heard_of == "Yes", ]
  know_rap_smart_survey_data <- dplyr::select(know_rap_smart_survey_data, RAP_understand:RAP_using)
  know_rap_levels <- c("Strongly Disagree",
                       "Disagree",
                       "Neutral",
                       "Agree",
                       "Strongly Agree")
  rap_opinions <- carsurvey2::calc_multi_col_freqs(know_rap_smart_survey_data, know_rap_levels, calc_props=TRUE)
  new_colnames <- c(RAP_understand = "I understand what the key components of the RAP methodology are",
                    RAP_confident = "I feel confident implementing RAP in my work",
                    RAP_important = "I think it is important to implement RAP in my work",
                    RAP_supported = "I feel supported to implement RAP in my work",
                    RAP_resources = "I know where to find resources to help me implement RAP",
                    RAP_using = "I and/or my team are currently implementing RAP")
  
  rap_opinions[[1]] <- dplyr::recode(rap_opinions[[1]], !!!new_colnames) 
  colnames(rap_opinions) <- c("Question",
                              "Strongly disagree",
                              "Disagree",
                              "Neutral",
                              "Agree",
                              "Strongly agree")
  rap_opinions_chart <- rap_opinions
  rap_opinions_chart[[1]] <- carsurvey2::break_q_names(rap_opinions_chart[[1]], 2)
  tables[["rap_opinions_chart"]] <- rap_opinions_chart
  
  #######################
  # Rap scores
  # components tables
  rap_score_smart_survey_data <- smart_survey_data[grepl("_score", colnames(smart_survey_data))]
  
  components_smart_survey_data <- rap_score_smart_survey_data[!colnames(rap_score_smart_survey_data) %in% c("basic_rap_score", "advanced_rap_score")]
  components_smart_survey_data[is.na(components_smart_survey_data)] <- 0
  components <- data.frame(colSums(components_smart_survey_data))
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
  tables[["components"]] <- components
  

  ##############################
  # basic_freqs tables
  basic_freqs <- data.frame(table(smart_survey_data$basic_rap_score))
  colnames(basic_freqs) <- c("Basic RAP score", "Count")
  tables[["basic_freqs"]] <- basic_freqs
  
  
  ##############################
  # advanced_freqs tables
  advanced_freqs <- data.frame(table(smart_survey_data$advanced_rap_score))
  colnames(advanced_freqs) <- c("Advanced RAP score", "Count")
  tables[["advanced_freqs"]] <- advanced_freqs
  
  
  ##################################
  # Coding practices
  # code_prac_chart tables
  code_prac_smart_survey_data <- smart_survey_data[grepl("gp_", colnames(smart_survey_data))]
  code_prac_smart_survey_data <- code_prac_smart_survey_data[smart_survey_data$code_freq != "Never", ]
  code_prac_levels = c("I don't understand this question",
                       "Never",
                       "Rarely",
                       "Sometimes",
                       "Regularly",
                       "All the time")
  
  code_prac <- carsurvey2::calc_multi_col_freqs(code_prac_smart_survey_data, code_prac_levels, calc_props = TRUE)
  
  colnames(code_prac)[c(2:length(code_prac))] <- code_prac_levels
  code_prac_questions <- c(
    gp_open_source = "I use open source software when programming",
    gp_dir_structure = "I follow a standard directory structure when programming",
    gp_guidelines = "I follow coding guidelines or style guides when programming",
    gp_version_control = "I use a source code version control system e.g. Git",
    gp_code_review = "Code my team writes is reviewed by a colleague",
    gp_function = "I write repetitive elements in my code as functions",
    gp_packages = "I collect my code and supporting material into packages",
    gp_unit_test = "I unit test my code",
    gp_auto_QA = "I write code to automatically quality assure smart_survey_data",
    gp_team_open_source = "My team open sources its code"
  )
  
  code_prac[[1]] <- dplyr::recode(code_prac[[1]], !!!code_prac_questions) 
  code_prac_chart <- code_prac
  code_prac_chart[[1]] <- carsurvey2::break_q_names(code_prac_chart[[1]], 2)
  tables[["code_prac_chart"]] <- code_prac_chart
  
  
  ################################
  # doc_chart tables
  doc_smart_survey_data <- dplyr::select(smart_survey_data, doc_AQA_log:doc_desk)
  doc_smart_survey_data <- doc_smart_survey_data[smart_survey_data$code_freq != "Never", ]
  
  doc <- carsurvey2::calc_multi_col_freqs(doc_smart_survey_data, code_prac_levels, calc_props = TRUE)
  colnames(code_prac)[c(2:length(code_prac))] <- code_prac_levels
  
  doc_questions <- c(doc_AQA_log = "Analytical Quality Assurance (AQA) logs",
                     doc_assumption_reg = "smart_survey_data or assumptions registers",
                     doc_func = "Documentation for each function or class",
                     doc_comments = "Code comments",
                     doc_flow = "Flow charts",
                     doc_readme = "README files",
                     doc_desk  = "Desk notes")
  
  doc[[1]] <- dplyr::recode(doc[[1]], !!!doc_questions)
  
  # Make doc_chart
  doc_chart <- doc
  doc_chart[[1]] <- carsurvey2::break_q_names(doc_chart[[1]], 2)
  
  # Similar but with formatted strings for plotly 
  tables[["doc"]] <- doc
  tables[["doc_chart"]] <- doc_chart
  
  ############################
  # Error handling
  # Check tables struct 

  for (table in names(tables)) {
    
    # Check that only smart_survey_dataframes have be stored
    if(!class(tables[[table]]) == "data.frame") stop("Tables contains a non data.frame. /n Check the contents of the tables list ")
    
    # Check that tables contains rows
    if(nrow(tables[[table]]) == 0) stop(table, " is empty")
    
  } 
  
  return(tables)
  
}







