

#####   ##   #####  #      ######  ####   
  #    #  #  #    # #      #      #      
  #   #    # #####  #      #####   ####  
  #   ###### #    # #      #           # 
  #   #    # #    # #      #      #    # 
  #   #    # #####  ###### ######  #### 

###########################
# README
# Functions defined here are used in generate_tables()

# All functions are to take smart_survey_data as the first argument
# Any objects needed, such as a list of languages are to be defined in generate_tables() and passed into the function.

# The only output of the function in the generated table, no additional objects to be returned or shared with other functions.

###########################



#' @title table_coding_frequency
#'
#' @param smart_survey_data This is generated using the carsurvey2::smart_survey_data_ functions.
#'
#' @return data.frame
#' 
#' @export 
#'

table_coding_frequency <- function(smart_survey_data) {
  
  smart_survey_data$code_freq <- factor(smart_survey_data$code_freq, levels = c("Never",
                                                                                "Rarely",
                                                                                "Sometimes",
                                                                                "Regularly",
                                                                                "All the time"))
  freq_table <- data.frame(table(smart_survey_data$code_freq))
  
  colnames(freq_table) <- c("Coding frequency", "Count")
  
  return(freq_table)
  
}

#' @title table_knowledge_of_languages
#'
#' @param smart_survey_data This is generated using the carsurvey2::smart_survey_data_ functions.
#' 
#' @param langs Formatted list of strings. langs object is available in carsurvey2::generate_tables()
#'
#' @return data.frame
#' @export

table_knowledge_of_languages <- function(smart_survey_data, langs) {
  
  knowledge <- smart_survey_data[grepl("knowledge_", colnames(smart_survey_data))]
  knowledge <- carsurvey2::calc_multi_col_freqs(cols = knowledge, factor_levels = c("Yes", "Don't Know", "No"))
  colnames(knowledge) <- c("Programming language", "Yes", "Don't know", "No")
  knowledge[[1]] <- stringr::str_split(knowledge[[1]], "_", simplify = TRUE)[,2 ]%>% dplyr::recode(!!!langs) # Rename questions
  
  return(knowledge)
  
}



#' @title table_access_to_programming_language
#'
#' @param smart_survey_data This is generated using the carsurvey2::smart_survey_data_ functions.
#' 
#' @param langs Formatted list of strings. langs object is available in carsurvey2::generate_tables()
#'
#' @return data.frame
#' @export

table_access_to_programming_language <- function(smart_survey_data, langs) {
  
  access <- smart_survey_data[grepl("available_", colnames(smart_survey_data))]
  access <- carsurvey2::calc_multi_col_freqs(cols = access, factor_levels = c("Yes", "Don't Know", "No"))
  colnames(access) <- c("Programming language", "Yes", "Don't know", "No")
  access[[1]] <- stringr::str_split(access[[1]], "_", simplify = TRUE)[,2] %>% dplyr::recode(!!!langs) # Rename questions
  
  return(access)
  
}

#' @title table_coding_tool_access_knowledge
#'
#' @param smart_survey_data This is generated using the carsurvey2::smart_survey_data_ functions.
#' 
#' @param langs Formatted list of strings. langs object is available in carsurvey2::generate_tables()
#'
#' @return data.frame
#' @export

table_coding_tool_access_knowledge <- function(smart_survey_data, langs) {
  
  code_tool_status <- smart_survey_data[grepl("status_", colnames(smart_survey_data))]
  code_tool_status <- carsurvey2::calc_multi_col_freqs(cols = code_tool_status, factor_levels = c("Access only", "Access and knowledge", "Knowledge only"))
  colnames(code_tool_status) <- c("Programming language", "Access only", "Access and knowledge", "Knowledge only") 
  code_tool_status[[1]] <- stringr::str_split(code_tool_status[[1]], "_", simplify = TRUE)[,2] %>% dplyr::recode(!!!langs) # Rename questions
  
  return(code_tool_status)
}


#' @title table_knowledge_of_rap
#'
#' @param smart_survey_data This is generated using the carsurvey2::smart_survey_data_ functions.
#'
#' @return data.frame
#' @export


table_knowledge_of_rap <- function(smart_survey_data) {
  
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
 
  return(rap_knowledge_chart)
}



#' @title table_opinion_of_rap 
#'
#' @param smart_survey_data This is generated using the carsurvey2::smart_survey_data_ functions.
#'
#' @return data.frame
#' @export

table_opinion_of_rap <- function(smart_survey_data) {
  
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
 
  return(rap_opinions_chart)
   
}


#' @title table_rap_score_components
#'
#' @param smart_survey_data This is generated using the carsurvey2::smart_survey_data_ functions.
#'
#' @return data.frame
#' @export

table_rap_score_components <- function(smart_survey_data) {
  
  rap_score <- smart_survey_data[grepl("_score", colnames(smart_survey_data))]
  
  components <- rap_score[!colnames(rap_score) %in% c("basic_rap_score", "advanced_rap_score")]
  components[is.na(components)] <- 0
  components <- data.frame(colSums(components))
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
  
  return(components)
  
}


#' @title table_rap_score_basic_frequencies
#'
#' @param smart_survey_data This is generated using the carsurvey2::smart_survey_data_ functions.
#'
#' @return data.frame
#' @export

table_rap_score_basic_frequencies <- function(smart_survey_data) {
  
  basic_freqs <- data.frame(table(smart_survey_data$basic_rap_score))
  colnames(basic_freqs) <- c("Basic RAP score", "Count")
  
  return(basic_freqs)
}

#' @title table_rap_score_advanced_frequencies
#'
#' @param smart_survey_data This is generated using the carsurvey2::smart_survey_data_ functions.
#'
#' @return data.frame
#' @export

table_rap_score_advanced_frequencies <- function(smart_survey_data) {
  
  advanced_freqs <- data.frame(table(smart_survey_data$advanced_rap_score))
  colnames(advanced_freqs) <- c("Advanced RAP score", "Count")
  
  return(advanced_freqs)
  
}

#' @title table_coding_practice_usage
#'
#' @param smart_survey_data This is generated using the carsurvey2::smart_survey_data_ functions.
#' @param code_prac_levels A vector of strings, length 6
#'
#' @return data.frame
#' @export

table_coding_practice_usage <- function(smart_survey_data, code_prac_levels) {
  
  code_prac_chart <- carsurvey2::coding_practices(smart_survey_data, code_prac_levels)
                                    
  code_prac_chart[[1]] <- carsurvey2::break_q_names(code_prac_chart[[1]], 2)
  
  return(code_prac_chart)
}

#' @title table_documenation_usage
#'
#' @param smart_survey_data This is generated using the carsurvey2::smart_survey_data_ functions.
#' @param code_prac_levels A vector of strings, length 6
#' 
#' @return data.frame
#' @export

table_documenation_usage <- function(smart_survey_data, code_prac_levels) {
  
  code_prac = carsurvey2::coding_practices(smart_survey_data, code_prac_levels)
  
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
  
  return(doc)
}


#' @title table_documenation_usage_plotly_formatted
#'
#' @param smart_survey_data This is generated using the carsurvey2::smart_survey_data_ functions.
#' @param code_prac_levels A vector of strings, length 6
#'
#' @return data.frame
#' @export

format_plotly_documenation_usage <- function(smart_survey_data, code_prac_levels) {
  
  table_documenation_usage_df = table_documenation_usage(smart_survey_data, code_prac_levels)
  
  table_documenation_usage_df[[1]] <- break_q_names(table_documenation_usage_df[[1]], 2)
  
  return(table_documenation_usage_df)
}


#' @title coding_practices
#' 
#' @description Used for genertaing more than 1 table = table_coding_practice_usage, table_documenation_usage
#'
#' @param smart_survey_data This is generated using the carsurvey2::smart_survey_data_ functions.
#' @param code_prac_levels A vector of strings, length 6
#'
#' @return data.frame
#' @export

coding_practices <- function(smart_survey_data, code_prac_levels) {
  
  code_prac_smart_survey_data <- smart_survey_data[grepl("gp_", colnames(smart_survey_data))]
  code_prac_smart_survey_data <- code_prac_smart_survey_data[smart_survey_data$code_freq != "Never", ]
  
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
  
  return(code_prac)
}



