#' @title generate_tables
#' 
#' @description Creates a series of tables 
#' 
#' @details This is a high level function that is used to create a series of tables. The tables are defined in this package as a functions, all prefioxxed with table_.
#' This function owns some objects used in the table formatting functions e.g langs, these are passed into the functions. Each table created is added to a named list.
#' To add additional tables create a function in the table.R file, prefix with table_, add to this function and assign to the object tables.
#'
#' @param smart_survey_data This is generated using the carsurvey2::smart_survey_data_ functions.
#'
#' @return A named list
#' 
#' @export

generate_tables <- function(smart_survey_data) {
  
  # List of tables to be returned by the function
  # Add all generated tables to this list
  tables = list()
  
  ################################################
  # Objects to be used in  the table_ functions.
  
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
  
  code_prac_levels = c("I don't understand this question",
                       "Never",
                       "Rarely",
                       "Sometimes",
                       "Regularly",
                       "All the time")
  
  # End of objects
  ################################################
  # Start creating tables
  
  tables[["freq_table"]] <- carsurvey2::table_coding_frequency(smart_survey_data)
 
  tables[["knowledge"]] <- carsurvey2::table_knowledge_of_languages(smart_survey_data, langs)
  
  tables[["access"]] <- carsurvey2::table_access_to_programming_language(smart_survey_data, langs)
  
  tables[["code_tool_status"]] <- carsurvey2::table_coding_tool_access_knowledge(smart_survey_data, langs)
  
  tables[["rap_knowledge_chart"]] <- carsurvey2::table_knowledge_of_rap(smart_survey_data)
  
  tables[["rap_opinions_chart"]] <- carsurvey2::table_opinion_of_rap(smart_survey_data)

  tables[["components"]] <- carsurvey2::table_rap_score_components(smart_survey_data)
  
  tables[["basic_freqs"]] <- carsurvey2::table_rap_score_basic_frequencies(smart_survey_data)
  
  tables[["advanced_freqs"]] <- carsurvey2::table_rap_score_advanced_frequencies(smart_survey_data)
  
  tables[["code_prac_chart"]] <- carsurvey2::table_coding_practice_usage(smart_survey_data, code_prac_levels)
  
  tables[["doc"]] <- carsurvey2::table_documenation_usage(smart_survey_data, code_prac_levels)
  
  tables[["doc_chart"]] <- carsurvey2::table_documenation_usage_plotly_formatted(smart_survey_data, code_prac_levels)
  
  
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







