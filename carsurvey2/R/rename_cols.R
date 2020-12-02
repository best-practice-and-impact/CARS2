#'@title Rename columns
#'
#'@description Rename columns in CARS wave 2 dataset.
#'
#'@param data a data frame containing prepared CARS wave 2 data
#'
#'@return df with renamed columns
#'
#'@export

rename_cols <- function(data) {
  
  if (!is.data.frame(data)) {
    stop("Unexpected input - data should be a data frame")
  }
  
  tryCatch(
    {
      data <- dplyr::rename(data, 
                            dept = "Q1",
                            other_dept = "Q1.1",
                            grade = "Q2",
                            nonCS = "Q3",
                            GSG = "Q3.1",
                            GES = "Q3.2",
                            GSR = "Q3.3",
                            GORS = "Q3.4",
                            sci_eng = "Q3.5",
                            DDAT = "Q3.6",
                            GAD = "Q3.7",
                            finance = "Q3.8",
                            datasci_GSG = "Q3.9",
                            datasci_non = "Q3.10",
                            non_prof = "Q3.11",
                            other_prof = "Q3.12", 
                            location = "Q4",
                            produce_official_stats = "Q5",
                            
                            edu_level = "Q6",
                            maths = "Q7",
                            stats = "Q7.1",
                            systems = "Q7.2",
                            compsci = "Q7.3",
                            econ = "Q7.4",
                            psych = "Q7.5",
                            geog = "Q7.6",
                            socialsci = "Q7.7",
                            lifesci = "Q7.8",
                            physsci = "Q7.9",
                            engineer = "Q7.10",
                            business = "Q7.11",
                            health = "Q7.12",
                            law = "Q7.13",
                            history = "Q7.14",
                            lang_lit = "Q7.15",
                            other_subject = "Q7.16",
                            
                            code_freq = "Q8",
                            data_cleaning = "Q9",
                            data_analysis = "Q9.1",
                            data_vis = "Q9.2",
                            QA = "Q9.3",
                            data_transfer = "Q9.4",
                            other_ops = "Q9.5",
                            other_ops_comments = "Q9.6",
                            
                            knowledge_R = "Q10",
                            knowledge_SQL = "Q10.2",
                            knowledge_SAS = "Q10.4",
                            knowledge_VBA = "Q10.6",
                            knowledge_python = "Q10.8",
                            knowledge_SPSS = "Q10.10",
                            knowledge_stata = "Q10.12",
                            knowledge_JS = "Q10.14",
                            knowledge_java = "Q10.16",
                            knowledge_C = "Q10.18",
                            
                            available_R = "Q10.1",
                            available_SQL = "Q10.3",
                            available_SAS = "Q10.5",
                            available_VBA = "Q10.7",
                            available_python = "Q10.9",
                            available_SPSS = "Q10.11",
                            available_stata = "Q10.13",
                            available_JS = "Q10.15",
                            available_java = "Q10.17",
                            available_C = "Q10.19",
                            
                            other_tool = "Q10.20",
                            
                            code_experience = "Q11",
                            ability_change = "Q12",
                            learn_before = "Q13",
                            code_learn_where = "Q14",
                            
                            RAP_heard_of = "Q15",
                            RAP_champ = "Q16",
                            RAP_champ_known = "Q17",
                            RAP_understand = "Q18",
                            RAP_confident = "Q18.1",
                            RAP_important = "Q18.2",
                            RAP_supported = "Q18.3",
                            RAP_resources = "Q18.4",
                            RAP_using = "Q18.5",
                            comments_RAP = "Q18.6",
                            
                            gp_open_source = "Q19",
                            gp_dir_structure = "Q19.1",
                            gp_guidelines = "Q19.2",
                            gp_version_control = "Q19.3",
                            gp_code_review = "Q19.4",
                            gp_function = "Q19.5",
                            gp_packages = "Q19.6",
                            gp_unit_test = "Q19.7",
                            gp_auto_QA = "Q19.8",
                            gp_team_open_source = "Q19.9",
                            
                            doc_AQA_log = "Q20",
                            doc_assumption_reg = "Q20.1",
                            doc_func = "Q20.2",
                            doc_comments = "Q20.3",
                            doc_flow = "Q20.4",
                            doc_readme = "Q20.5",
                            doc_desk = "Q20.6",
                            doc_other = "Q20.7",
                            
                            use_cont_integration = "Q21",
                            use_dependency_management = "Q22",
                            use_reprod_workflow = "Q23",
                            comments_coding_practices ="Q24",
                            use_github = "Q25",
                            use_gitlab = "Q25.1",
                            use_bitbucket = "Q25.2",
                            use_AWS = "Q25.3",
                            use_googlecloud = "Q25.4",
                            use_other = "Q25.5",
                            
                            comments_coding_support = "Q26",
                            comments_survey = "Q27",
                            comments_other = "Q28")
    }, 
    error = function(e) {
      stop("Incorrect column names - use cleaned smartsurvey API data")
    }
  )
  
  return(data)
  
}
