
# This script is designed to be used with the project root as the working directory (../)
library(magrittr)

# Data ingest and basic cleaning
# Derive new variables
# All data steps use function prefixed with data_

API_data <- carsurvey2::ingest()
if(API_data$status_code != 200) stop("Unsuccessful API request. Status code: ", API_data$status_code, "\n Process Killed.")

carsurvey_data <- carsurvey2::convert_raw(API_data)  %>% 
                  carsurvey2::tidy_ingest() %>% 
                  carsurvey2::rename_cols() %>%
                  carsurvey2::derive_rap_scores() %>%
                  carsurvey2::derive_code_status()      

# Build the site
navbar <- carsurvey2::render_navbar()
carsurvey2::save_navbar(navbar, "rmarkdown/main")
carsurvey2::save_navbar(navbar, "rmarkdown/summary_template")
carsurvey2::render_main_site(carsurvey_data)
carsurvey2::render_filtered_pages(carsurvey_data, filter_variable = "dept", page_title = "Department")
carsurvey2::render_filtered_pages(carsurvey_data, filter_variable = "grade", page_title = "Grade")
