
# This script is designed to be used with the project root as the working directory (../)
library(magrittr)

carsurvey_data <- carsurvey2::preprocess()

# Build the site
navbar <- carsurvey2::render_navbar()
carsurvey2::save_navbar(navbar, "rmarkdown/main")
carsurvey2::save_navbar(navbar, "rmarkdown/summary_template")
carsurvey2::render_main_site(carsurvey_data)
carsurvey2::render_filtered_pages(carsurvey_data, filter_variable = "dept", page_title = "Department")
carsurvey2::render_filtered_pages(carsurvey_data, filter_variable = "grade", page_title = "Grade")
carsurvey2::render_prof_pages(carsurvey_data, page_title = "Profession")