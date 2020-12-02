# This script is designed to be used with the  project root as the working directory (../)

library(magrittr)

# Data ingest and basic cleaning

data <- carsurvey2::ingest() %>% 
  carsurvey2::convert_raw() %>%
  carsurvey2::tidy_ingest_data() %>%
  carsurvey2::rename_cols()

# Derive new variables

data <- carsurvey2::derive_rap_scores(data)
data <- carsurvey2::derive_code_status(data)

# Generate tables
source("table_pipeline.R")

save_navbar <- function(code, path) {
  filename <- paste(path, "_navbar.html", sep = "/")
  write(code, filename)
}

navbar_info <- carsurvey2::read_site_yml("rmarkdown/_site.yml")
navbar_page <- carsurvey2::build_navbar(navbar_info)
save_navbar(navbar_page, "rmarkdown")

knitr::opts_chunk$set(message = FALSE)
rmarkdown::clean_site("rmarkdown")
rmarkdown::render_site("rmarkdown")

