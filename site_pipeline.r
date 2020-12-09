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

save_navbar <- function(code, path) {
  filename <- paste(path, "_navbar.html", sep = "/")
  write(code, filename)
}

# Create navigation bar
navbar_info <- carsurvey2::read_site_yml("rmarkdown/main/_site.yml")
navbar_page <- carsurvey2::build_navbar(navbar_info)
save_navbar(navbar_page, "rmarkdown/main")
save_navbar(navbar_page, "rmarkdown/deps")

# Get list of departments with sample >= 20
deps <- data.frame(table(data$dept))
dep_list <- deps[deps[2] >= 20, ]

# Generate tables
source("table_pipeline.R")

# Remove old site and knit
knitr::opts_chunk$set(message = FALSE, warning = FALSE)
rmarkdown::clean_site("rmarkdown/main")
rmarkdown::render_site("rmarkdown/main")

# Department profiles

original_data <- data

for (dep in dep_list[[1]]) {
  url <- paste0("../../docs/", dep)
  data <- original_data[original_data$dept == dep, ]
  source("table_pipeline.R")
  rmarkdown::render("rmarkdown/deps/template.rmd", output_file = url)
}

