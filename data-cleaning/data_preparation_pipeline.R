# This script is designed to be used with the  project root as the working directory (../)

library(magrittr)

# Data ingest and basic cleaning

data <- carsurvey2::ingest() %>% 
  carsurvey2::convert_raw() %>%
    carsurvey2::tidy_ingest_data()

source("data-cleaning/column_renaming.R")

# Derive new variables

source("data-cleaning/derive_rap_scores.R")