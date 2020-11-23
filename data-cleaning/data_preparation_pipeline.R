# This script is designed to be used with the  project root as the working directory (../)

library(magrittr)

# Data ingest and basic cleaning

data <- carsurvey2::ingest() %>% 
  carsurvey2::convert_raw() %>%
    carsurvey2::tidy_ingest_data() %>%
      carsurvey2::rename_cols()

# Derive new variables

data <- carsurvey2::derive_rap_scores(data)
