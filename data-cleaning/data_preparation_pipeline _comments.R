# This script is designed to be used with the  project root as the working directory (../)

library(magrittr)

# Data ingest and basic cleaning

data <- carsurvey2::ingest() %>% 
  carsurvey2::convert_raw() %>%
    carsurvey2::tidy_ingest_data() %>%
      carsurvey2::rename_cols()

# Derive new variables

data <- carsurvey2::derive_rap_scores(data)

selected_cols <- data[c("dept",
                        "comments_RAP",
                        "comments_coding_practices",
                        "comments_coding_support",
                        "comments_survey",
                        "comments_other")]

# Remove responses with no free text
mask_df <- data[c("comments_RAP",
                  "comments_coding_practices",
                  "comments_coding_support",
                  "comments_survey",
                  "comments_other")]

mask <- !apply(is.na(mask_df), 1, all)

filtered_data <- selected_cols[mask, ]

# Recode small departments (n < 5)

dep_counts <- data.frame(table(filtered_data$dept))
small_counts <- dep_counts[dep_counts$Freq < 5, ]

filtered_data$dept <- sapply(filtered_data$dept, function(x) ifelse(x %in% small_counts[[1]], "Other", x))
