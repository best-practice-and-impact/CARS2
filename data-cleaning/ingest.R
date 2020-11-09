# Get data(csv) from smartsurvey API

url <- "https://api.smartsurvey.io/v1/surveys/790800/exports/1438876/download" 

query_string <- list(
  page_size = "2",
  api_token = Sys.getenv("CARS_TOKEN"),
  api_token_secret = Sys.getenv("CARS_SECRET"),
  include_labels = "true",
  sort_by = "date_ended,asc"
)

r <- httr::GET(
  url, 
  query = query_string
)

content <- httr::content(r, as = "text")

data <- read.table(
  text = content, 
  sep = ",", 
  header = TRUE, 
  fill = TRUE, 
  quote = "\"\"",
  na.strings = c("", ".", "NA", "-")
)
