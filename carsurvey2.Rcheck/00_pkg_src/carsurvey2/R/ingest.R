#'@title Download smartsurvey export via the API
#'
#'@description Download the exported data from smartsurvey using the API and convert binary csv file to a data frame.
#'
#'@param survey the survey ID (character string/numeric). Defaults to "790800".
#'@param export the export ID (character string/numeric). Defaults to "1438876".
#'@param token the API token (character string). Loaded from environment variable by default.
#'@param secret the secret API token (character string). Loaded from environment variable by default.
#'@param check_hash a hash to check the downloaded data aginst or FALSE to skip the check (character string/logical). Defaults to final export hash.
#'
#'@return the exported data as a dataframe
#'
#'@export

ingest <- function(survey = "790800",
                   export = "1438876",
                   token = Sys.getenv("CARS_TOKEN"),
                   secret = Sys.getenv("CARS_SECRET"),
                   check_hash = "6c64deda0580f9cf9cc6a32a3445472b13179053") {
  
  # Check input types
  if (!is.character(survey) && !is.numeric(survey) | !is.character(export) && !is.numeric(export)) {
    stop("Unexpected input - survey and export should be character or numeric variables.")
  }
  
  if (!is.character(token) | !is.character(secret)) {
    stop("Unexpected input - token and secret should be character variables.")
  }
  
  if (!is.character(check_hash) && check_hash != FALSE) {
    stop("Unexpected input - check_hash should be a character variable or set to FALSE.")
  }
  
  if (length(survey) > 1 | length(token) > 1 | length(token) > 1 | length(secret) > 1 | length(check_hash) > 1) {
    stop("Unexpected input - one or more of the supplied arguments contain multiple elements.")
  }
  
  # API request
  url <- paste0("https://api.smartsurvey.io/v1/surveys/", survey, "/exports/", export, "/download") 
  
  query_string <- list(
    api_token = token,
    api_token_secret = secret
  )
  
  tryCatch(
    {
      r <- httr::GET(
        url, 
        query = query_string
      )    
    },
    error = function(e) {
      stop(paste("Error in API request: ", e))
    }
  )
  
  # Check request status code
  if (r$status_code != 200) {
    stop(paste0("Unsuccessful API request. Status code: ", r$status_code))
  }
  
  # Check hashes match
  if (is.character(check_hash)) {
    hash <- digest::digest(r$content, algo = "sha1", serialize = FALSE)
    if (hash != check_hash) {
      stop("hashed data does not match verification hash.")
    }
  }
  
  return(r)
}