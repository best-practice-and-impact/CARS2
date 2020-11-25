#'@title Convert raw smartsurvey data to data.frame 
#'
#'@description Extract contents (raw csv) from smartsurvey API request and convert to data.frame
#'
#'@param r api response object
#'
#'@return response content as a data.frame
#'
#'@export

convert_raw <- function(r) {
  
  if (class(r) != "response") {
    stop("Unexpected input - r is not a response object.")
  } else if (r$status_code != 200) {
    stop("Unsuccessful API request - no data.")
  }
  
  content <- rawToChar(r$content)
  
  data <- utils::read.table(
    text = content, 
    sep = ",", 
    header = TRUE, 
    fill = TRUE, 
    quote = "\"\"",
    na.strings = c("", ".", "NA", "-", "\"\"", "\".\"", "\"NA\"", "\"-\"")
  )
  
  return(data)
}
