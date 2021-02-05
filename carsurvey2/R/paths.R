
#' @title Format filter path
#'
#' @description This function cleans the filter names
#'
#' @param path A string 
#'
#' @return String
#' 
#' @export
#'

format_filter_path = function(path) {
  
  url <- gsub(" \\(excl. agencies\\)", "", department)
  url <- gsub(" \\(or equivalent\\)", "", department)
  url <- gsub(" ", "-", url)
  url <- gsub(",", "", url)
  return(url)
  
}

