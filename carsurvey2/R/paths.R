
#' @title Format department path
#'
#' @description This function cleans the department names
#'
#' @param department A string 
#'
#' @return String
#' 
#' @export
#'

format_department_path = function(department) {
  
  url <- gsub(" \\(excl. agencies\\)", "", department)
  url <- gsub(" ", "-", url)
  url <- gsub(",", "", url)
  return(url)
  
}

