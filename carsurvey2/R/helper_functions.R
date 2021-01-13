
#' @title format_file_path
#'
#' @description This function cleans the department names
#'
#' @param department 
#'
#' @return String
#' 
#' @export
#'
#' @examples
#' 
#' department = "Cabinet Office (excl. agencies)"
#' 
#' format_file_path(department)

format_file_path = function(department) {
  
  url <- gsub(" \\(excl. agencies\\)", "", department)
  url <- gsub(" ", "-", url)
  url <- gsub(",", "", url)
  return(url)
  
}



#' @title print_cat
#' 
#' @details Who knows what this function does, run it to find out ... if you dare!
#' 
#' @export
#'
#' @examples print_cat()

print_success_cat = function() {
  
  cat_person <- r"{

 -----------------------------  
 Website Built It's purrrfect!
 ----------------------------- 
    \
      \
        \
            |\___/|
          ==) ^Y^ (==
            \  ^  /
             )=*=(
            /     \
            |     |
           /| | | |\
           \| | |_|/\
           //_// ___/
               \_)
  
}"
  cat(cat_person)

}