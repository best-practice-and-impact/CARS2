########  ######## ##    ## ########  ######## ########  
##     ## ##       ###   ## ##     ## ##       ##     ## 
##     ## ##       ####  ## ##     ## ##       ##     ## 
########  ######   ## ## ## ##     ## ######   ########  
##   ##   ##       ##  #### ##     ## ##       ##   ##   
##    ##  ##       ##   ### ##     ## ##       ##    ##  
##     ## ######## ##    ## ########  ######## ##     ## 

# High Level functions that are called to build the site. Prefixed render_

#' @title render_main_site
#'
#' @description Processes the Smart survey data to generate a series of tables. 
#' Then removes the old site and uses the generated tables to render a new site.
#' Uses the markdown files located in the internal var markdown_file_path.
#'
#' @param smart_survey_data This is generated using the carsurvey2::data_ functions.
#'
#' @export 


render_main_site = function(smart_survey_data) {
  
  markdown_file_path = "rmarkdown/main"
  
  # smart_survey_data This data object is bound to the function render_main_site
  # rmarkdown::render_site access this function enviroment to load the data 
  # It looks for an r object called smart_survey_data
  # DO NOT change the name of the dataframe! Keep it smart_survey_data
  
  # Remove old site and knit
  knitr::opts_chunk$set(message = FALSE, warning = FALSE)
  rmarkdown::clean_site(markdown_file_path)
  rmarkdown::render_site(markdown_file_path, quiet = TRUE)
  
}


#' @title render_navbar
#'
#' @description Renders and saves the site navbar.
#' @export

render_navbar = function() {
  
  # Function bound to render_navbar
  save_navbar <- function(code, path) {
    filename <- paste(path, "_navbar.html", sep = "/")
    write(code, filename)
  }
  
  # Create navigation bar
  navbar_info <- carsurvey2::read_site_yml("rmarkdown/main/_site.yml")
  navbar_page <- carsurvey2::html_build_navbar(navbar_info)
  save_navbar(navbar_page, "rmarkdown/main")
  save_navbar(navbar_page, "rmarkdown/deps")
}


#' @title render_department_pages
#' 
#' @description Creates pages for each department.
#' 
#' @details For each department filters the smart_survey_data to only that department
#' and then renders the common department template generating a unique page for each 
#' department. The template is located at the internal var template_path.  
#'
#' @param smart_survey_data This is generated using the carsurvey2::data_ functions.
#'
#' @export

render_department_pages = function(smart_survey_data) {
  
  output_folder = "../../docs"
  template_path = "rmarkdown/deps/template.rmd"
  
  message("Writing files to ", output_folder)
  # Get list of departments with sample >= 20
  deps <- data.frame(table(smart_survey_data$dept))
  dep_list <- deps[deps[2] >= 20, ]
  departments = as.character(dep_list$Var1)
  
  for (dep in departments) {
    message("Writing page for ", carsurvey2::format_file_path(dep))
    file_path <- carsurvey2::format_file_path(dep)
    
    # filter data to just the department
    filtered_data <- smart_survey_data[smart_survey_data$dept == dep, ]
    
    # Create a unique variable for each department
    variable_name <- paste0(file_path, "_filtered_data")
    
    # Create a variable in the global enviroment 
    # This is accessed by the rmarkdown::render()
    assign(variable_name, filtered_data)

    rmarkdown::render(template_path, 
                      output_file = paste0(output_folder, "/", file_path),
                      quiet = TRUE)
    
    # This is the opposite of assign
    # This delete the variable cleaning the global enviroment 
    rm(list = variable_name)
    
  } # end for-loop
  
}