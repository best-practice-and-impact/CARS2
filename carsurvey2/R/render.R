# High Level functions that are called to build the site. Prefixed render_

#' @title Render main site
#'
#' @description Processes the Smart survey data to generate a series of tables. 
#' Then removes the old site and uses the generated tables to render a new site.
#' Uses the markdown files located in the internal var markdown_file_path.
#'
#' @param data This is generated using the carsurvey2::data_ functions.
#' @param markdown_file_path the path containing the rmarkdown site documents
#'
#' @export 


render_main_site <- function(data, markdown_file_path = "rmarkdown/main") {

  # Remove old site and knit
  knitr::opts_chunk$set(message = FALSE, warning = FALSE)
  rmarkdown::clean_site(markdown_file_path)
  rmarkdown::render_site(markdown_file_path, quiet = TRUE)
  
}

#' @title Render navigation bar
#'
#' @description Creates the site navbar.
#' 
#' @param yml_path Path to the rmarkdown site yml config
#' 
#' @return navbar_page html code for navbar
#' 
#' @export

render_navbar <- function(yml_path = "rmarkdown/main/_site.yml") {
  
  # Create navigation bar
  navbar_info <- carsurvey2::read_site_yml(yml_path)
  navbar_page <- carsurvey2::build_navbar(navbar_info)
  
  return(navbar_page)
}

#' @title Save navigation bar
#'
#' @description Saves the site navbar.
#' 
#' @param code html code (string)
#' @param path path for saving the navigation bar (excluding file name)
#' 
#' @export

save_navbar <- function(code, path) {
  filename <- paste(path, "_navbar.html", sep = "/")
  write(code, filename)
}

#' @title Render filtered pages
#' 
#' @description Creates pages by filter (e.g. by department/profession/grade).
#' 
#' @details For each department filters the data to only that department
#' and then renders the common department template generating a unique page for each 
#' department. The template is located at the internal var template_path.  
#'
#' @param data This is generated using the carsurvey2::data_ functions.
#' @param filter_variable The variable that the data is filtered on. This is given as a string such as "dept". 
#' The data is filtered by getting all values in the filter_variable greater than 20 and subsetting the data.
#' @param page_title This is ONLY PART of the title. The title is generated from "DRAFT: ", page_title , " profile: ", filter -- 
#' where filter is one element of the list as described above in filter_variable (values over 20) and the page title is this argument.
#' @param output_folder Folder the site is built and saved to
#' @param template_path The path to the template that gets render for each department
#' @export


render_filtered_pages <- function(data,
                               filter_variable,
                               page_title = "",
                               output_folder = "../../docs",
                               template_path = "rmarkdown/summary_template/template.rmd") {
  
  if(!sum(colnames(data) == filter_variable) == 1) stop("filter column: ", filter_variable,
                                                                     " doesn't exist in the data provided. \nCheck that filter is equal to a valid column name")
  
  # get grade with sample > 20
  filter_table <- data.frame(
    table(data[filter_variable])
  )
  filter_over_20 <- filter_table[filter_table[2] >= 20, ]
  filter_list = as.character(filter_over_20$Var1)
  
  for (filter in filter_list) {
    
    file_path <- carsurvey2::format_department_path(filter)
    message("Writing page for ", file_path)
    
    # filter data to just the department
    filtered_data <- data[data[filter_variable] == filter, ]
    if(!nrow(filtered_data) == filter_over_20$Freq[filter_over_20$Var1 == filter]) stop("Filtered data row number is not equal to number of : ", file_path)
    
    title <- paste0("DRAFT: ", page_title , " profile: ", filter)
    
    filtered_tables <- carsurvey2::generate_tables(filtered_data)
    
    samples <- list(
      all = nrow(filtered_data),
      coders = sum(filtered_data$code_freq != "Never"),
      heard_of_rap = sum(filtered_data$RAP_heard_of == "Yes")
    )
    
    rmarkdown::render(template_path, 
                      output_file = paste0(output_folder, "/", file_path),
                      quiet = TRUE,
                      params = list(
                        title = title, 
                        tables = filtered_tables,
                        samples = samples
                      ))
  } 
}
