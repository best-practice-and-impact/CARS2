#'@title Add table toggle buttons to down document 
#'
#'@description Add "show table" and "show chart" buttons to rmarkdown document (html). Expects table contaners to be named "name-table" and chart containers to be named "name-chart"
#'
#'@param output_name the name of assigned to the output. Used to name buttons and reference tables and chart. 
#'
#'@return html buttons and CSS 
#'
#'@export

insert_table_toggle <- function(output_name) {
  
  if (length(output_name) > 1) {
    stop("Unexpected input - output name should be a single character string.")
  }
  
  if (!grepl("^[A-Za-z]+$", substring(output_name, 1, 1))) { # If the first character of output_name is not a letter
    stop("Invalid html ID - output_name should begin with a letter")
  } else if (!grepl("^[a-zA-Z0-9_.-]*$", output_name)) { # If output_name contains invalid characters for html id
    stop("invalid html ID - output_name should not include special characters other than underscores, hyphens or stops")
  }
    
  table_button_name <- paste("show-table", output_name, sep = "-")
  chart_button_name <- paste("show-chart", output_name, sep = "-")
  chart_name <- paste0(output_name, "-chart")
  table_name <- paste0(output_name, "-table")
  
  toggle_chart_button <- paste0(
    '<a role="button" class="toggle-button" id="', 
    chart_button_name, 
    '" onclick="show_chart(\'', output_name,'\')"> Show chart </a>'
  )
  
  toggle_table_button <- paste0(
    '<a role="button" class="toggle-button" id="', 
    table_button_name, 
    '" onclick="show_table(\'', output_name,'\')"> Show table </a>'
  )
  
  # Add style to hide table and show chart button by default
  style <- paste0(
    '<style>',
    ' #', chart_button_name, ' {display: none;}',
    ' #', table_name, ' {display: none;}',
    ' </style>'
  )
  
  knitr::raw_html(
    paste(
      toggle_chart_button,
      toggle_table_button,
      style,
      sep = "\n"
    )
  )
}