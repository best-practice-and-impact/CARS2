# delete
ym = list("column_names" = column_names,
          "column_options" = opt, 
          "column_free_text" = col_left)

API_data <- carsurvey2::data_ingest()

data <- carsurvey2::data_convert_raw(API_data)  %>% 
  carsurvey2::data_tidy_ingest() %>% 
  carsurvey2::data_rename_cols() 

library(lubridate)
# config
dummy_data_row_number = 10
# dummy_data = data.frame("init" = 1:dummy_data_length)
config = yaml::read_yaml("dummy_data.yaml")

# end config

multiple_choice <- dummy_data_options(config, dummy_data_row_number)

config$column_free_text

userID = sample(149228438:151337079, dummy_data_row_number)
UserNo = 1:dummy_data_row_number
Name = rep(NA, dummy_data_row_number)
Email = rep(NA, dummy_data_row_number)
ips = c()
IP.Address = for(i in 1:dummy_data_row_number) ips = append(ips, paste(sample(1:256, 4), collapse = "."))
Unique.id = rep(NA, dummy_data_row_number)



ymd_hms("2020-10-01 20:11:59")



dummy_data_options <- function(config, dummy_data_row_number = 100) {
  
  dummy_data_options <- data.frame("init" = 1:dummy_data_row_number)
  
  column_options <- config$column_options
  
  for(col in names(column_options)) {
    
    name = gsub("_options", "", col)
    
    sampled_data = sample(column_options[[col]], dummy_data_row_number, replace = TRUE)
    
    dummy_data_options[name] <- sampled_data
    
  }
  dummy_data_options$init <- NULL
  return(dummy_data_options)
}

dummy_data_options <- data.frame("init" = 1:dummy_data_length)

column_options <- config$column_options

for(col in names(column_options)) {
  
  name = gsub("_options", "", col)
  
  sampled_data = sample(column_options[[col]], dummy_data_row_number, replace = TRUE)
  
  dummy_data_options[name] <- sampled_data
  
}















generate_random_multiple_choice <- function() {
  unique_chars = unique(column)
  sample(unique_chars, dummy_data_row_number, replace = TRUE)
}


multiple_choice <- function(column) {
  number_unique = 100
  
  if(nrow(unique(column) ) < number_unique ) {
    return(TRUE) 
  } else {
    return(FALSE)
  }
} 





# Functions 
# Need loading in before script can be run

create_yes_no_df <- function(dummy_data_length, yes_no_cols) {
  # creates equal number of yes or no
  
  dummy_data = data.frame("init" = 1:dummy_data_length)
  
  create_yes_no_column <- function(dummy_data_length) {
    binary = ifelse(sign(rnorm(dummy_data_length))==-1,0,1)
    ifelse(binary == 0, "Yes", "No")
  }
  
  for(col in yes_no_cols) {
    dummy_data[col] <- create_yes_no(dummy_data_length)
  }
  
  return(dummy_data)
}






for(i in column_names[yes_no]) {
  dummy_data[i] <- create_yes_no(dummy_data_length)
}


create_yes_no_column <- function(dummy_data_length) {
  binary = ifelse(sign(rnorm(dummy_data_length))==-1,0,1)
  ifelse(binary == 0, "Yes", "No")
}



only_yes_no <- function(column) {
  if(length(unique(column) ) == 2 ) {
    if( all(unique(column) %in% c("Yes", "No")) ) {
      return(TRUE)
    }
    return(FALSE)
  } else {
    return(FALSE)
  }
} 
  

# Create options
opt = list()

for (col in ym$column_names) {
  column = data[col]
  if(multiple_choice(column)) {
    name = paste0(col, "_options")
    
    options = unique(column)
    
    if(is.na(options)) next()
    
    opt[name] <- options
  }
}

  
