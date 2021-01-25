
generate_dummy_data <- function(dummy_data_row_number = 100, config = NULL) {
  
  if(is.null(config)) config = yaml::read_yaml("carsurvey2/dummy_data.yaml")
  
  multiple_choice <- dummy_data_options(config, dummy_data_row_number)
  
  free_text <- dummy_data_free_text(config, dummy_data_row_number)
  
  unique_cols <- dummy_data_unique(dummy_data_row_number)
  
  
  dummy_data <- data.frame(multiple_choice, free_text, unique_cols)
  
  if(!length(config$column_names) == length(dummy_data)) stop("Incorrect Number of columns")
  
  if(!all(config$column_names %in% colnames(dummy_data))) stop("column names aren't correct")
  
  dummy_data <- dplyr::select(dummy_data, config$column_names)
  
  return(dummy_data)
}


dummy_data_free_text = function(config, dummy_data_row_number) {

  dummy_data_text_df <- data.frame("init" = 1:dummy_data_row_number)
  
  for (col in config$column_free_text) {
    
    dummy_data_text_df[col] <- generate_text(dummy_data_row_number)
  }
  
  # handle use_other values are wither text or NO (not NA)
  dummy_data_text_df$use_other <-ifelse(is.na(dummy_data_text_df$use_other), "No", dummy_data_text_df$use_other)
  
  dummy_data_text_df$init <- NULL
  
  return(dummy_data_text_df)
}

# Free Text






dummy_data_unique = function(dummy_data_row_number) {
  
  # Uniques
  userID = sample(149228438:151337079, dummy_data_row_number)
  UserNo = 1:dummy_data_row_number
  Name = rep(NA, dummy_data_row_number)
  Email = rep(NA, dummy_data_row_number)
  ips = c()
  for(i in 1:dummy_data_row_number) ips = append(ips, paste(sample(1:256, 4), collapse = "."))
  IP.Address = ips
  Unique.ID = rep(NA, dummy_data_row_number)
  Started = generate_time(dummy_data_row_number)[["start"]]
  Ended = generate_time(dummy_data_row_number)[["end"]]
  
  data.frame(userID, 
             UserNo,
             Name,
             Email,
             IP.Address,
             Unique.ID,
             Started,
             Ended)
  
}


generate_text <- function(dummy_data_row_number, frequency = 7) {
  # frequency controls how many are NA 
  # so frequency = 6 would mean on average 6 would be NA 4 would be text

  vec <- c()
  
  for (i in 1:dummy_data_row_number) {
    if(sample(1:10, 1) > frequency) {
      vec = append(vec, generate_elvish())
    } else {
      vec = append(vec, NA)
    }
    
  }
  return(vec)
}



generate_elvish <- function() {
  sentence = ""
  for (i in 1:sample(1:30, 1)){
    word <- paste0(sample(letters, sample(2:10, 1)), collapse = "")
    sentence <- paste(sentence, word, sep = " ")
  }
  
  return(sentence)
}


generate_time <- function(dummy_data_row_number) {
  
  start_vec = c()
  end_vec = c()
  
  for (i in 1:dummy_data_row_number){
    
    duration_min <- sample(2:10, 1)
    duration_sec <- sample(1:60, 1)
    
    year <- "2020"
    month <- sample(10:12, 1)
    day <- sample(10:28, 1)
    
    hour <- sample(1:24, 1)
    minute <- sample(1:60, 1)
    sec <- sample(1:60, 1)
    
    
    if (hour < 10) {
      hour_char <- paste0("0", hour)
    } else {
      hour_char <- hour
    }
    
    if (minute < 10) {
      minute_char <- paste0("0", minute)
    } else {
      minute_char <- minute
    }
    
    if (sec < 10) {
      sec_char <- paste0("0", sec)
    } else {
      sec_char <- sec
    }
    
    start <- paste0(paste(year, month, day, sep = "-"),
                    " ",
                    paste(hour_char, minute_char, sec_char, sep = ":"))
    
    if(minute + duration_min > 60) {
      hour <- hour + 1
      minute <- minute + duration_min - 60
      
      if (hour > 24) hour <- hour -24
      
    }
    
    if(sec + duration_sec > 60) {
      minute <- minute + 1
      sec <- sec + duration_sec - 60
      if(minute > 60) minute <- minute - 60
    }
    
    
    if (hour < 10) {
      hour_char <- paste0("0", hour)
    } else {
      hour_char <- hour
    }
    
    if (minute < 10) {
      minute_char <- paste0("0", minute)
    } else {
      minute_char <- minute
    }
    
    if (sec < 10) {
      sec_char <- paste0("0", sec)
    } else {
      sec_char <- sec
    }
    
    end <- paste0(paste(year, month, day, sep = "-"),
                  " ",
                  paste(hour_char, minute_char, sec_char, sep = ":"))
    
    start_vec <- append(start_vec, start)
    end_vec <- append(end_vec, end)
  } # end for
  
  
  
  return(list("start" = start_vec, 
              "end" = end_vec))
  
}


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



