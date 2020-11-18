#'@title Tidy the ingested data from the smartsurvey API
#'
#'@description Tidy column names and remove empty rows from the ingested data. 
#' The question numbers returned are only nested to one level, e.g. Q1.1, Q1.2, etc.
#'
#'@param data the data returned by the ingest function (data.frame)
#'
#'@return the tidied data (data.frame)
#'
#'@export

tidy_ingest_data <- function(data) {
  
  # Check input
  if (!is.data.frame(data)) {
    stop("Unexpected input - data should be a data.frame")
  }
  
  # Rename headings
  # SmartSurvey data comes with question names spread over the first three rows and the full question names included
  # The code below relabels the columns with question numbers
  
  q_numbers <- sapply(
    colnames(data), function(colname) {
      if (substring(colname, 1, 1) == "Q") { # Return the question numbers from column names beginning with Q
        return(
          stringr::str_split(
            string = colname,
            pattern = "[.]",
            simplify = T
          )[1]
        )
      } else if (stringr::str_detect(colname, "X")) { # Return missing for empty column names
        return(NA)
      } else {
        return(colname)
      }
    }
  )

  q_numbers[1] <- "userID"
  
  # carry question numbers forward to replace missing question numbers
  q_numbers <- zoo::na.locf(q_numbers) 
  
  # Number duplicate column names
  q_freqs <- data.frame(table(q_numbers))
  q_freqs$q_numbers <-
    factor(q_freqs$q_numbers, levels = unique(q_numbers))
  q_freqs <- q_freqs[order(q_freqs$q_numbers),]
  
  new_colnames = Map(
    function(name, freq) {
      if (freq == 1) {
        return(as.character(name))
      } else {
        return(c(paste0(rep(name, freq),
                        c(
                          "", rep(".", freq - 1)
                        ),
                        c("", c(
                          1:(freq - 1)
                        )))))
      }
    },
    q_freqs$q_numbers, 
    q_freqs$Freq
  )
  new_colnames <- purrr::flatten(new_colnames)
  
  colnames(data) <- new_colnames
  
  # Drop empty rows
  data <- data[!is.na(data$userID), ] 

  # Correct strings
  data <- data.frame(
    lapply(data, function(x) {
      gsub("Don@SQ@t Know", "Don't Know", x)
    })
  )

  return(data)
}
