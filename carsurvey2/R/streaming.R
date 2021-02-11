#'@title Enforce streaming rules
#'
#'@description Input the data as a data frame and apply each of the streaming rules in turn.
#'
#'@param Input the data which is the output from carsurvey2::tidy_ingest(data) 
#'
#'@return the exported data after streaming rules as a dataframe
#'
#'@export

streaming <- function(data){
  if (class(data) != "data.frame") {
    stop("Unexpected input - data is not a data.frame")
  } 
  #If degree then dont ask page 3
  col_names <- colnames(dplyr::select(data,Q7:Q7.16))
  Q7_data <- dplyr::select(data,Q7:Q7.16)
  Q7_data[!(data$Q6 %in% c("Bachelor's degree (or equivalent)","Master's degree (or equivalent)","Doctoral degree (or equivalent)")),] <- NA
  data[col_names] <- Q7_data
  
  #If Q8 is "Never" then make P12 NA
  col_names <- colnames(dplyr::select(data,Q19:Q24))
  Q8_data <- dplyr::select(data,Q19:Q24)
  Q8_data[data$Q8 == "Never",] <- NA
  data[col_names] <- Q8_data
  
  # if Q13 "No" skip P7
  data$Q14 <- ifelse(data$Q13 =="Yes" , data$Q14, NA)
  
  #If Q15 "No" skip P 9, 10 and 11
  col_names <- colnames(dplyr::select(data,Q16:Q18.6))
  Q15_data <- dplyr::select(data,Q16:Q18.6)
  Q15_data[data$Q15 == "No",] <- NA
  data[col_names] <- Q15_data
  
  
  #If Q16 is "Yes" Skip P10
  data$Q17 <- ifelse(data$Q16 =="Yes" , NA, data$Q17)
  
  #If Q19: select source code and I don't understand ot never then skip P13
  col_names <- colnames(dplyr::select(data,Q25:Q25.5))
  Q19_data <- dplyr::select(data,Q25:Q25.5)
  Q19_data[data$Q19 %in% c("Never","I don't understand this question"),] <- NA
  data[col_names] <- Q19_data

  return(data)
}
