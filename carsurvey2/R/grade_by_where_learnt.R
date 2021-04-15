data <- carsurvey2::preprocess()

#unique(data$code_learn_where)
levels <- c("In education",
            "In private sector employment",
            "In public sector employment",
            "Self-taught",
            "Other")

# Recode other responses
data$code_learn_where[!is.na(data$code_learn_where) & !data$code_learn_where %in% levels ] <- "Other"
data$code_learn_where <- factor(data$code_learn_where, levels = levels)

#(data$grade)
order <- c(
  "Executive Officer (or equivalent)",
  "Higher Executive Officer (or equivalent)",
  "Senior Executive Officer (or equivalent)",
  "Fast Stream",
  "Grade 7 (or equivalent)",
  "Grade 6 (or equivalent)",
  "Other - NHS",
  "Other"
)
data$grade <- factor(data$grade, levels = order)

#Grade and where learnt
grade_by_where_learnt <- data[,c("code_learn_where","grade")]
cross_table_data <- table(grade_by_where_learnt$code_learn_where, grade_by_where_learnt$grade)
#percentage
cross_table_prop <- prop.table(cross_table_data ,2)
cross_table_prop <- as.data.frame.matrix(cross_table_prop)
cross_table_prop<- round(cross_table_prop,2)
cross_table_data <- cross_table_prop[1:6]


#Degree by where learnt
data <- carsurvey2::preprocess()


#'@title process data
#'
#'@decription filters data to where learnt and degree subjects
#'
#'@param data
#'
#'@return data frame
#'
#'@export 
#'
process_data <- function(data){
  degree <- dplyr::select(data,c("code_learn_where","maths":"lang_lit"))
  degree <- na.omit(degree)
  degree[degree == "No"] <-  as.numeric(0)
  degree[degree == "Yes"] <- as.numeric(1)
  degree[-1] <- lapply(degree[-1], as.numeric)
  return(degree)
}

degree <- process_data(data)

#' @title merge columns
#'
#' @description takes data and merges two columns and removes them
#' 
#' @param data data frame
#' @param col1 1st column name
#' @param col2 2nd column name
#' 
#' @return dataframe
#' 
#' @export

merge_columns <- function(data, col1, col2) {
  data <- dplyr::mutate(data, new_col = dplyr::case_when(data[col1] == 1 | data[col2] == 1 ~ 1,
                                                         data[col1] == 0 & data[col2] == 0 ~ 0))
  data[c(col1, col2)] <- NULL
  names(data)[names(data) == "new_col"] <- paste(col1, " & ", col2)
  return(data)
}

#merge columns
degree <- merge_columns(degree, "lifesci", "health")
degree <- merge_columns(degree, "law", "history")
degree <- merge_columns(degree, "law  &  history", "lang_lit")
degree <- merge_columns(degree, "engineer", "systems")


#aggregate

degree_by_where <- aggregate(degree[ , colnames(degree) != "code_learn_where"],
                             list(degree$code_learn_where),
                             sum,
                             na.rm = T)
degree_by_where_actuals <- degree_by_where

#prop
degree_by_where[-1] <- t(t(degree_by_where[-1])/rowSums(t(degree_by_where[-1])))
degree_by_where[-1] <- round(degree_by_where[-1],2)
#filter
degree_by_where <- degree_by_where[c(4,5,6,7),]

#exploratory
sum(apply(degree_by_where_actuals[-1],2,sum))





