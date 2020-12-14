#'@title Derive coding tools knowledge and access
#'
#'@description Derive coding tool knowledge but no access/access but no knowledge status..
#'
#'@param data a data frame containing cleaned CARS wave 2 data
#'
#'@return df containing the coding tool status columns
#'
#'@export

derive_code_status <- function(data) {
  # Check input
  expected_cols <- c(
    "knowledge_R",
    "knowledge_SQL",
    "knowledge_SAS",
    "knowledge_VBA",
    "knowledge_python",
    "knowledge_SPSS",
    "knowledge_stata",
    "knowledge_JS",
    "knowledge_java",
    "knowledge_C",
    "available_R",
    "available_SQL",
    "available_SAS",
    "available_VBA",
    "available_python",
    "available_SPSS",
    "available_stata",
    "available_JS",
    "available_java",
    "available_C"
  )
  
  if (!is.data.frame(data)) {
    stop("Unexpected input - data should be a data frame")
  } else if (!all(expected_cols %in% colnames(data))) {
    missing <-
      paste(expected_cols[!(expected_cols %in% colnames(data))], collapse = "\n")
    stop(paste0("Unexpected input - missing column names: ", missing))
  }
  
  lang_cols <- colnames(data)[grepl("available_", colnames(data))]
  langs <- as.vector(stringr::str_split(lang_cols, "_", simplify = T)[,2])
  
  new_cols <- lapply(langs, function(lang) {
    k_column <- as.vector(data[paste0("knowledge_", lang)] == "Yes")
    a_column <- as.vector(data[paste0("available_", lang)] == "Yes")
    new_col <- paste0(a_column, k_column)
    new_col[new_col == "TRUETRUE"] <- "Access and knowledge"
    new_col[new_col == "TRUEFALSE"] <- "Access only"
    new_col[new_col == "FALSETRUE"] <- "Knowledge only"
    new_col[new_col == "FALSEFALSE"] <- "No access or knowledge"
    return(new_col)
  })
  
  new_colnames <- paste0("status_", langs)
  data[new_colnames] <- new_cols
  
  return(data)
}
