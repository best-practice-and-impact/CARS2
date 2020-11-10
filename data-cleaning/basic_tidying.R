# Set working directory before running this script

# Rename headings
# SmartSurvey data comes with question names spread over the first three rows and the full question names included
# The code below relabels the columns with more useful labels (question numbers)

data <- read.csv("latest_data.csv", na.strings = c("", ".", "NA", "-"))
original_colnames <- colnames(data)

get_qnames <- function(colname) {
  if (stringr::str_detect(colname, "Q")) {
    return(stringr::str_split(
      string = colname,
      pattern = "[.]",
      simplify = T
    )[1])
  } else if (stringr::str_detect(colname, "X")) {
    return(NA)
  } else {
    return(colname)
  }
}

q_numbers <- c(unname(sapply(colnames(data), get_qnames)))

q_numbers <- zoo::na.locf(q_numbers)
q_freqs <- data.frame(table(q_numbers))
q_freqs$q_numbers <-
  factor(q_freqs$q_numbers, levels = unique(q_numbers))
q_freqs <- q_freqs[order(q_freqs$q_numbers),]

label_duplicates <- function(name, freq) {
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
}

new_colnames = Map(label_duplicates, q_freqs$q_numbers, q_freqs$Freq)
new_colnames <- purrr::flatten(new_colnames)

colnames(data) <- new_colnames

# Drop unnecessary columns and rows

data <- data[-c(1, 2), ]

data <-  subset(data, select = -c(
    UserNo,
    Name,
    Email, 
    IP.Address,
    Unique.ID,
    Started,
    Ended
  )
)

names(data)[names(data) == "ï..UserID"] <-  "unique.ID"

# Correct strings

data <- data.frame(
  lapply(data, function(x) {
    gsub("Don@SQ@t Know", "Don't Know", x)
  })
)


save(data, file = "relabelled_data.Rdata")

