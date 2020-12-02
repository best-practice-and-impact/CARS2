# This script is designed to be used with the  project root as the working directory (../)

# Coding frequency table
data$code_freq <- factor(data$code_freq, levels = c("Never",
                                                    "Rarely",
                                                    "Sometimes",
                                                    "Regularly",
                                                    "All the time"))
freq_table <- data.frame(table(data$code_freq))
colnames(freq_table) <- c("Coding frequency", "Count")

# Programming tools

knowledge <- data[grepl("knowledge_", colnames(data))]
knowledge <- apply(knowledge, 2, table)
knowledge <- data.frame(apply(knowledge, 1, function(x) x))
knowledge$lang <- as.vector(stringr::str_split(rownames(knowledge), "_", simplify = TRUE)[,2])
knowledge <- knowledge[c(4, 3, 1, 2)]
colnames(knowledge) <- c("Programming language", "Yes", "Don't know", "No")

access <- data[grepl("available_", colnames(data))]
access <- apply(access, 2, table)
access <- data.frame(apply(access, 1, function(x) x))
access$lang <- as.vector(stringr::str_split(rownames(access), "_", simplify = TRUE)[,2])
access <- access[c(4, 3, 1, 2)]
colnames(access) <- c("Programming language", "Yes", "Don't know", "No")

code_tool_status <- data[grepl("status_", colnames(data))]
code_tool_status <- apply(code_tool_status, 2, table)
code_tool_status <- data.frame(apply(code_tool_status, 1, function(x) x))
code_tool_status$lang <- as.vector(stringr::str_split(rownames(code_tool_status), "_", simplify = TRUE)[,2])
code_tool_status <- code_tool_status[c(5, 2, 1, 3)]
colnames(code_tool_status) <- c("Programming language", "Access only", "Access and knowledge", "Knowledge only")


