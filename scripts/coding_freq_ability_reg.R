data <- carsurvey2::preprocess()

### Coding ability change by coding frequency
data$ability_change <- factor(data$ability_change, levels = c("Significantly worse", 
                                                              "Slightly worse", 
                                                              "No change", 
                                                              "Slightly better",
                                                              "Significantly better"))

data$code_freq <- factor(data$code_freq , levels = c("Never", 
                                                     "Rarely",
                                                     "Sometimes",
                                                     "Regularly",
                                                     "All the time"))


ability_change_crosstab <- as.data.frame.matrix(table(data$code_freq, data$ability_change))
ability_change_perc <- ability_change_crosstab/rowSums(ability_change_crosstab)

ability_change_perc <- cbind(change_in_ability = factor(rownames(ability_change_perc), levels = rownames(ability_change_perc)), 
                             ability_change_perc)

# Plot
carsurvey2::plot_likert(ability_change_perc, 
                        3, 
                        "Frequency", 
                        "Coding<br>frequency",
                        n = sum(ability_change_crosstab))

# Chi squaew
chisq.test(ability_change_crosstab)

# Logistic regression

# Data preparation

ability_change <- data[!is.na(data$ability_change), c("code_freq", "ability_change")]
ability_change <- dplyr::mutate(ability_change, Never_rarely = as.numeric(code_freq == "Never" | code_freq == "Rarely"),
                                Sometimes = as.numeric(code_freq == "Sometimes"),
                                Regularly_all_the_time = as.numeric(code_freq == "Rarely" | code_freq == "All the time"))

ability_change$ability_change <- factor(ability_change$ability_change, levels = c("Significantly worse", 
                                        "Slightly worse", 
                                        "No change", 
                                        "Slightly better",
                                        "Significantly better"))

# Model

model <- MASS::polr(ability_change ~ Regularly_all_the_time + Sometimes + Never_rarely, data = ability_change, Hess=TRUE)

coefficients_table <- coef(summary(model))

ci <- confint(model)

or <- exp(cbind(OR = coef(model), ci))

p <- pnorm(abs(coefficients_table[, "t value"]), lower.tail = FALSE) * 2
