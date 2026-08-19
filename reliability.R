library(data.table)
setwd("/data/")

data <- fread("data.txt")


phq_items <- data_all[, paste0("PHQ", 1:9)]

# Cronbach's alpha
alpha_result <- psych::alpha(phq_items)

alpha_result$total$raw_alpha

alpha_result$alpha.drop



gese_items <- data_all[, paste0("GESE", 1:10)]


# Cronbach's alpha
gese_alpha <- psych::alpha(gese_items)

gese_alpha$total$raw_alpha

gese_alpha$total$std.alpha
