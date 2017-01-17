#' This script won't be part of the build package.
#' Its purpose is to transfer the source data to a R data.frame.
#'
#' The original dataset needed to be adjusted beforehand because it contained REAL and Integer
#' categories for describing the dimensions.
#'
library(foreign)
spambase <- read.arff(file = "data-raw/dataset_44_spambase_adjusted.arff")

# remove duplicates as they distort the analysis
spambase <- spambase[!duplicated(spambase)]

# adjust datatype to original datatype which got lost during read
spambase$capital_run_length_longest <- as.integer(spambase$capital_run_length_longest)
spambase$capital_run_length_total <- as.integer(spambase$capital_run_length_total)

names(spambase)[1:57] <- c("make", "address", "all", "3d", "our", "over", "remove", "internet",
                           "order", "mail", "receive", "will", "people", "report", "adresses",
                           "free", "business", "email", "you", "credit", "your", "font", "000",
                           "money", "hp", "hpl", "george", "650", "lab", "labs", "telnet", "857",
                           "data", "415", "85", "technology", "1999", "parts", "pm", "direct",
                           "cs", "meeting", "original", "project", "re", "edu", "table",
                           "conference", ";", "+", "[", "!", "$", "#", "CAP_avg", "CAP_longest",
                           "CAP_total")

# write table to file for the package
devtools::use_data(spambase, overwrite = T)

