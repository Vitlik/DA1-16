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

# write table to file for the package
devtools::use_data(spambase, overwrite = T)
