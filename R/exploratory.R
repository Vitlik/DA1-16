
#' @title Exploratory data analysis wrapper function
#' @description This is a wrapper function for the whole exploratory data analysis step
exploratory.data_analysis <- function(){
  # structure the data into the groups it contains
  exploratory.structure_data()

  # create various plots for visual analysis
  exploratory.create_plots()
}

#' @title Structure the spambase data
#' @description Create an environment variable \code{stuff} which
exploratory.structure_data <- function(){
  e <<- baseenv()

  # denotes whether the e-mail was considered spam (1) or not (0)
  e$classes <- spambase[, 58, drop = F]
  e$noclasses <- spambase[, -58, drop = F]
  e$allspam <- spambase[spambase[,58]==1, -58]
  e$allnospam <- spambase[spambase[,58]==0, -58]

  # continuous numeric values which describe the occurence of a word in \code{%}
  e$word_freq <- spambase[, 1:48]
  e$word_freq.spam <- spambase[spambase[,58]==1, 1:48]
  e$word_freq.nospam <- spambase[spambase[,58]==0, 1:48]

  # continuous numeric values which describe the occurence of a special character in \code{%}
  e$char_freq <- spambase[, 49:54]
  e$char_freq.spam <- spambase[spambase[,58]==1, 49:54]
  e$char_freq.nospam <- spambase[spambase[,58]==0, 49:54]

  # continuous numeric value which describe the average length of uninterrupted sequences of
  #   capital letters in \code{%}
  e$CAP.length_avg <- spambase[, 55, drop = F]
  e$CAP.length_avg.spam <- spambase[spambase[,58]==1, 55, drop = F]
  e$CAP.length_avg.nospam <- spambase[spambase[,58]==0, 55, drop = F]

  # discret numeric value which describe the length of longest uninterrupted sequence of
  #   capital letters
  e$CAP.length_longest <- spambase[, 56, drop = F]
  e$CAP.length_longest.spam <- spambase[spambase[,58]==1, 56, drop = F]
  e$CAP.length_longest.nospam <- spambase[spambase[,58]==0, 56, drop = F]

  # discret numeric value which describe the total number of capital letters in the e-mail
  e$CAP.length_total <- spambase[, 57, drop = F]
  e$CAP.length_total.spam <- spambase[spambase[,58]==1, 57, drop = F]
  e$CAP.length_total.nospam <- spambase[spambase[,58]==0, 57, drop = F]
}

#' @title Create exploratory plots
#' @description
exploratory.create_plots <- function(){
  # create outputfolder if not existing yet
  if(!file.exists("out")) dir.create("out")

  # Write information about the structure of the data into a file
  target <- file.path("out/1. Exploratory - str.txt")
  file.create(target)
  sink(target)
    str(spambase)
  sink()

  # Write summary information of the data into a file
  target <- file.path("out/1. Exploratory - summary.csv")
  file.create(target)
  write.csv2(summary(spambase), target)
  #
  target <- file.path("out/1. Exploratory - summary - spam.csv")
  file.create(target)
  write.csv2(summary(allspam), target)
  #
  target <- file.path("out/1. Exploratory - summary - nospam.csv")
  file.create(target)
  write.csv2(summary(allnospam), target)

  #
  pdf(file.path("out/1. Exploratory - Histograms.pdf"))
  layout(matrix(1:(5 * ceiling(ncol(spambase[,1:20])/5)), ncol = 5, byrow = T))
  sapply(names(spambase[,1:20]), function(name) {
    hist(spambase[[name]], main = name,
         #xlab = paste("p =", shapiro.test(spambase[[name]])),
         col = ercis.red, col.axis = ercis.grey, col.lab = ercis.grey,
         col.main = ercis.grey, col.sub = ercis.grey)
  })
  dev.off()

  #
  pdf(file.path("out/1. Exploratory - Correlation matrix.pdf"))
  corrplot::corrplot(cor(spambase[,1:57]), method = "circle", tl.cex=0.5)
  dev.off()
}
