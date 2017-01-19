
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
  # environment variable which stores all structures data
  e <- baseenv

  # continuous numeric values which describe the occurence of a word in \code{%}
  e$word_freq <- spambase[,1:48]

  # continuous numeric values which describe the occurence of a special character in \code{%}
  e$char_freq <- spambase[,49:54]

  # continuous numeric value which describe the average length of uninterrupted sequences of
  #   capital letters in \code{%}
  e$CAP.length_avg <- spambase[,55, drop = F]

  # discret numeric value which describe the length of longest uninterrupted sequence of
  #   capital letters
  e$CAP.length_longest <- spambase[,56, drop = F]

  # discret numeric value which describe the total number of capital letters in the e-mail
  e$CAP.length_total <- spambase[,57, drop = F]

  # denotes whether the e-mail was considered spam (1) or not (0)
  e$classification <- spambase[,58, drop = F]
}

#' @title Create exploratory plots
#' @description
exploratory.create_plots <- function(){
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
