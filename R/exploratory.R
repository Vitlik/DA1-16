
#' @title Exploratory data analysis wrapper function
#' @description This is a wrapper function for the whole exploratory data analysis step
b.exploratory.data_analysis <- function(){
  # structure the data into the groups it contains
  b.exploratory.structure_data()

  # write structure and summary information to files
  b.exploratory.str_summary()

  #
  b.exploratory.correlations()

  #
  b.exploratory.dim_vis()
}

#' @title Structure the spambase data
#' @description Create an environment variable \code{stuff} which
b.exploratory.structure_data <- function(){
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
#' @description Several
b.exploratory.str_summary <- function(){
  # create outputfolder if not existing yet
  if(!file.exists("out")) dir.create("out")

  # Write information about the structure of the data into a file
  target <- file.path("out/1. Exploratory - str.txt")
  file.create(target)
  sink(target)
    str(spambase)
  sink()

  # Write summary information of the data into a file
  to_write <- z.util.gen.summary(noclasses, "All")
  target <- file.path("out/1. Exploratory - summary.csv")
  file.create(target)
  write.csv2(to_write, target)
  #
  to_write <- z.util.gen.summary(allnospam, "Diff", allspam)
  target <- file.path("out/1. Exploratory - summary - compare.csv")
  file.create(target)
  write.csv2(to_write, target)
}

#' Test title
#'
#' blub description
b.exploratory.correlations <- function(){
  # correlation view
  #
  pdf(file.path("out/1. Exploratory - Correlation matrix.pdf"))
  corrplot::corrplot(cor(noclasses), main = "\nCorrelation Matrix - All Data", type="upper",
                     method = "circle", tl.cex=0.5, diag = F, order = "hclust", tl.srt=80)
  corrplot::corrplot(cor(allnospam), main = "\nCorrelation Matrix - NoSpam", type="upper",
                     method = "circle", tl.cex=0.5, diag = F, order = "hclust", tl.srt=80)
  corrplot::corrplot(cor(allspam), main = "\nCorrelation Matrix - Spam", type="upper",
                     method = "circle", tl.cex=0.5, diag = F, order = "hclust", tl.srt=80)
  dev.off()
}

#'
b.exploratory.dim_vis <- function(){
  # scatterplots, barplots,

  #
  pdf(file.path("out/1. Exploratory - Histograms.pdf"))
  #layout(matrix(1:(5 * ceiling(ncol(spambase[,1:20])/5)), ncol = 5, byrow = T))
  for(name in names(spambase[,1])){
    print(ggplot(noclasses, aes(x=noclasses[,"make"])) +
      geom_histogram(binwidth = max(noclasses[,"make"])/100, fill=ercis.red) +
      theme_bw(base_size = 12, base_family = ""))
    # ggplot(noclasses[noclasses[,2]>0,], aes(x=noclasses[noclasses[,2]>0,2])) +
    #   geom_histogram(binwidth = 0.14, fill=ercis.red) + theme_bw(base_size = 12, base_family = "")
  }
  dev.off()
}
