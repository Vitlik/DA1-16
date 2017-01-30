
#' @title Exploratory - Data Analysis Wrapper
#' @description To get (back) to the overview of all steps and functions use this link:
#' \code{\link{a.a.main}}
#'
#' This is a wrapper function for the whole exploratory data analysis step.
#'
#' First the data is stored in different variables which will then be accessable from the
#' environment.
#'
#' Then different visualizations are created which are summaries, correlations and dimension
#' visualization.
#'
#' It executes four functions:
#' \enumerate{
#'   \item \code{\link{b.b.exploratory.structure_data}}
#'   \item \code{\link{b.c.exploratory.str_summary}}
#'   \item \code{\link{b.d.exploratory.correlations}}
#'   \item \code{\link{b.e.exploratory.dim_vis}}
#' }
#'
#' @author Vitali Friesen
b.a.exploratory.data_analysis <- function(){
  # structure the data into the structural groups it contains
  b.b.exploratory.structure.data()

  # write structure and summary information to files
  b.c.exploratory.str_summary()

  # create correlation plots
  b.d.exploratory.correlations()

  # visualize the single dimensions/attributes of the data
  b.e.exploratory.dim_vis()
}

#' @title Exploratory - Structure Data
#' @description To get (back) to the overview of all steps and functions use this link:
#' \code{\link{a.a.main}}
#'
#' It creates an environment variable \code{e} which stores all types types in its own variable.
#'
#' @author Vitali Friesen
b.b.exploratory.structure.data <- function(){
  par(mar=c(4.1,5.1,4.1,2.1))

  e <<- baseenv()

  # denotes whether the e-mail was considered spam (1) or not (0)
  e$classes <- spambase[, 58]
  e$noclasses <- spambase[, -58]
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

#' @title Exploratory Structure Summary
#' @description To get (back) to the overview of all steps and functions use this link: \code{\link{a.a.main}}
#'
#' Create a txt file with the output of str(spambase) into the folder "out/" named: "1.
#' Exploratory - str.txt"
#'
#' Create csv files with different information of about the data (min, 25% quantile, median, mean,
#' 75% quantile, max) with the name: "1. Exploratory - summary.csv"
#'
#' Additionally it creates another summary file where summaries of spam and non-spam are compared:
#' "1. Exploratory - summary - compare.csv"
#'
#' @author Vitali Friesen
b.c.exploratory.str_summary <- function(){

  # Write information about the structure of the data into a file
  target <- file.path("out/1. Exploratory - str.txt")
  file.create(target)
  sink(target)
    str(spambase)
  sink()

  # Write summary information of the data into a file
  to_write <- z.a.util.gen.summary(noclasses, "All")
  target <- file.path("out/1. Exploratory - summary.csv")
  file.create(target)
  write.csv2(to_write, target)
  #
  to_write <- z.a.util.gen.summary(allnospam, "Diff", allspam)
  target <- file.path("out/1. Exploratory - summary - compare.csv")
  file.create(target)
  write.csv2(to_write, target)

  cat("Summary written\n")
}

#' @title Exploratory - Correlations
#' @description To get (back) to the overview of all steps and functions use this link: \code{\link{a.a.main}}
#'
#' Creates three plots with corrplot funtion from corrplot package.
#'
#' The plots show correlations between the dimension of spam and non-spam mail and the combined
#' observations.
#'
#' The file is named: "out/1. Exploratory - Correlation matrix.pdf"
#'
#' @author Vitali Friesen
b.d.exploratory.correlations <- function(){
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

  cat("Correlations plotted\n")
}

#' @title Exploratory - Dimension Visualization
#' @description To get (back) to the overview of all steps and functions use this link: \code{\link{a.a.main}}
#'
#' This function simply creates several plots to better understand the dimensions of the data.
#'
#' First plot is a barplot of the class dimension to grasp the amount of non-spam and spam
#' classifications. The plot can be find in "out/1. Exploratory - Barplot of email numbers.pdf"
#'
#' The second plot creates Histograms for each dimension to show the distributions of observations.
#' It is stored in: "out/1. Exploratory - Histograms.pdf"
#'
#' The third plot contains the scatterplots of all pairs of dimensions which had a correlation of
#' at least 0.2 in descending order which puts the plots of dimensions with the highest correlation
#' to the top.
#' The result is stored in: "out/1. Exploratory - Scatterplots of highly correlated dimensions.pdf"
#'
#' @author Vitali Friesen
b.e.exploratory.dim_vis <- function(){
  # barplot of single logical dimension "class"
  ggplot2::ggsave(ggplot2::ggplot() + ggplot2::aes(spambase[,58]) +
                    ggplot2::geom_bar(fill=ercis.red) + ggplot2::labs(x="Number of Mails") +
                    ggplot2::theme_bw(base_size = 12, base_family = ""),
                  file="out/1. Exploratory - Barplot of email numbers.pdf")

  # Create histogram plot of nospam and spam emails for each dimension + p-value
  p <- lapply(names(noclasses), function(column){
    ggplot2::ggplot(spambase) + ggplot2::aes(x=noclasses[,column]) +
      ggplot2::labs(x=paste0("Histogram of: ", column)) +
      ggplot2::geom_histogram(binwidth = max(noclasses[,column])/100, fill=ercis.red) +
      ggplot2::theme_bw(base_size = 18, base_family = "") +
      ggplot2::annotate("text",
                        label = paste0("p-value: ", shapiro.test(spambase[,column])$p.value),
                        x=Inf,y=Inf, vjust=1, hjust=1, size = 9, colour = ercis.red)
  })
  pdf(file.path("out/1. Exploratory - Histograms.pdf"))
  sapply(p, function(plot_i){
    print(plot_i)
  })
  dev.off()

  # create scatterplots of highly correlated dimensions
  # get sorted list of correlations
  corlist <- cor(noclasses)
  corlist[lower.tri(corlist,diag=TRUE)]=NA  #Prepare to drop duplicates and meaningless information
  corlist=as.data.frame(as.table(corlist))  #Turn into a 3-column table
  corlist=na.omit(corlist)                  #Get rid of the junk we flagged above
  corlist=corlist[order(-abs(corlist$Freq)),] #Sort by highest correlation (whether +ve or -ve)
  p <- lapply(1:nrow(corlist[corlist$Freq > 0.2,]), function(rowID){
    ggplot2::ggplot(noclasses) +
      ggplot2::annotate("text", label = paste0("cor: ",round(corlist[rowID,"Freq"],2)),x=Inf,y=Inf,
                        vjust=1, hjust=1, size = 9, colour = ercis.red) +
      ggplot2::aes(x=noclasses[,corlist[rowID,"Var1"]], y=noclasses[,corlist[rowID,"Var2"]]) +
      ggplot2::labs(x=corlist[rowID,"Var1"],y=corlist[rowID,"Var2"]) +
      ggplot2::geom_point() + ggplot2::theme_bw(base_size = 12, base_family = "")
  })
  pdf(file.path("out/1. Exploratory - Scatterplots of highly correlated dimensions.pdf"))
  sapply(p, function(plot_i){
    print(plot_i)
  })
  dev.off()

  cat("Dimension visualizations plottet\n")
}
