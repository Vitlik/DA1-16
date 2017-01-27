
#' @title Exploratory data analysis wrapper function
#' @description This is a wrapper function for the whole exploratory data analysis step
b.exploratory.data_analysis <- function(){
  # structure the data into the groups it contains
  b.exploratory.structure_data()

  # write structure and summary information to files
  b.exploratory.str_summary()
  cat("Summary written\n")

  #
  b.exploratory.correlations()
  cat("Correlations plotted\n")

  #
  b.exploratory.dim_vis()
  cat("Dimension visualizations plottet\n")
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
  # barplot of single logical dimension "class"
  ggplot2::ggsave(ggplot2::ggplot() + ggplot2::aes(spambase[,58]) +
                    ggplot2::geom_bar(fill=ercis.red) + ggplot2::labs(x="Number of Mails") +
                    ggplot2::theme_bw(base_size = 12, base_family = ""),
                  file="out/1. Exploratory - Barplot of email numbers.pdf")

  # Create histogram plot of nospam and spam emails for each dimension
  p <- lapply(names(noclasses), function(column){
    ggplot2::ggplot(spambase) + ggplot2::aes(x=noclasses[,column]) +
      ggplot2::labs(x=paste0(column," : NoSpam vs. Spam")) +
      ggplot2::geom_histogram(binwidth = max(noclasses[,column])/100, fill=ercis.red) +
      ggplot2::theme_bw(base_size = 12, base_family = "") + ggplot2::facet_grid(. ~ spambase[,58])
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
                        vjust=1, hjust=1, size = 6, colour = ercis.red) +
      ggplot2::aes(x=noclasses[,corlist[rowID,"Var1"]], y=noclasses[,corlist[rowID,"Var2"]]) +
      ggplot2::labs(x=corlist[rowID,"Var1"],y=corlist[rowID,"Var2"]) +
      ggplot2::geom_point() + ggplot2::theme_bw(base_size = 12, base_family = "")
  })
  pdf(file.path("out/1. Exploratory - Scatterplots of highly correlated dimensions.pdf"))
  sapply(p, function(plot_i){
    print(plot_i)
  })
  dev.off()
}
