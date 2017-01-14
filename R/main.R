#' Main function which guides through the data analysis.
#'
#' @export
spambase.analysis <- function(){

  message("1. Exploratory data analysis")
  exploratory_data_analysis()
  cat("\n")

  message("2. Normality check")
  normality.check()
  cat("\n")

  message("3. Transformation to normality")
  normality.transform()
  cat("\n")

  message("4. Outlier detection")
  outlier_detection()
  cat("\n")

  message("5. Principal Component Analysis")
  dim.pca()
  cat("\n")

  message("6. Multidimensional scaling")
  dim.mds()
  cat("\n")

  message("7. Fuzzy C-Means clustering")
  clustering()
  cat("\n")
}
