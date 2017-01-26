#' Main function which guides through the data analysis.
#'
#' @export
a.spambase.analysis <- function(){

  message("1. Exploratory data analysis")
  b.exploratory.data_analysis()
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
  pca()
  cat("\n")

  message("7. Fuzzy C-Means clustering")
  clustering()
  cat("\n")
}
