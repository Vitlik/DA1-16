#' Main function which guides through the data analysis.
#'
#' @export
a.spambase.analysis <- function(){

  message("1. Exploratory data analysis")
  b.a.exploratory.data_analysis()
  cat("\n")

  message("2. Normality check")
  c.a.normality.check()
  cat("\n")

  message("3. Transformation to normality")
  c.b.normality.transform()
  cat("\n")

  message("4. Outlier detection")
  d.a.outlier_detection()
  cat("\n")

  message("5. Principal Component Analysis")
  e.a.pca()
  cat("\n")

  message("7. K-Means & Fuzzy C-Means clustering")
  f.a.clustering()
  cat("\n")
}
