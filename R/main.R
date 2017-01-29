#' Main function which guides through the data analysis.
#'
#' @export
a.spambase.analysis <- function(){

  z.z.set_environment()

  message("1. Exploratory data analysis")
  b.a.exploratory.data_analysis()
  cat("\n")

  message("2. Normality")
  c.a.normality()
  cat("\n")

  message("3. Outlier detection")
  d.a.outlier_detection()
  cat("\n")

  message("4. Principal Component Analysis")
  e.a.pca()
  cat("\n")

  message("5. K-Means & Fuzzy C-Means clustering")
  #f.a.clustering()
  cat("\n")
}
