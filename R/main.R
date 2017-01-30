#' @title Function for introducing package content
#'
#' @description The task of this function is to start the whole analyzation process of the \href{http://www.openml.org/d/44}{spambase data}. The analyzation is done several steps which can be seen in the following listing:
#'
#' \enumerate{
#'   \item main
#'     \enumerate{
#'       \item \code{\link{a.a.main}}
#'       \item \code{\link{a.b.main.spambase.analysis}}
#'   }
#'   \item exploratory
#'     \enumerate{
#'       \item \code{\link{b.a.exploratory.data_analysis}}
#'       \item \code{\link{b.b.exploratory.structure_data}}
#'       \item \code{\link{b.c.exploratory.str_summary}}
#'       \item \code{\link{b.d.exploratory.correlations}}
#'       \item \code{\link{b.e.exploratory.dim_vis}}
#'   }
#'   \item normality
#'     \enumerate{
#'       \item \code{\link{c.a.normality}}
#'       \item \code{\link{c.b.qqplots}}
#'       \item \code{\link{c.c.chisquare}}
#'       \item \code{\link{c.d.distribution_test}}
#'       \item \code{\link{c.e.norm_transform}}
#'   }
#'   \item outlier detection
#'     \enumerate{
#'       \item \code{\link{d.a.outlier_detection}}
#'   }
#'   \item outlier detection
#'     \enumerate{
#'       \item \code{\link{e.a.pca}}
#'   }
#'   \item outlier detection
#'     \enumerate{
#'       \item \code{\link{f.a.clustering}}
#'   }
#' }
#' Further more there is the util script for supporting the other scritps:
#' \itemize{
#'   \item \code{\link{z.a.util.gen.summary}}
#'   \item \code{\link{z.b.util.diff}}
#'   \item \code{\link{z.c.boxcox}}
#'   \item \code{\link{z.d.loglik}}
#'   \item \code{\link{z.z.util.set.environment}}
#' }
#'
#' Each of clickable listing elements are functions in the package which have a specific task.
#'
#' A separate project documentation was foregone to avoid duplicate documentation. Therefore this documentation is extended to take that into account.
#'
#' The naming convention with LETTER.LETTER.FUNCTIONNAME has solely the purpose to ensure the correct ordering of the functions in the generated pdf of the documentation which was done by roxygen2. Otherwise the order of function and project descriptions may not follow the linear process of the steps and makes the understanding more difficult.
#'
#' @author Vitali Friesen
#' @export
a.a.main <- function(){
  a.b.main.spambase.analysis()
}

#' @title Meta function for starting meta steps of the analysis
#' @description To get (back) to the overview of all steps and functions use this link: \code{\link{a.a.main}}
#'
#' This function wraps all the meta steps for analyzing the data.
#' \enumerate{
#'   \item Explore basic structure of the data
#'   \item Check for normality in the data and try to transform to it
#'   \item Search for possible outliers
#'   \item Try to reduce dimensionality with principal component analysis
#'   \item Cluster data to detect similarites in spam mail and in non-spam mails
#' }
#'
#' Each step will be further explained in the corresponding function descriptions
#'
#' @author Vitali Friesen
a.b.main.spambase.analysis <- function(){

  message("0. Loading environment")
  z.z.util.set.environment()
  cat("\n")

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
