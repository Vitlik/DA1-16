
#' @title Outlier Detection
#' @description To get (back) to the overview of all steps and functions use this link: \code{\link{a.a.main}}
#'
#' Starts the steps for finding outliers.
#'
#' Outliers are observation which fall off far from other observations and can lower the quality of
#' different analyses tool results.
#'
#' Outliers can occur in any distribution by chance but they can also come due to different
#' technical errors like measurement error of sensor or errors on the harddrive.
#' The latter ones can disturb method to find the distributions for given data while the former is
#' a legitimate part of the observations.
#' Unfortunately these cannot be distinguished
#'
#' Nevertheless outliers
#'
#' @author Vitali Friesen (structure)
d.a.outlier_detection <- function(){
  #
  # outlier.scores <- DMwR::lofactor(noclasses, k=5)
  # outliers <- order(outlier.scores, decreasing=T)[1:(nrow(noclasses)*0.01)]
  #
  #
  #
  # outlier.scores <- Rlof::lof(noclasses, k=c(5:10))
  # outliers <- order(outlier.scores, decreasing=T)[1:(nrow(noclasses)*0.01)]
}
