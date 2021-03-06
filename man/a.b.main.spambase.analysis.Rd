% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/main.R
\name{a.b.main.spambase.analysis}
\alias{a.b.main.spambase.analysis}
\title{MAIN - Spambase Analysis}
\usage{
a.b.main.spambase.analysis()
}
\description{
To get (back) to the overview of all steps and functions use this link:
\code{\link{a.a.main}}

Meta function for starting meta steps of the analysis.

This function wraps all the meta steps for analyzing the data.
\enumerate{
  \item Load environment (\code{\link{z.z.util.set.environment}})
  \item Explore basic structure of the data (\code{\link{b.a.exploratory.data_analysis}})
  \item Check for normality in the data and try to transform to it
  (\code{\link{c.a.normality}})
  \item Search for possible outliers (\code{\link{d.a.outlier_detection}})
  \item Try to reduce dimensionality with principal component analysis
  (\code{\link{e.a.pca}})
  \item Cluster data to detect similarites in spam mail and in non-spam mails
  (\code{\link{f.a.clustering}})
}

Each step will be further explained in the corresponding function descriptions
}
\author{
Vitali Friesen
}

