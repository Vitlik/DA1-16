
#' @title Clustering
#' @description To get (back) to the overview of all steps and functions use this link:
#' \code{\link{a.a.main}}
#'
#' This part start the clustering analysis
#'
#' Two functions are executed here:
#' \enumerate{
#'   \item \code{\link{f.b.hierarchical}}
#'   \item \code{\link{f.c.kmeans}}
#'   \item \code{\link{f.d.cmeans}}
#'   \item \code{\link{f.e.plotWSSVsK}}
#' }
#'
#' @author Lijin Lan, Evelyn Navarrete
f.a.clustering <- function(spambase.scaled.out, spambase.scaled.new){

  f.b.hierarchical(spambase.scaled.out)

  f.c.kmeans(spambase.scaled.out, spambase.scaled.new)

  f.d.cmeans(spambase.scaled.out, spambase.scaled.new)

  f.f.plotWSSVsK(spambase.scaled.out)

}

#' @title Clustering - Hierarchical
#' @description To get (back) to the overview of all steps and functions use this link:
#' \code{\link{a.a.main}}
#'
#'
#'
#' @author Lijin Lan
f.b.hierarchical <- function(spambase.scaled.out){
  methods = c("single","ward.D")
  opar = par(mfrow = c(1,2))
  sapply(methods, function(method) {
    res.clust = hclust(dist(spambase.scaled.out), method = methods)
    plot(res.clust, main = sprintf("method: %s", method))
  })
  par(opar)
}

#' @title Clustering - Hierarchical
#' @description To get (back) to the overview of all steps and functions use this link:
#' \code{\link{a.a.main}}
#'
#'
#'
#' @author Lijin Lan
f.c.kmeans <- function(spambase.scaled.out){
  # K-means clustering
  k.cluster<-kmeans(spambase.scaled.new, centers= 2, nstart = 40, algorithm = 'Lloyd',
                    iter.max = 50)
  sum(k.cluster$cluster-1==1)/length(k.cluster$cluster)
  sum(k.cluster$cluster-1!=spambase[rownames(spambase.scaled.out),
                                    'class'])/length(k.cluster$cluster)
}

#' @title Clustering - Hierarchical
#' @description To get (back) to the overview of all steps and functions use this link:
#' \code{\link{a.a.main}}
#'
#'
#'
#' @author Lijin Lan
f.d.cmeans <- function(spambase.scaled.out, spambase.scaled.new){
  # C-means clustering
  c.cluster <- e1071::cmeans(spambase.scaled.new, iter.max = 20, centers = 2,
                             dist = 'euclidean', method = 'cmeans', m = 1.2)
  sum(c.cluster$cluster-1==spambase[rownames(spambase.scaled.out), 'class'])/nrow(spambase)
}



#' @title Clustering - plotWSSVsK
#' @description To get (back) to the overview of all steps and functions use this link:
#' \code{\link{a.a.main}}
#'
#' Plot within sum of squares vs k
#'
#' @author Lijin Lan
f.e.plotWSSVsK <- function(data) {
  n = nrow(data) # determine possible numbers for K*
  Ks = 1:20
  # WSS for the actual data
  tot.wss = sapply(Ks, function(k) {
    kmeans(data, centers = k, algorithm = "Lloyd", iter.max = 70)$tot.withinss
  })

  # WSS for the uniformaly generated data
  unif.data = matrix(runif(2 * n, min = min(data[, 1]), max = max(data[, 2])), ncol = 2)
  exp.tot.wss = sapply(Ks, function(k) {
    kmeans(unif.data, centers = k)$tot.withinss })

  # actually draw the plot
  plot(Ks, tot.wss, type = "b", col = "red",
       xlab = "Number of clusters",
       ylab = expression(W[k]))
  lines(Ks, exp.tot.wss, col = "blue")
  points(Ks, exp.tot.wss, col = "blue")
}
