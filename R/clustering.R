
#' @title Clustering
#' @description
#'
#' @author Lijin Lan, Evelyn Navarrete
f.a.clustering <- function(){





  plotWSSVsK(spambase.scaled.out)


}

# plot within sum of squares vs k
plotWSSVsK = function(data) {
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
