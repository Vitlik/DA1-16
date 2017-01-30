library(grid)
library(DMwR)
library(ggplot2)
library(e1071)
##outlier detection with lofactor
spambase.scaled<-scale(spambase[1:57], center=TRUE, scale=TRUE)
spambase.scaled<-data.frame(spambase.scaled)

# inspect whether there are raws with only zeros
sum(apply(spambase.scaled,1,mean)==0)

lof.det<-lofactor(spambase.scaled,k=100)


# plot the outliers and their IDs
gg.df<-data.frame(names=as.factor(1:nrow(spambase)), lof.det)
ggplot(gg.df, aes(x=gg.df[,1], y=gg.df[,2], col="black", label=gg.df[,1]))+
  geom_point(col='black', pch=1) +
  geom_text(aes(label=ifelse(gg.df[,2]>(mean(lof.det)+3.5*sd(lof.det)),as.character(gg.df[,1]),'')),hjust=0,vjust=0)

# leave out outliers of the dataset
spambase.scaled.out<-spambase.scaled[-which(lof.det>(mean(lof.det)+3.5*sd(lof.det))),]

## combine higly correlated variables with PCA and factor analysis

# create a correlation matrix
cor.mat<-cor(spambase.scaled.out)

# set the upper triangle of the matrix to zero
cor.mat[upper.tri(cor.mat, diag = TRUE)]<-0

# find out which variables have a correlation coefficients greater than 0.5
high.c<-which(cor.mat>0.5, arr.ind = TRUE)
rnames<-rownames(cor.mat)[high.c[,1]]
cnames<-colnames(cor.mat)[high.c[,2]]
high.c<-cbind(rnames,cnames)

# inspect the number of latent factors
res.pca<-princomp(spambase.scaled.out)
screeplot(res.pca, npcs=57)

# conduct the factor analysis and check out the loadings
spam.factor<-factanal(spambase.scaled.out, factors = 4)

# check which variables highly load on a factor (>0.4)
spam.factor$loadings
factor1.variable<-which(abs(spam.factor$loadings[,1])>0.4, arr.ind = TRUE)
factor2.variable<-which(abs(spam.factor$loadings[,2])>0.4, arr.ind = TRUE)# there is only one variable, therefore no difference
factor3.variable<-which(abs(spam.factor$loadings[,3])>0.4, arr.ind = TRUE)
factor.variable<-c(factor1.variable, factor2.variable, factor3.variable)

# calculate the scores of factor based on the highly loading variables
factor1<-apply(spambase.scaled.out[,which(abs(spam.factor$loadings[,1])>0.4)],1,mean)
factor2<-spambase.scaled.out[,which(abs(spam.factor$loadings[,2])>0.4)]
factor3<-apply(spambase.scaled.out[,which(abs(spam.factor$loadings[,3])>0.4)],1,mean)
factor.score<-cbind(factor1, factor2, factor3)

# replace variables with factors in the dataset
spambase.scaled.new<-spambase.scaled.out[,-factor.variable]
spambase.scaled.new<-cbind(spambase.scaled.new, factor.score)

## clustering

# hierachical clustering
methods = c("single","ward.D")
opar = par(mfrow = c(1,2))
sapply(methods, function(method) {
  res.clust = hclust(dist(spambase.scaled.out), method = methods)
  plot(res.clust, main = sprintf("method: %s", method))
})
par(opar)
# K-means clustering
k.cluster<-kmeans(spambase.scaled.new, centers= 2, nstart = 40, algorithm = 'Lloyd',iter.max = 50)
sum(k.cluster$cluster-1==1)/length(k.cluster$cluster)
sum(k.cluster$cluster-1!=spambase[rownames(spambase.scaled.out),'class'])/length(k.cluster$cluster)

# C-means clustering
c.cluster<-cmeans(spambase.scaled.new, iter.max = 20, centers = 2, dist = 'euclidean', method = 'cmeans', m = 2)
sum(c.cluster$cluster-1==spambase[rownames(spambase.scaled.out), 'class'])/nrow(spambase)


# plot within sum of squares vs k
plotWSSVsK = function(data) {
  n = nrow(data) # determine possible numbers for K*
  Ks = seq(n - 1L)
# WSS for the actual data
  tot.wss = sapply(Ks, function(k) {
  kmeans(data, centers = k, algorithm = "Lloyd", iter.max = 20)$tot.withinss
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

plotWSSVsK(spambase.scaled.out)
