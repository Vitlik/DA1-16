library(ggplot2)
library(ggfortify)
library(methods)
library(foreign)
library(cluster)
#install.packages('calibrate')
library(MASS)
library(calibrate)
install.packages('DMwR')
library(grid)
library(DMwR)




#cor.mat<-cor(spambase[,1:48], method = 'pearson')
#comp.mat<-prcomp(cor.mat, scale. = F) ## should we really scale the data?
spambase.48<-spambase[,1:48]


##outlier detection with lofactor

spambase.scaled<-scale(spambase[1:57], center=TRUE, scale=TRUE)
lof.det<-lofactor(spambase.scaled,k=5)

gg.df<-data.frame(names=as.factor(1:nrow(spambase)), lof.det)
ggplot(gg.df, aes(x=gg.df[,1], y=gg.df[,2], col="black", label=gg.df[,1]))+
  geom_point(col='black', pch=1) +
  geom_text(aes(label=ifelse(gg.df[,2]>(mean(lof.det)+3.5*sd(lof.det)),as.character(gg.df[,1]),'')),hjust=0,vjust=0)

# we leave out outliers in the dataset
spambase.scaled.out<-spambase.scaled[-which(lof.det>(mean(lof.det)+3.5*sd(lof.det))),]


## normal plot
names<-names(spambase.48)
opar=par(mfrow=c(2,2))
Index<-1:nrow(spambase)
for(n in 1:12){
  start<-(n-1)*4+1
  for(ii in start:min(start+3,48)){
    assign(names[ii],spambase[ii])
    plot.df<-data.frame(Index,get(names[ii]))
    plot(spambase[,ii], ylab = colnames(spambase)[ii])
    textxy(X=plot.df[,1], Y=plot.df[,2], seq(1:4800))
    abline(h=5*sqrt(var(plot.df[,2])))
    remove(names[ii])
  }
}

## more clear but less effective way to plot outliers
gg.df<-data.frame(names=as.factor(seq(1,nrow(spambase))), make=spambase[,1])
ggplot(gg.df, aes(x=gg.df[,1], y=gg.df[,2], colour="black", label=gg.df[,1]))+
  geom_point() +
  geom_text(aes(label=ifelse(gg.df[,2]>3*sqrt(var(gg.df[,2])),as.character(gg.df[,1]),'')),hjust=0,vjust=0)+
  abline(h=5*sqrt(var(gg.df[.,2])))

## QQ-plot
qqplot(y=spambase[,1], plot.it = TRUE, distribution= ppois(spambase[,1],lambda=1),sort(spambase[,1]))

library(lattice)
qqmath(spambase[,1],
       distribution = ppois(q=seq(1:4610),lambda = 1))



## Decide how many principal components do we need
res.pca<-princomp(spambase.scaled.out, cor = TRUE, scores = TRUE)
#res.pca1<-prcomp(spambase.48, scale. = TRUE)
cumsum(res.pca$sdev)/sum(res.pca$sdev) # the explained ration should be estimated by the eigenvalues or the sum of squared eigenvalues? Because the squared value would be then the variance, which is applied in the screediagram
(pc.no<-sum(cumsum(res.pca$sdev)/sum(res.pca$sdev)<=0.9)+1)
screeplot(res.pca, npcs = 10)
autoplot(res.pca,label=TRUE,loadings=TRUE,loadings.label=TRUE)
# ## delete undesired words
#
# drops<-c('you','your','george','hp')
# spambase.48<-spambase[,1:48]
# spambase.word.new<-spambase.48[,!(names(spambase.48)%in%drops)]
# spambase.word.new<-data.frame(spambase.new)
#
# ## conduct the PCA again
# cor.mat.new<-cov(spambase.word.new,method = 'pearson')
# comp.mat.new<-prcomp(cor.mat.new,scale. = F) ## necessary to scale?
# cumsum(comp.mat.new$sdev^2)/sum(comp.mat.new$sdev^2)
# sum(cumsum(comp.mat.new$sdev^2)/sum(comp.mat.new$sdev^2)<=0.9)+1
# screeplot(comp.mat.new, npcs = 10)
# autoplot(comp.mat.new,label=TRUE,loadings=TRUE,loadings.label=TRUE)
# ##
#comp.mat.extr<-comp.mat$rotation[,1:27] ## should we simply apply the $rotation matrix?


## Combine variables based on factor sores

## find out variables with high factor loadings (threshold=0.4)
# comp.mat.extr.high<-comp.mat.extr[which(apply(abs(comp.mat.extr),1,max)>0.4),]
# dim(comp.mat.extr.high) # really only consider those variables with high loadings?

## detect which variables cross-load on both PCs
#sum((apply(abs(comp.mat.extr),1,min)>0.4))

## which variables load a lot on PC1 and PC2. Eliminate those cross-loading variables?
# PC1.var<-comp.mat.extr[which(abs(comp.mat.extr[,1])>0.4),1]
# PC2.var<-comp.mat.extr[which(abs(comp.mat.extr[,2])>0.4),2]
# length(PC1.var)+length(PC2.var)

## calculate the scores of PCs

# PCs<-paste0(rep('PC',27), 1:27)
#
# for(ii in 1:27){
#   assign(PCs[ii], apply(spambase.48[,which(abs(comp.mat.extr[,ii])>0.1)],1,mean))
# }
#
# score.PC<-mget(PCs)
# score.PC<-do.call(cbind, score.PC)
# remove(list = PCs)

## use predict function to test the PCs for new data


## Clustering
clara<-clara(res.pca$scores[,1:pc.no], metric = 'euclidean', k=1, samples = 100, sampsize = nrow(res.pca$scores)/100)
autoplot()

## hierachical clustering

methods = c("average", "single", "complete","ward.D")
opar = par(mfrow = c(1))
sapply(methods, function(method) {
  res.clust = hclust(dist(res.pca$scores[,1:pc.no]), method = 'ward.D')
  plot(res.clust, main = sprintf("method: %s", method))
})
par(opar)

## K-means clustering
#initial.centers<-matrix(c(-0.05,0,0.05,0.05),byrow = TRUE, ncol = 2L)

k.cluster<-kmeans(res.pca$scores[,1:pc.no] , centers= 2, nstart = pc.no, algorithm = 'Lloyd',iter.max = 50)
sum(k.cluster$cluster-1==1)/length(k.cluster$cluster)
sum(k.cluster$cluster-1==spambase[rownames(spambase.scaled.out), 'class'])/length(k.cluster$cluster)


## cluster raw data
k.cluster.raw<-kmeans(spambase[,1:48], centers= 2, nstart = 20, algorithm = 'Lloyd',iter.max =40)
sum(k.cluster.raw$cluster-1==1)/length(k.cluster.raw$cluster)
sum(k.cluster.raw$cluster-1==spambase[, 'class'])/nrow(spambase)
## cluster scaled raw data
k.cluster.raw<-kmeans(spambase.scaled[,1:57], centers= 2, nstart = 20, algorithm = 'Lloyd',iter.max =50)
sum(k.cluster.raw$cluster-1==1)/length(k.cluster.raw$cluster)
sum(k.cluster.raw$cluster-1==spambase[, 'class'])/nrow(spambase)
## cluster data after leaving out outliers
k.cluster.raw<-kmeans(spambase.scaled.out[,1:57], centers= 2, nstart = pc.no, algorithm = 'Lloyd',iter.max =50)
sum(k.cluster.raw$cluster-1==1)/length(k.cluster.raw$cluster)
sum(k.cluster.raw$cluster-1==spambase[rownames(spambase.scaled.out), 'class'])/length(k.cluster.raw$cluster)

## cmeans clustering

c.cluster<-cmeans(spambase.scaled.out[,1:48], iter.max = 20, centers = 2, dist = 'euclidean', method = 'cmeans', m = 2)
sum(c.cluster$cluster-1!=spambase[rownames(spambase.scaled.out), 'class'])/length(k.cluster.raw$cluster)


colnames(spambase.scaled.out.rename)<-names
autoplot(object=k.cluster.raw, data=spambase.scaled.out.rename)

ggplot(spambase.scaled.out, aes(Petal.Length, Petal.Width, color = Species)) + geom_point()

plotWSSVsK = function(data) { n = nrow(data) # determine possible numbers for K*
Ks = seq(n - 1L)
# WSS for the actual data
tot.wss = sapply(Ks, function(k) {
  kmeans(data, centers = k, algorithm = "Lloyd", iter.max = 50)$tot.withinss
})
# WSS for the uniformaly generated data
unif.data = matrix(
  runif(2 * n, min = min(data[, 1]),
        max = max(data[, 2])), ncol = 2)
exp.tot.wss = sapply(Ks, function(k) {
  kmeans(unif.data, centers = k)$tot.withinss })
# actually draw the plot
plot(Ks, tot.wss, type = "b", col = "red",
     xlab = "Number of clusters",
     ylab = expression(W[k]))
lines(Ks, exp.tot.wss, col = "blue")
points(Ks, exp.tot.wss, col = "blue")
}

plotWSSVsK(res.pca$scores[,1:pc.no])

plot(cor.prc.new, label)
points(k.cluster$centers, col='red')


res.cluster<-which(k.cluster$cluster == 1)

## validation of our cluster with the given classification (note that they have a 7% error range)
sum(k.cluster$cluster==spambase[, 'class'])/nrow(spambase)
sum(k.cluster.raw$cluster==spambase[, 'class'])/nrow(spambase)
sum(k.cluster.raw$cluster!=spambase[rownames(spambase.scaled.out), 'class'])/nrow(spambase)

c.cluster<-cmeans(spambase.scaled.out, iter.max = 20, centers = 2, dist = 'euclidean', method = 'cmeans', m = 2)
sum(c.cluster$cluster-1==spambase[rownames(spambase.scaled.out), 'class'])/nrow(spambase)
