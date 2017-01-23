cor.mat<-cor(spambase[,1:48], method = 'pearson')
comp.mat<-prcomp(cor.mat, scale. = F) ## should we really scale the data?

## Decide how many principal components do we need

cumsum(comp.mat$sdev^2)/sum(comp.mat$sdev^2)
sum(cumsum(comp.mat$sdev^2)/sum(comp.mat$sdev^2)<=0.9)+1
screeplot(comp.mat, npcs = 10)
autoplot(comp.mat,label=TRUE,loadings=TRUE,loadings.label=TRUE)
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
comp.mat.extr<-comp.mat$rotation[,1:27] ## should we simply apply the $rotation matrix?

# when including other variables such as "length of capital letters", we need to standardize the scales

## Combine variables based on factor sores

## find out variables with high factor loadings (threshold=0.4)
comp.mat.extr.high<-comp.mat.extr[which(apply(abs(comp.mat.extr),1,max)>0.4),]
dim(comp.mat.extr.high) # really only consider those variables with high loadings?

## detect which variables cross-load on both PCs
sum((apply(abs(comp.mat.extr),1,min)>0.4))

## which variables load a lot on PC1 and PC2. Eliminate those cross-loading variables?
PC1.var<-comp.mat.extr[which(abs(comp.mat.extr[,1])>0.4),1]
PC2.var<-comp.mat.extr[which(abs(comp.mat.extr[,2])>0.4),2]
length(PC1.var)+length(PC2.var)

## calculate the scores of PCs

PCs<-paste0(rep('PC',27), 1:27)

for(ii in 1:27){
  assign(PCs[ii], apply(spambase.48[,which(abs(comp.mat.extr[,ii])>0.1)],1,mean))
}

score.PC<-mget(PCs)
score.PC<-do.call(cbind, score.PC)
remove(list = PCs)

## use predict function to test the PCs for new data


## Clustering
autoplot(clara(score.PC, metric = 'euclidean', k=2))

## hierachical clustering

methods = c("average", "single", "complete","ward.D")
opar = par(mfrow = c(1))
sapply(methods, function(method) {
  res.clust = hclust(dist(score.PC), method = 'ward.D')
  plot(res.clust, main = sprintf("method: %s", method))
})
par(opar)

## K-means clustering
initial.centers<-matrix(rnorm(0,54),byrow = TRUE, ncol = 2L)

k.cluster<-kmeans(score.PC , centers= initial.centers, algorithm = 'Lloyd',iter.max = 25)

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

plotWSSVsK(score.PC)

plot(cor.prc.new, label)
points(k.cluster$centers, col='red')


res.cluster<-which(k.cluster$cluster == 1)

