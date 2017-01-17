# install.packages('ggplot2')
# install.packages('ggfortify')
# install.packages('methods')
# install.packages('cluster')
library(ggplot2)
library(ggfortify)
library(methods)
library(foreign)
library(cluster)

raw.data<-read.arff(file = 'E:/OneDrive/WWU Münster/Data Analytics/DA1 Project/dataset_44_spambase_adjusted.arff')
raw.data<-raw.data
dim(raw.data)



cor.mar<-cor(raw.data[,1:48], method = 'pearson')

# test the cor.mar in SPSS
write.foreign(cor.mar, datafile = 'E:\\OneDrive\\WWU Münster\\Data Analytics\\DA1 Project\\DA1 Project\\cor.mar.txt',
              codefile='E:\\OneDrive\\WWU Münster\\Data Analytics\\DA1 Project\\DA1 Project\\cor.mar.sps',package="SPSS")
write.table(cor.mar, file = 'E:\\OneDrive\\WWU Münster\\Data Analytics\\DA1 Project\\DA1 Project\\cor.mar.txt', sep='\t')
################
print(autoplot(cor.mar))
cor.prc<-prcomp(cor.mar) # prcomp is the function to derive principal component from a correlation matrix

##to check the eigenvalues of pricipal components

apply(apply(prc$loadings,c(1,2),function(x){x^2}), 2, sum)

cor.prc<-prcomp(cor.mar, retx=TRUE)

apply(apply(cor.prc$rotation,c(1,2),function(x){x^2}), 2, sum)
barplot(apply(cor.prc$rotation,c(1,2),function(x){x^2})[,'PC1'])
sort(cor.prc$sdev^2, decreasing=TRUE)

apply(apply(cor.prc$x,c(1,2),function(x){x^2}), 2, sum)# X is the matrix of final resutls,
#in which the raw data abstracted the means are multiplied by the eigenvectors. I.e. displayed on the chosen dimensioins


#### Decide how many principal components do we need

cumsum(cor.prc$sdev^2)/sum(cor.prc$sdev^2)

sum(cumsum(cor.prc$sdev^2)/sum(cor.prc$sdev^2)<=0.5)+1


screeplot(cor.prc, npcs = 10)


autoplot(cor.prc,label=TRUE,loadings=TRUE,loadings.label=TRUE, )

com.matrix<-cor.prc$x[,c('PC1','PC2')]

## when including other variables such as "length of capital letters", we need to standardize the scales

## Combine variables based on factor sores

## find out variables with high factor loadings (threshold=0.4)
com.matrix.extr<-com.matrix[which(apply(abs(com.matrix),1,max)>0.4),]

dim(com.matrix.extr)

## detect which variables cross-load on both PCs
sum((apply(abs(com.matrix),1,min)>0.4))

## which variables load a lot on PC1 and PC2. Eliminate those cross-loading variables?
PC1.var<-com.matrix[which(abs(com.matrix[,1])>0.4),1]
PC2.var<-com.matrix[which(abs(com.matrix[,2])>0.4),2]
length(PC1.var)+length(PC2.var)

## calculate the scores of PC1 and PC2
data.PC1<-apply(raw.data[,which(abs(com.matrix[,1])>0.4),1],1,mean)
data.PC2<-apply(raw.data[,which(abs(com.matrix[,2])>0.4),2],1,mean)

data.PC<-cbind(data.PC1, data.PC2)


## use predict function to test the PCs for new data


## Clustering
plot(data.PC)
autoplot(clara(data.PC, metric = 'euclidean', k=2))

## hierachical clustering

methods = c("average", "single", "complete","ward.D")
opar = par(mfrow = c(2, 2))
sapply(methods, function(method) {
  res.clust = hclust(dist(data.PC), method = method)
  plot(res.clust, main = sprintf("method: %s", method))
  })
par(opar)

## K-means clustering
initial.centers<-matrix(c(-0.5,0,1.5,0.1),byrow = TRUE, ncol = 2L)

k.cluster<-kmeans(data.PC, centers= initial.centers, algorithm = 'Lloyd',iter.max = 25)

plotWSSVsK = function(data) { n = nrow(data) # determine possible numbers for K*
  Ks = seq(n - 1L)
  # WSS for the actual data
  tot.wss = sapply(Ks, function(k) {
  kmeans(data, centers = k, algorithm = "Lloyd", iter.max = 20)$tot.withinss
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

plotWSSVsK(data.PC)

plot(cor.prc.new, label)
points(k.cluster$centers, col='red')


res.cluster<-which(k.cluster$cluster == 1)

