tempo <- function(){

spambase.dupli <- spambase[!duplicated(spambase),]

#### factor
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


### logistic regression
class<-spambase[,58]
class<-class[-which(lof.det>(mean(lof.det)+3.5*sd(lof.det)))]

spambase.scaled.new<-spambase.scaled.out[,-factor.variable]
spambase.scaled.new<-cbind(spambase.scaled.new, factor.score, class)
spambase.scaled.new<-as.data.frame(spambase.scaled.new)

# replace variables with factors in the dataset
spambase.scaled.new<-spambase.scaled.out[,-factor.variable]
spambase.scaled.new<-cbind(spambase.scaled.new, factor.score)

## logistic regression

lr.model<-glm(class~., family = binomial(link = 'logit'), data = spambase.scaled.new, maxit=50)
lr.summary<-summary(lr.model)

relevant.v<-colnames(spambase.scaled.new)[lr.summary$coefficients[2:nrow(lr.summary$coefficients),
                                                                  4] < 0.05]
relevant.v<-relevant.v[!relevant.v=='class']
spambase.scaled.new.lr<-spambase.scaled.new[,which(colnames(spambase.scaled.new)%in%relevant.v)]
# K-means clustering after logistic regression
k.cluster<-kmeans(spambase.scaled.new.lr, centers= 2, nstart = 20, algorithm = 'Lloyd',iter.max = 50)
sum(k.cluster$cluster-1==1)/length(k.cluster$cluster)
# validation of our cluster with the given classification (note that they have a 7% error range)
sum(k.cluster$cluster-1==spambase[rownames(spambase.scaled.out),'class'])/length(k.cluster$cluster)




### cluster
c.cluster <- e1071::cmeans(spambase.scaled.new.lr, iter.max = 20, centers = 2, dist = 'euclidean', method = 'cmeans', m = 1.1)
c.cluster <- e1071::cmeans(spambase.scaled.new,    iter.max = 20, centers = 2, dist = 'euclidean', method = 'cmeans', m = 2)
c.cluster <- e1071::cmeans(spambase.scaled.new,    iter.max = 20, centers = 2, dist = 'euclidean', method = 'cmeans', m = 3)

c.cluster$cluster[c.cluster$cluster == 2] <- 0

sum(c.cluster$cluster-1==spambase[rownames(spambase.scaled.new.lr),'class'])/length(c.cluster$cluster)
sum(c.cluster$cluster  ==spambase[rownames(spambase.scaled.new.lr),'class'])/length(c.cluster$cluster)

# correctly assigned observations
c.cluster$membership[c.cluster$cluster-1==spambase[rownames(spambase.scaled.new.lr),'class'],2]
# first 10 elements of the correctly assigned spam
sort(c.cluster$membership[c.cluster$cluster-1==spambase[rownames(spambase.scaled.new.lr),'class'],2],T)[1:10]



## kmeans
k.cluster<-kmeans(spambase.scaled.new.lr, centers= 2, nstart = 20, algorithm = 'Lloyd',iter.max = 50)
k.cluster<-kmeans(spambase.scaled.new,    centers= 2, nstart = 20, algorithm = 'Lloyd',iter.max = 50)
k.cluster<-kmeans(spambase.scaled.out,    centers= 2, nstart = 20, algorithm = 'Lloyd',iter.max = 50)
k.cluster<-kmeans(spambase.dupli,         centers= 2, nstart = 20, algorithm = 'Lloyd',iter.max = 50)

k.cluster$cluster[c.cluster$cluster == 2] <- 0

sum(k.cluster$cluster-1==spambase[rownames(spambase.scaled.new.lr),'class'])/length(k.cluster$cluster)
sum(k.cluster$cluster  ==spambase[rownames(spambase.scaled.new.lr),'class'])/length(k.cluster$cluster)

sum(k.cluster$cluster-1==spambase[,'class'])/length(k.cluster$cluster)
sum(k.cluster$cluster  ==spambase[,'class'])/length(k.cluster$cluster)




#######  factor
spam.factor<-factanal(spambase[,-58], factors = 4)

# check which variables highly load on a factor (>0.4)
spam.factor$loadings
factor1.variable<-which(abs(spam.factor$loadings[,1])>0.4, arr.ind = TRUE)
factor2.variable<-which(abs(spam.factor$loadings[,2])>0.4, arr.ind = TRUE)# there is only one variable, therefore no difference
factor3.variable<-which(abs(spam.factor$loadings[,3])>0.4, arr.ind = TRUE)
factor.variable<-c(factor1.variable, factor2.variable, factor3.variable)

# calculate the scores of factor based on the highly loading variables
factor1<-apply(spambase[,which(abs(spam.factor$loadings[,1])>0.4)],1,mean)
factor2<-spambase[,which(abs(spam.factor$loadings[,2])>0.4)]
factor3<-apply(spambase[,which(abs(spam.factor$loadings[,3])>0.4)],1,mean)
factor.score<-cbind(factor1, factor2, factor3)

# replace variables with factors in the dataset
spambase.scaled.new<-spambase[,-factor.variable]
spambase.scaled.new<-cbind(spambase.scaled.new, factor.score)


#####  logistic
class<-spambase[,58]

spambase.scaled.new<-spambase.scaled.new[,-factor.variable]
spambase.scaled.new<-cbind(spambase.scaled.new, factor.score, class)
spambase.scaled.new<-as.data.frame(spambase.scaled.new)

# replace variables with factors in the dataset
spambase.scaled.new<-spambase[,-factor.variable]
spambase.scaled.new<-cbind(spambase.scaled.new, factor.score)

## logistic regression

lr.model<-glm(class~., family = binomial(link = 'logit'), data = spambase.scaled.new, maxit=50)
lr.summary<-summary(lr.model)

relevant.v<-colnames(spambase.scaled.new)[lr.summary$coefficients[2:nrow(lr.summary$coefficients),
                                                                  4] < 0.05]
relevant.v<-relevant.v[!relevant.v=='class']
spambase.scaled.new.lr<-spambase.scaled.new[,which(colnames(spambase.scaled.new)%in%relevant.v)]



ggplot(spambase.scaled.new.lr, aes(re, X..4, color=factor(k.cluster$cluster-1))) +
  geom_point(size=3) + labs(y = "$", x="re\nfrom the subject of a mail: \"Re: ...\"") +
  scale_colour_manual(name="Spam", values = c(col.ercis.cyan, ercis.red)) +
  theme_bw(base_size = 12, base_family = "")

ggplot(data.frame(x), aes(first, second, color=factor(cl$cluster-1))) +
  geom_point(size=3) + #labs(y = "$", x="re\nfrom the subject of a mail: \"Re: ...\"") +
  geom_point(data=data.frame(cl$centers), aes(x=X,y=Y),pch = 8) +
  #scale_colour_manual(name="Spam", values = c(col.ercis.cyan, ercis.red)) +
  theme_bw(base_size = 12, base_family = "")


x<-rbind(matrix(rnorm(100,sd=1),ncol=2), matrix(rnorm(100,mean=1,sd=1),ncol=2))

cl<-cmeans(x, 2, 100, method="cmeans", m=1.1)
cl<-kmeans(x, centers= 2, nstart = 20, algorithm = 'Lloyd',iter.max = 50)

}
