

#' @title Prinicipal Component Analysis & Factor Analysis
#' @description To get (back) to the overview of all steps and functions use this link: \code{\link{a.a.main}}
#'
#'
#'
#' Two functions are executed here:
#' \enumerate{
#'   \item \code{\link{e.b.pca.analyze}}
#'   \item \code{\link{e.c.factor.analysis}}
#' }
#'
#' @author Lijin Han, Evelyn Navarrete
e.a.pca <- function(spambase.scaled.out){

  e.b.pca.analyze(spambase.scaled.out)

  return(e.c.factor.analysis(spambase.scaled.out))
}

#' @title PCA & FA - PCA Analysis
#' @description
#'
#' @author Lijin Lan
e.b.pca.analyze <- function(spambase.scaled.out){
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
}

#'
e.c.factor.analysis <- function(spambase.scaled.out){
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
  return(spambase.scaled.new)
}
