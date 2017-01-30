

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
#' @author Lijin Lan, Evelyn Navarrete
e.a.pca <- function(spambase.scaled.out){

  #PCA & FA - Analyze the correlation and latent factors
  e.b.pca.analyze(spambase.scaled.out)

  # factor analysis -- conduct factor analysis and replace variables with factors
  return(e.c.factor.analysis(spambase.scaled.out))
}

#' @title PCA & FA - Analyze the correlation and latent factors
#' @description The function  firstly creates a correlation matrix and lists up variables, which are highly
#' correlated with each other. The thereshold is empirically set to 0.5. In the end, the function conducts
#' a principal component analysis to select the relevant factors while a screediagram is also generated
#' to help to better identify the number.
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
  (high.c<-cbind(rnames,cnames))

  # inspect the number of latent factors
  res.pca<-princomp(spambase.scaled.out)
  screeplot(res.pca, npcs=57)
}

#' @title factor analysis -- conduct factor analysis
#' @description The function firstly inspects the loading matrix, to see how each variables load on the factor.
#' Consequently the function select those variables, which highly (loading>0.4) load on factors and replace them
#' in the dataset with corresponding factors.
#' @author Lijin
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
