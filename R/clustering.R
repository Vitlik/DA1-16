

f.a.clustering <- function(){

  # Fuzzy C Means
  # install the package which contain the function "cmeans"
  install.packages("e1071")
  library(e1071)

  # use cmeans to cluster our data with 48 variables
  c.cluster <- cmeans(stuff[,1:48],2,50,verbose = TRUE,method = "cmeans")

  # show how many emails in each class
  c.cluster$size

  # if you want to see each email belongs to which class
  # c.cluster$cluster

  # Comparison of the clusters with email type (variable: class)
  table(c.cluster$cluster,stuff$class)

  # Compute the rate of matching result
  sum(as.numeric(c.cluster$cluster)!=spambase[,58])/nrow(spambase)

  # use cmeans to cluster our data with 57 variables
  c.cluster_all <- cmeans(stuff[,1:57],2,50,verbose = TRUE,method = "cmeans")

  # show how many emails in each class
  c.cluster_all$size

  # if you want to see each email belongs to which class
  # c.cluster_all$cluster

  # Comparison of the clusters with email type (variable: class)
  table(c.cluster_all$cluster,stuff$class)

  # Compute the rate of matching result
  sum(as.numeric(c.cluster_all$cluster)!=spambase[,58])/nrow(spambase)


}
