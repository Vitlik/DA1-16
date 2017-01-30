

f.a.clustering <- function(){

  # Fuzzy C Means

  # use cmeans to cluster our data with 48 variables
  c.cluster <- e1071::cmeans(word_freq,2,50,verbose = TRUE,method = "cmeans")

  # show how many emails in each class
  c.cluster$size

  # if you want to see each email belongs to which class
  # c.cluster$cluster

  # Comparison of the clusters with email type (variable: class)
  table(c.cluster$cluster,classes)

  # Compute the rate of matching result
  sum(as.numeric(c.cluster$cluster)!=classes)/nrow(spambase)

  # use cmeans to cluster our data with 57 variables
  c.cluster_all <- e1071::cmeans(noclasses,2,50,verbose = TRUE,method = "cmeans")

  # show how many emails in each class
  c.cluster_all$size

  # if you want to see each email belongs to which class
  # c.cluster_all$cluster

  # Comparison of the clusters with email type (variable: class)
  table(c.cluster_all$cluster,classes)

  # Compute the rate of matching result
  sum(as.numeric(c.cluster_all$cluster)!=classes)/nrow(spambase)


}
