

c.a.normality.check <- function(){
  # p-value

  # qq plots
  pdf(file.path("out/2. Exploratory - QQ-Plots.pdf"))
  sapply(names(noclasses), function(column){
    qqnorm(spambase[,column], main=paste0("Q-Q Plot for: ",column), col = ercis.red)
    qqline(spambase[,column])
  })
  dev.off()

  # shapiro wilk test


}

c.b.normality.transform <- function(){
  # exponential to normal (log)

  # boxcox
}
