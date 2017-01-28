

c.a.normality.check <- function(){
  # qq plots
  pdf(file.path("out/2. Exploratory - QQ-Plots.pdf"))
  sapply(names(noclasses), function(column){
    qqnorm(spambase[,column], main=paste0("Q-Q Plot for: ",column), col = ercis.red)
    qqline(spambase[,column])
  })

  dev.off()

  # QQ-plot for Chi-Square distribution
  # Chi-Square based on generalized distances
  # It is used to check the corrrelation between data
  x <- stuff[1:54]
  cm <- colMeans(x)
  S <- cov(x)
  d <- apply(x,1,function(x) t(x-cm) %*% solve(S) %*% (x-cm))

  plot(qc <- qchisq((1:nrow(x)-1/2)/nrow(x), df=7),
       sd <- sort(d), xlab=expression(paste(chi[7]^ 2, "Quantile")),
       ylab="Ordered Distances", xlim=range(qc)*c(1,1.1),
       pch=19)

  out <- which(rank(abs(qc-sd), ties= "random") > nrow(x)-3)
  text(qc[out], sd[out]-1.5, names(out),col="blue")
  abline(a=0,b=1,col= "red",lwd=2)

  # draw histgraph for each variable
  layout(matrix(1:24,nrow = 4,ncol = 6))
  sapply(colnames(stuff[1:48]), function(x){
    hist(as.numeric(stuff[[x]]),main=x,xlab="",
         col="cyan")
  })

  layout(matrix(1:9,nrow = 3,ncol = 3))
  sapply(colnames(stuff[49:57]), function(x){
    hist(as.numeric(stuff[[x]]),main=x,xlab="",
         col="cyan")
  })

  # shapiro wilk test
  # check if the p-value in the result greater than 0.05 or not
  sapply(colnames(stuff[1:48]), function(x){
    shapiro.test(as.numeric(stuff[[x]]))
  })

  sapply(colnames(stuff[49:57]), function(x){
    shapiro.test(as.numeric(stuff[[x]]))
  })

}

c.b.normality.transform <- function(){
  # exponential to normal (log)

  # boxcox
}
