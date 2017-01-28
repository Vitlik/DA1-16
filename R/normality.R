

c.a.normality <- function(){

  # qq plots
  pdf(file.path("out/2. Normality - Normality QQ-Plots.pdf"))
  layout(matrix(1:4, ncol=2,nrow=2))
  sapply(names(noclasses), function(column){
    qqnorm(spambase[,column], main=paste0("Normality Q-Q Plot for: ",column), col = ercis.red)
    qqline(spambase[,column],lwd=2)
  })
  dev.off()

  # QQ-plot for Chi-Square distribution
  # Chi-Square based on generalized distances
  # It is used to check the corrrelation between data
  x <- noclasses
  cm <- colMeans(x)
  S <- cov(x)
  d <- apply(x,1,function(x) t(x-cm) %*% solve(S) %*% (x-cm))

  pdf(file.path("out/2. Normality - Normality chi^2-Plot.pdf"))
  plot(qc <- qchisq((1:nrow(x)-1/2)/nrow(x), df=7),
       sd <- sort(d), xlab=expression(paste(chi[7]^2, "Quantile")),
       ylab="Ordered Distances", xlim=range(qc)*c(1,1.1),
       pch=19,cex.lab=2,cex.axis=2,cex=2,
       col=ercis.red
       )

  abline(a=0,b=1,lwd=2)
  dev.off()
}


c.b.boxcox <- function(data, lambda){
  if (lambda == 0) {
    return(log(data))
  }
  return((data^lambda - 1) / lambda)
}

c.c.loglik = function(lambda, data) {
  n = length(data)
  boxcoxed = c.c.boxcox(data, lambda)
  a = var(boxcoxed)
  b = sum(log(data))
  return((-n/2) * a + (lambda - 1) * b)
}

