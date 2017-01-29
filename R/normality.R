

c.a.normality <- function(){

  # qq plots
  c.b.qqplots()

  # QQ-plot for Chi-Square distribution
  # Chi-Square based on generalized distances
  # It is used to check the corrrelation between data
  c.c.chisquare()

  # distribution overview
  c.d.distribution_test()

  # Examplary use of transformation to normality
  c.e.norm_transform()
}

c.b.qqplots <- function(){
  pdf(file.path("out/2. Normality - Normality QQ-Plots.pdf"))
  layout(matrix(1:4, ncol=2,nrow=2))
  sapply(names(noclasses), function(column){
    qqnorm(spambase[,column], main=paste0("Normality Q-Q Plot for: ",column), col = ercis.red)
    qqline(spambase[,column], lwd=2)
  })
  dev.off()
  layout(1)

  cat("Normality Q-Q-Plots created\n")
}


c.c.chisquare <- function(){
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

  cat("Normality chi^2-Plot created\n")
}

c.d.distribution_test <- function(){
  #plot distributions
  pdf(file.path("out/2. Normality - Cullen and Frey graphs.pdf"))
  sapply(names(noclasses), function(column){
    fitdistrplus::descdist(spambase[,column])
    title(column, line = 0.5)
  })
  dev.off()

  cat("Cullen and Frey graphs created\n")
}


c.e.norm_transform <- function(){

  vari <- spambase[spambase[,"will"]>0,"will"]

  pdf(file.path("out/2. Normality - Example normality transformation.pdf"))

  print(ggplot2::ggplot() + ggplot2::aes(x=vari) +
    ggplot2::labs(x="",title="vector \"will\" reduced by mails without this word (freq = 0)") +
    ggplot2::geom_histogram(binwidth = max(vari)/100, fill=ercis.red) +
    ggplot2::theme_bw(base_size = 12, base_family = "") +
    ggplot2::annotate("text", label = paste0("p-value: ", shapiro.test(vari)$p.value),
                      x=Inf,y=Inf, vjust=1, hjust=1, size = 6, colour = ercis.red))

  qqnorm(vari, main="Normality Q-Q Plot for: will", col = ercis.red)
  qqline(vari, lwd=2)

  #
  res = optimize(z.d.loglik, interval = c(-2, 2), vari, maximum = TRUE)
  vari <- scale(z.c.boxcox(vari, res$maximum))

  print(ggplot2::ggplot() + ggplot2::aes(x=vari) +
          ggplot2::labs(x="",title="vector \"will\" after boxcox transformation with optimized lambda") +
          ggplot2::geom_histogram(binwidth = max(vari)/100, fill=ercis.red) +
          ggplot2::theme_bw(base_size = 12, base_family = "") +
          ggplot2::annotate("text", label = paste0("p-value: ", shapiro.test(vari)$p.value),
                            x=Inf,y=Inf, vjust=1, hjust=1, size = 6, colour = ercis.red))

  qqnorm(vari, main="Normality Q-Q Plot for: will", col = ercis.red)
  qqline(vari, lwd=2)

  dev.off()
}
