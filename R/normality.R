
#' @title Normality
#' @description To get (back) to the overview of all steps and functions use this link: \code{\link{a.a.main}}
#'
#' This functions wraps the normality analysis and transformation of spambase.
#'
#' A normal distribution enables the usage of several methods which require normality.
#'
#' But finding a normal distribution in data derived from the reality is quite unlikely and
#' even though several other distribution can be transformed to get close to normality, finding
#' these in data from quality is also unlikely.
#'
#' The normality check starts with creation of Q-Q-Plots for each dimension, showing that none of
#' the dimensions is close normal distributed.
#'
#' For the sake of completeness normality was also checked over all dimensions as a whole.
#' This was done by creating a chi square plot for normal distribution which again shows the same
#' as the univariate plots.
#'
#' Maybe an other distribution is in place with the exponential one being suspected looking at the Q-Q-Plots. Therefore a Cullen and Frey graph was plotted for variable showing that the all data far from being exponential distributed. Some dimensions come close to the gamma distribution which is the higher order of an exponential distribution.
#' But still the distribution seems to not fit into any known distribution (at least regarding the dataset as a whole).
#' Specific subset may fit into an exisiting distribution.
#'
#' The main obstacle of having an exponential distribution seem to be the substantial amount of zero values in each dimension.
#'
#' The dimension "will" was chosen because it's distribution is closest to being exponential distributed compared to all other dimensions. "will" also appears to have one of the smallest amounts of zeros in its observations which support the hypothesis that the zeros are changing the distribution from exponential to something unknown.
#'
#' Thus one option is the deletion of all/most observations being zero.
#' But this is viable for data that would get biased too much that way.
#' In this case most of dimensions consist of huge amounts of zero values and thus are highly affected by the deletion of zeros.
#'
#' Even though the deletion of rows in this framework the transformation to normality was rejected, a transformation of a dimension was done via this approach to prove that the distributions become exponention by removing all zero observation. As mentioned this will be done with the "will" dimension in the function of this script.
#'
#' Four functions are executed here:
#' \enumerate{
#'   \item \code{\link{c.b.qqplots}}
#'   \item \code{\link{c.c.chisquare}}
#'   \item \code{\link{c.d.distribution_test}}
#'   \item \code{\link{c.e.norm_transform}}
#' }
#'
#' @author Vitali Friesen
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

#' @title Normality - Q-Q-Plots
#' @description To get (back) to the overview of all steps and functions use this link:
#' \code{\link{a.a.main}}
#'
#' This function creates 57 (all dimensions without classification) Q-Q-Plots for normality and stores them in the file: "out/2. Normality - Normality QQ-Plots.pdf"
#'
#' @author Vitali Friesen
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

#' @title Normality - Chi Square
#' @description To get (back) to the overview of all steps and functions use this link:
#' \code{\link{a.a.main}}
#'
#' c.c.chisquare() creates a chi square plot for checking the normality of all dimensions combined and stores it in: "out/2. Normality - Normality chi^2-Plot.pdf"
#'
#' @author Vitali Friesen
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

#' @title Normality - Distribution Test
#' @description To get (back) to the overview of all steps and functions use this link: \code{\link{a.a.main}}
#'
#' This function creates a Cullen and Frey graph for each dimension. These graphs can support the
#' identification of distributions.
#'
#' @author Vitali Friesen
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

#' @title Normality - Normality Transformation
#' @description To get (back) to the overview of all steps and functions use this link: \code{\link{a.a.main}}
#'
#' This function computes an examplary data transformation with the dimension "will" to check the possibility to normalize the dimensions with removed zero observations.
#'
#' The results show that the normality of the distribution could be improved tremendously even it's still far from normal. The resulting p-value of the Shapiro-Wilk-Test could be increased from something like x*10^(-48) to x*10^(-8).
#'
#' \code{\link{c.a.normality}}
#' @author Vitali Friesen
c.e.norm_transform <- function(){

  vari <- spambase[spambase[,"will"]>0,"will"]

  pdf(file.path("out/2. Normality - Example normality transformation.pdf"))

  # plot histogram of "will"  after zero" observations were removed
  print(ggplot2::ggplot() + ggplot2::aes(x=vari) +
    ggplot2::labs(x="",title="vector \"will\" reduced by mails without this word (freq = 0)") +
    ggplot2::geom_histogram(binwidth = max(vari)/100, fill=ercis.red) +
    ggplot2::theme_bw(base_size = 12, base_family = "") +
    ggplot2::annotate("text", label = paste0("p-value: ", shapiro.test(vari)$p.value),
                      x=Inf,y=Inf, vjust=1, hjust=1, size = 6, colour = ercis.red))

  # Q-Q-Plot for "will" before transformation but with "zero" observations removed
  qqnorm(vari, main="Normality Q-Q Plot for: will", col = ercis.red)
  qqline(vari, lwd=2)

  # optimize Log-likelihood function of boxcox to find best lambda between -2 and 2
  res = optimize(z.d.loglik, interval = c(-2, 2), vari, maximum = TRUE)
  # transform data with boxcox and best lambda and scale afterwards
  vari <- scale(z.c.boxcox(vari, res$maximum))

  # plot new histogram with normalized observation attributes
  print(ggplot2::ggplot() + ggplot2::aes(x=vari) +
          ggplot2::labs(x="",title="vector \"will\" after boxcox transformation with optimized lambda") +
          ggplot2::geom_histogram(binwidth = max(vari)/100, fill=ercis.red) +
          ggplot2::theme_bw(base_size = 12, base_family = "") +
          ggplot2::annotate("text", label = paste0("p-value: ", shapiro.test(vari)$p.value),
                            x=Inf,y=Inf, vjust=1, hjust=1, size = 6, colour = ercis.red))

  # plot new Q-Q-Plot
  qqnorm(vari, main="Normality Q-Q Plot for: will", col = ercis.red)
  qqline(vari, lwd=2)

  dev.off()
}
