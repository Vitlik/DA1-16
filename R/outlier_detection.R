
#' @title Outlier Detection
#' @description To get (back) to the overview of all steps and functions use this link: \code{\link{a.a.main}}
#'
#' Starts the steps for finding outliers.
#'
#' Outliers are observation which fall off far from other observations and they bias the results of
#' different analyses tool.
#'
#' There are different reasons for the ocurrance of outliers：They simply occur as contigencies or
#' they occur because of technical errors, such as measurement error of sensor or errors on the harddrive.
#' Another important source of outliers is that those outliers might actually belong to another
#' undersampled distribution, which is not discovered.
#' Eliminating outliers generated from the undersampled distribution can disturb the attempt to recover
#' the real distributions while deleting outliers caused by technical errors is legitimate.
#' Unfortunately these cannot always be distinguished and therefore need to be treated carefully.
#'
#' Two functions are executed here:
#' \enumerate{
#'   \item \code{\link{d.b.outlier_detection}}
#'   \item \code{\link{d.c.outlier_delete}}
#' }
#'
#' @author Lijin Lan (construct of function), Vitali Friesen (structure, documentation)
d.a.outlier_handle <- function(){

  tmp <- d.b.outlier_detection()
  return(d.c.outlier_delete(tmp))

}

#' @title Outliers - Detect
#' @description
#'
#' @author Lijin Lan
d.b.outlier_detection <- function(){
  # inspect whether there are raws with only zeros
  sum(apply(spambase.scaled,1,mean)==0)

  # The function "lofactor" identifies outliers by calculating the multivariate distances.
  # It returns a vector of local outlier factors of each object in the data.
  # The parameter "neigbour" (k) is set to 100,
  #   since there are a large number duplicated raws in the dataset. In
  # oder to avoid generating NAs, there should be more neigbours to be compared.
  lof.det<-DMwR::lofactor(spambase.scaled,k=100)

  # plot the outliers and their IDs in the dataset.
  # Those objects, whose values are greater than the mean
  # value plus 4 times standard deviation are ploted in red with respective number.
  gg.df<-data.frame(names=as.factor(1:nrow(spambase)), lof.det)
  plot.new()
  ggplot(gg.df, aes(x=gg.df[,1], y=gg.df[,2], col="black", label=gg.df[,1]))+
    geom_point(col='black', pch=1) +
    geom_text(aes(label=ifelse(gg.df[,2]>(mean(lof.det)+4*sd(lof.det)),
                               as.character(gg.df[,1]),'')),hjust=0,vjust=0)+
    labs(title= "Outlier Detection", x = "Observations", y = "")+
    theme(plot.title = element_text(hjust=0.5))+
    geom_hline(aes(yintercept = mean(lof.det)+4*sd(lof.det),col='red'))
  # lines(x = c(0, nrow(spambase)), y=c(0, mean(lof.det)+4*sd(lof.det)), ylim=c(0,300), col='red')
  return(lof.det)
}

#' @title Outliers - Delete
d.c.outlier_delete<- function(lof.det){
  # After visually inspect the outliers, we select an appropriate threshold for deleting outliers.
  # In our case, it is 4 times standard deviation
  spambase.scaled.out<-spambase.scaled[-which(lof.det>(mean(lof.det)+3.5*sd(lof.det))),]
  return(spambase.scaled.out)
}





