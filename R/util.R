
#' @title Example Function for docu creation
#'
#' @description Example function for understanding how to document
#' Calculates product of \code{x^2} multiplied by \code{y}
#'
#' @param x A number which will be squared
#' @param y A number which get multiplied to \code{x^2}
#' @return the caculated result of \code{x * x * y}
#' @examples
#' test.func(3,2)
#' # 18
#'
#' test.func(2,3)
#' # 12
z.z.util.test.func <- function(x, y){
  return(x*x*y)
}



z.a.util.gen.summary <- function(frame1, f1_type, frame2 = NULL, ...){
  stopifnot(is.data.frame(frame1))
  if(is.null(frame2) | !is.data.frame(frame2)){
    tmp <- rbind(
      sapply(1:ncol(frame1), function(column){min(frame1[,column])}),
      rbind(
        sapply(1:ncol(frame1), function(column){
          quantile(frame1[,column],probs = 0.25)}),
        rbind(
          sapply(1:ncol(frame1), function(column){median(frame1[,column])}),
          rbind(
            colMeans(frame1),
            rbind(
              sapply(1:ncol(frame1), function(column){
                quantile(frame1[,column],probs = 0.75)}),
              sapply(1:ncol(frame1), function(column){max(frame1[,column])})
            )
          )
        )
      )
    )
    tmp <- round(tmp,4)
    colnames(tmp) <- colnames(frame1)
    row.names(tmp) <- c(paste0(f1_type," Min:"), paste0(f1_type," 1st Qu.:"),
                        paste0(f1_type," Median:"), paste0(f1_type," Means:"),
                        paste0(f1_type," 3rd Qu.:"), paste0(f1_type," Max:"))
    return(tmp)
  } else {
    tmp <- rbind(
      z.b.util.diff(
        sapply(1:ncol(frame1), function(column){min(frame1[,column])}),
        sapply(1:ncol(frame2), function(column){min(frame2[,column])})
      ),
      rbind(
        z.b.util.diff(
          sapply(1:ncol(frame1), function(column){
            quantile(frame1[,column],probs = 0.25)}),
          sapply(1:ncol(frame2), function(column){
            quantile(frame2[,column],probs = 0.25)})
        ),
        rbind(
          z.b.util.diff(
            sapply(1:ncol(frame1), function(column){median(frame1[,column])}),
            sapply(1:ncol(frame2), function(column){median(frame2[,column])})
          ),
          rbind(
            z.b.util.diff(
              colMeans(frame1),
              colMeans(frame2)
            ),
            rbind(
              z.b.util.diff(
                sapply(1:ncol(frame1), function(column){
                  quantile(frame1[,column],probs = 0.75)}),
                sapply(1:ncol(frame2), function(column){
                  quantile(frame2[,column],probs = 0.75)})
              ),
              z.b.util.diff(
                sapply(1:ncol(frame1), function(column){
                  max(frame1[,column])}),
                sapply(1:ncol(frame2), function(column){
                  max(frame2[,column])})
              )
            )
          )
        )
      )
    )
    colnames(tmp) <- colnames(frame1)
    row.names(tmp) <- c(paste0(f1_type," Min:"), paste0(f1_type," 1st Qu.:"),
                        paste0(f1_type," Median:"), paste0(f1_type," Means:"),
                        paste0(f1_type," 3rd Qu.:"), paste0(f1_type," Max:"))
    tmp <- rbind(
      rbind(z.a.util.gen.summary(frame1, "NoSpam"), rep("",length(frame1))),
      rbind(
        rbind(z.a.util.gen.summary(frame2, "Spam"), rep("",length(frame1))),
        tmp
      )
    )
    return(tmp)
  }
}


z.b.util.diff <- function(vec1, vec2){
  stopifnot(length(vec1) == length(vec2) & is.numeric(c(vec1,vec2)))
  return(
    sapply(1:length(vec1), function(pos){
      ifelse(vec2[pos] != 0,
             ifelse(vec1[pos] != 0,
                    paste0(round((vec2[pos]/vec1[pos])*100, 2),"%"),
                    "only in spam"),
             ifelse(vec1[pos] != 0, "only in nospam", "Both 0"))
    })
  )
}


z.c.boxcox <- function(data, lambda){
  if (lambda == 0) {
    return(log(data))
  }
  return((data^lambda - 1) / lambda)
}


z.d.loglik = function(lambda, data) {
  n <- length(data)
  boxcoxed <- z.c.boxcox(data, lambda)
  a <- var(boxcoxed)
  b <- sum(log(data))
  return((-n/2) * a + (lambda - 1) * b)
}

z.z.set_environment <- function(){
  # set some colors
  ercis.black    <<- rgb(  0/255,   0/255,   0/255, 1)
  ercis.grey     <<- rgb( 94/255,  94/255,  93/255, 1)
  ercis.red      <<- rgb(133/255,  35/255,  57/255, 1)
  ercis.lightred <<- rgb(200/255, 156/255, 166/255, 1)
  ercis.blue     <<- rgb(135/255, 151/255, 163/255, 1)
  ercis.darkblue <<- rgb( 67/255,  92/255, 139/255, 1)
  ercis.cyan     <<- rgb(  0/255, 156/255, 179/255, 1)
  ercis.orange   <<- rgb(231/255, 124/255,  18/255, 1)
  ercis.green    <<- rgb(135/255, 191/255,  42/255, 1)
}
