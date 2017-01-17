
#' @title Example Function for docu creation
#'
#' @description
#' Example function for understanding documentation
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
util.test.func <- function(x, y){
  return(x*x*y)
}

# set some colors
ercis.black    = rgb(  0/255,   0/255,   0/255, 1)
ercis.grey     = rgb( 94/255,  94/255,  93/255, 1)
ercis.red      = rgb(133/255,  35/255,  57/255, 1)
ercis.lightred = rgb(200/255, 156/255, 166/255, 1)
ercis.blue     = rgb(135/255, 151/255, 163/255, 1)
ercis.darkblue = rgb( 67/255,  92/255, 139/255, 1)
ercis.cyan     = rgb(  0/255, 156/255, 179/255, 1)
ercis.orange   = rgb(231/255, 124/255,  18/255, 1)
ercis.green    = rgb(135/255, 191/255,  42/255, 1)
