

# output folder for all the created information (text, tables & plots)
output_folder = "../out"

#' @title Example Function
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
test.func <- function(x, y){
  return(x*x*y)
}
