#' @title Sigmoid Function
#'
#' @description Sigmoid function y = 1/(1+exp(-x))
#'
#' @param x Single value or list of values.

#' @return y
#' @examples
#'
#' @export

sigmoid <- function(x){
  y <- 1/ (1+exp(-x))
  return(y)
}
