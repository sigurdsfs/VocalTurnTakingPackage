#' @title Tanh Function
#'
#' @description tanh function y = exp(x)/sum(exp(x))
#'
#' @param x Single value or list of values.

#' @return y
#' @examples
#'
#' @export

tanh <- function(x){
  y <- exp(x)/sum(exp(x))
  return(y)
}
