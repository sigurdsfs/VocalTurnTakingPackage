tanh <- function(x){
  y <- exp(x)/sum(exp(x))
  return(y)
}
