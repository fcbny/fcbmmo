transform_data <- function(x,fun=log,...) {
  r <- match.fun(fun)(x,...)
  
  return(r)
}

transform_log <- function(x,add=0,base=exp(1)) {
  r <- log(x + add,base)
  
  return(r)
}

transform_negExponential <- function(x) {
  r <- exp(-1 * x)
  
  return(r)
}

transform_exponentiation <- function(x,exponent=2) {
  r <- x^exponent
  
  return(r)
}

transform_squareroot <- function(x) {
  r <- sqrt(x)
  
  return(r)
}

transform_nothing <- function(x) {
  r <- c()
  
  return(r)
}