transform_data <- function(x,fun=log,...) {
  r <- match.fun(fun)(x,...)
  
  return(r)
}

transform_log <- function(x,base=exp(1)) {
  r <- log(x,base)
  
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