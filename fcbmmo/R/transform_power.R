transform_power <-
function(x,lambda=0) {
  if (lambda != 0) {
    n <- length(x)
    r <- (x^lambda - 1)
    r <- r / (lambda * (transform_nthroot(prod(x),n=n))^(lambda - 1))
  } else {
    r <- transform_nthroot(prod(x),n=n) * log(x,base=exp(1))
  }
  
  return(r)
}
