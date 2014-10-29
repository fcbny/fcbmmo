onePam_boxcox <-
function(x,lambda=0) {
  if (lambda != 0) {
    r <- (x^lambda - 1) / lambda
  } else {
    r <- log(x,base=exp(1))
  }
  
  return(r)
}
