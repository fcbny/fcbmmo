twoPam_boxcox <-
function(x,lambda1,lambda2) {
  if (lambda1 != 0) {
    r <- ((x + lambda2)^lambda1 - 1) / lambda1
  } else {
    r <- log(x + lambda2,base=exp(1))
  }
  
  return(r)
}
