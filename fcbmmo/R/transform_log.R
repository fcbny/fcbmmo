transform_log <-
function(x,base=exp(1),add=0) {
  r <- log((x + add),base=base)
  
  return(r)
}
