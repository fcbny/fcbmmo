transform_data <-
function(x,fun=log,...) {
  r <- match.fun(fun)(x,...)
  
  return(r)
}
