transform_boxcox <-
function(x,lambda1=0,lambda2=NULL,type="1-parameter") {
  if (length(type) != 1) {
    stop("You've provided more than one or zero arguement to parameter 'type'")
  }
  
  if (type != "1-parameter" & type != "2-parameter") {
    stop("Invalid 'type' parameter. Only '1-parameter' or '2-parameter' accepted")
  }

  
  if (type == "1-parameter") {
    if (TRUE %in% (x <= 0)) {
      stop("Only x > 0 values are valid in 1-parameter boxcox")
    }
    
    if (!is.null(lambda1)) {
      r <- onePam_boxcox(x,lambda=lambda1)
    } else {
      # profile likelihood estimation of lambda
      stop("Profile likelihood estimation not implemented yet")
    }
    
  } else if (type == "2-parameter") {
    if (!is.null(lambda1) & !is.null(lambda2)) {
      r <- twoPam_boxcox(x,lambda1=lambda1,lambda2=lambda2)
    } else {
      stop("Both lambdas must be numeric in 2-parameter boxcox")
    }
  }
  
  return(r)
}
