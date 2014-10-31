transform_datamatrix <- function(dm,margin=2,fun=transform_log,...) {
  dm.class <- class(dm)
  dm <- as.matridm(dm)
  
  r <- apply(dm,MARGIN=margin,FUN=match.fun(fun),...)
  
  if (dm.class == "data.frame") {
    r <- as.data.frame(dm)
  } else if (dm.class %in% c("integer","numeric")) {
    r <- as.vector(r)
  }
  
  return(r)
}

transform_log <- function(x,base=exp(1),add=0) {
  r <- log((x + add),base=base)
  
  return(r)
}

transform_negExponential <- function(x,a=1) {
  r <- exp(-a * x)
  
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

transform_nthroot <- function(x,n=3) {
  r <- x^(1/n)
  
  return(r)
}

transform_power <- function(x,lambda=0) {
  if (lambda != 0) {
    n <- length(x)
    r <- (x^lambda - 1)
    r <- r / (lambda * (transform_nthroot(prod(x),n=n))^(lambda - 1))
  } else {
    r <- transform_nthroot(prod(x),n=n) * log(x,base=exp(1))
  }
  
  return(r)
}

transform_boxcox <- function(x,lambda1=0,lambda2=NULL,type="1-parameter") {
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
    if (is.null(lambda2)) {
      min(x) > -lambda2
      lambda2 <- -min(x)
    }
    if (FALSE %in% (x > -lambda2)) {
      r <- twoPam_boxcox(x,lambda1=lambda1,lambda2=lambda2)
    } else {
      stop("Both lambdas must be numeric in 2-parameter boxcox")
    }
  }
  
  return(r)
}

# Define the boxcox functions for transform_boxcox function---
#-------------------------------------------------------------
onePam_boxcox <- function(x,lambda=0) {
  if (lambda != 0) {
    r <- (x^lambda - 1) / lambda
  } else {
    r <- log(x,base=exp(1))
  }
  
  return(r)
}

twoPam_boxcox <- function(x,lambda1,lambda2) {
  if (lambda1 != 0) {
    r <- ((x + lambda2)^lambda1 - 1) / lambda1
  } else {
    r <- log(x + lambda2,base=exp(1))
  }
  
  return(r)
}
#-------------------------------------------------------------
#-------------------------------------------------------------
