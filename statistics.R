stat_skewness <- function(x,type="moment-coefficient",na.rm=FALSE) {
  if (na.rm == TRUE) {
    x <- x[!is.na(x)]
    x <- x[x != Inf]
  }
  
  x.mean <- mean(x)
  x.sd <- sd(x)
  x.n <- length(x)
  x.mode <- stat_mode(x)
  x.median <- median(x)
  
  if (type == "moment-coefficient") {
    # Pearson's moment coefficient of skewness
    r <- ((x - x.mean) / x.sd)^3
    r <- mean(r)
    
  } else if (type == "sample") {
    # Sample skewness
    m <- sum((x - x.mean)^3) / x.n
    s <- ((sum((x - x.mean)^2) /(x.n - 1)))^(3/2)
    r <- m / s
    
  } else if (type == "mode-skew") {
    # Pearson's mode skewness
    r <- (x.mean - x.mode) / x.sd
    
  } else if (type == "first-skew") {
    # Pearson's first skew coefficient
    r <- 3 * (x.mean - x.mode) / x.sd
    
  } else if (type == "second-skew") {
    # Pearson's second skew coefficient
    r <- 3 * (x.mean − x.median) / x.sd
    
  } else {
    stop("Invalid argument for parameter 'type'")
  }
  
  return(r)
}

stat_kurtosis <- function(x,na.rm=FALSE,excess=TRUE) {
  if (na.rm == TRUE) {
    x <- x[!is.na(x)]
    x <- x[x != Inf]
  }
  
  x.n <- length(x)
  x.mean <- mean(x)
  
  r <- mean((x - x.mean)^4) / (mean((x - x.mean)^2))^2
  
  if (excess == TRUE) {
    r <- r - 3
  }
  return(r)
}

stat_mode <- function(x,na.rm=FALSE) {
  if (na.rm == TRUE) {
    x <- x[!is.na(x)]
    x <- x[x != Inf]
  }
  
  x.uni <- unique(x)
  r <- x.uni[which.max(tabulate(match(x,x.uni)))]
  
  return(r)
}
