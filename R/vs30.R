vs30 <- function(mod)
{
  s <- mod$s
  
  # Layer thickness
  thk <- mod$h
  n <- length(thk)
  
  # Depths to bottom of layers
  d <- cumsum(thk)
  
  if( any(d >= 30) )
  {
    # Layers below 30 m
    i <- which(d < 30)
    l <- 30 - d[max(i)]
    
    s30 <- (sum(thk[i] * s[i]) + l * s[max(i) + 1])/30
    vs30 <- 1/s30
  }else
  {
    vs30 <- NA
    cat("Vs(30) cannot be calculated.\n")
  }
  return(vs30)
}
