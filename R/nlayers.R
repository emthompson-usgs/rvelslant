nlayers <- function(z, bot){
  n <- NULL
  if(length(bot > 1)){
    top <- c(0, bot[(1:length(bot)-1)])
  }else{
    top <- 0
  }
  for(i in 1:length(bot)){
    n[i] <- length(z[z > top[i] & z <= bot[i]])
  }
  return(n)
}


