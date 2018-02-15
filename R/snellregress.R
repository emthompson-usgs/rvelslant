snellregress <- function(z, tt, bot, hoffset, wt = NULL, sig = NULL){
  max.iter <- 18
  voltol <- 0.01
  maxdiff  <- 1
  theta <- 0
  i <- 1
  while(maxdiff > voltol & i <= max.iter){
    cat(paste(sep = "", i, "..."))
    mod1  <- regress(z, tt, bot, theta, hoffset, wt = wt, sig = sig)
    theta <- thetamatrix(z, v = mod1$v, bot, hoffset)
    mod2  <- regress(z, tt, bot, theta, hoffset, wt = wt, sig = sig)
    maxdiff  <- max(abs(mod1$v - mod2$v))
    i <- i + 1
  }
  if(i == max.iter){
    mod2 <- NA
  }
  return(mod2)
}
