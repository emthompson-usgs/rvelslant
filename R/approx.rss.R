approx.rss <- function(newZ, mod){
  bot <- sort(c(newZ, mod$bot))
  mod2 <- regress(z = mod$z, tt = mod$tt.slant, bot = bot, 
               hoffset = mod$hoffset, wt = mod$wt, sig = mod$sig)
  rss <- sum((mod2$tt.slant - mod2$tt.hat)^2)
  # To make sure depths that cause layers to be negative
  # are never used:
  if(any(mod2$s <= 0)) rss <- sum(mod2$tt.slant^2)
  return(rss)
}
