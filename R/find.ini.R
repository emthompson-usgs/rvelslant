find.ini <- function(mod){
  rss <- NULL
  bot <- mod$bot
  top <- bot - mod$h
  mid <- (bot + top)/2
  n <- length(bot)
  for(i in 1:n){
    keep <- mod$z > top[i] & mod$z <= bot[i]
    rss[i] <- sum((mod$tt.slant[keep] - mod$tt.hat[keep])^2)
  }
  nl <- nlayers(z = mod$z, bot = mod$bot)
  wrss <- nl * rss/sum(nl)
  wrss.ix <- sort(wrss, index.return = TRUE, decreasing = TRUE)$ix
  mid <- mid[wrss.ix]
  top <- top[wrss.ix]
  bot <- bot[wrss.ix]
  return(list(mid = mid, top = top, bot = bot))
}
