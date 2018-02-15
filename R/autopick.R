autopick <- function(data, snell = TRUE){
  mod1 <- regress(z = data$z, tt = data$tt.slant, bot = max(data$z), 
                  hoffset = data$hoffset, wt = data$wt, sig = data$sig)
  hoffset <- data$hoffset
  RSS <- sum((mod1$tt - mod1$tt.hat)^2)
  K <- length(mod1$s) + 1; N <- length(mod1$z)
  AIC1 <- 2 * K + N * log(RSS/N) + 2 * K * (K + 1)/(N - K - 1)
  decAIC <- TRUE
  while(decAIC == TRUE){
    for(i in 1:length(mod1$bot)){
      ini.vals <- find.ini(mod1)
      opt.out <- optimize(f = approx.rss, interval = c(ini.vals$top[i], ini.vals$bot[i]),
                          tol = 0.001, mod = mod1)
      newZ <- opt.out$minimum
      nlay <- nlayers(z = mod1$z, bot = sort(c(newZ, mod1$bot)))
      tmod <- regress(z = mod1$z, tt = mod1$tt.slant, bot = sort(c(mod1$bot, newZ)),
                      hoffset = mod1$hoffset, wt = mod1$wt, sig = mod1$sig)
      sign.test <- sign(tmod$s)
      sratio <- tmod$s[1:(length(tmod$s) - 1)]/tmod$s[2:length(tmod$s)]
      if(all(nlay >= 1) & all(sign.test > 0) & all(sratio < 20) & all(sratio > 0.05)){
        newbot <- sort(c(mod1$bot, newZ))
        cat(paste(sep = "", length(newbot)," layer model\nIteration "))
        if(snell == TRUE & hoffset > 0){
          mod2 <- snellregress(z = mod1$z, tt = mod1$tt.slant, bot = newbot,
                               hoffset = mod1$hoffset, wt = mod1$wt, sig = mod1$sig)
        }else{
          mod2 <- regress(z = mod1$z, tt = mod1$tt.slant, bot = newbot,
                          hoffset = mod1$hoffset, wt = mod1$wt, sig = mod1$sig)
        }
        cat("done.\n")
      }else{
        mod2 <- mod1
      }
      RSS <- sum((mod2$tt - mod2$tt.hat)^2)
      K <- length(mod2$s) + 1; N <- length(mod2$z)
      AIC2 <- 2 * K + N * log(RSS/N) + 2 * K * (K + 1)/(N - K - 1)
      decAIC <- AIC1 > AIC2
      if(decAIC == TRUE){
        mod1 <- mod2; AIC1 <- AIC2; break
        # par(cex.main = cex, mfrow = c(1,3)); plotmod(mod1)
      }else{ # An additional boundary is sometimes needed to realize benefit of 1
        if(all(nlay >= 1) ){
          ini.vals <- find.ini(mod2)
          opt.out <- optimize(f = approx.rss, interval = c(ini.vals$top[1], ini.vals$bot[1]),
                              tol = 0.001, mod = mod2)
          newZ <- opt.out$minimum
          nlay <- nlayers(z = mod1$z, bot = sort(c(newZ, mod2$bot)))
          tmod <- regress(z = mod1$z, tt = mod1$tt.slant, bot = sort(c(mod1$bot, newZ)),
                          hoffset = mod1$hoffset, wt = mod1$wt, sig = mod1$sig)
          sign.test <- sign(tmod$s)
          sratio <- tmod$s[1:(length(tmod$s) - 1)]/tmod$s[2:length(tmod$s)]
          if(all(nlay >= 1) & all(sign.test > 0) & all(sratio < 20) & all(sratio > 0.05)){
            newbot <- sort(c(mod2$bot, newZ))
            cat(paste(sep = "", "\t", length(newbot)," layer model:\n\tIteration "))
            if(snell == TRUE & hoffset > 0){
              mod3 <- snellregress(z = mod1$z, tt = mod2$tt.slant, bot = newbot,
                                   hoffset = mod2$hoffset, wt = mod1$wt, sig = mod1$sig)
            }else{
              mod3 <- regress(z = mod1$z, tt = mod2$tt.slant, bot = newbot,
                              hoffset = mod2$hoffset, wt = mod1$wt, sig = mod1$sig)
            }
            cat("done.\n")
          }else{
            mod3 <- mod2
          }
          RSS <- sum((mod3$tt - mod3$tt.hat)^2)
          K <- length(mod3$s) + 1; N <- length(mod3$z)
          AIC3 <- 2 * K + N * log(RSS/N) + 2 * K * (K + 1)/(N - K - 1)
          decAIC <- AIC1 > AIC3
          if(decAIC == TRUE){
            mod1 <- mod3; AIC1 <- AIC3; break
          }
        }
      }
    }
  }
  return(mod1$bot)
}
