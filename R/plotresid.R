plotresid <- function(mod, nticks = NULL, grid = FALSE, depth = "v",
                      axes = TRUE){
  z <- mod$z; tt <- mod$tt;hoffset <- mod$hoffset;bot <- mod$bot
  h <- mod$h;top <- bot - h;s <- mod$s;tt.hat <- mod$tt.hat; sig <- mod$sig
  # To be consistent with Boore's program:
  sigcol <- rep("white", length(sig));sigcol[sig == 1] <- "black"
  sigcol[sig == 2] <- "blue";sigcol[sig == 3] <- "cyan"
  sigcol[sig == 4] <- "magenta";sigcol[sig == 5] <- "red"
  # For display purposes, at z = 0, the tt should be hoffset * s1:
  # snellregress() was used if theta != 0, otherwise, t0 = 0
  if(all(mod$theta == 0)){
    t0 <- 0
  }else{
    t0 <- hoffset * s[1]
  }
  # Now plot the residuals:
  RES <- (tt[1:length(tt.hat)] - tt.hat) * 1000
  if(!is.null(nticks)){
    tcks <- pretty(x = z, n = nticks)
  }else{
    tcks <- pretty(x = z)
  }
  if(depth == "v"){
    plot(RES, z[1:length(tt.hat)], type = "n", ylim = c(max(z), 0), xlab = "", 
         main = "", ylab = "", axes = FALSE)
    abline(v = 0, col = "grey", lwd = 2)
    if(axes == TRUE){
      axis(side = 3)
      axis(side = 2, labels = FALSE, at = tcks)
    }
    if(grid == TRUE){
      abline(h = tcks, col = "grey", lty = 1)
      abline(v = pretty(x = RES), col = "grey", lty = 1)
      abline(v = 0, col = "black", lwd = 2)
      box(col = "black", lty = 1)
    }
    title(main = "Residuals, msec", line = 2)
    abline(h = bot, col = "red")
    points(RES, z[1:length(tt.hat)], pch = 16, col = sigcol)
  }else{
    plot(z[1:length(tt.hat)], RES, type = "n", xlim = c(0, max(z)), xlab = "", 
         main = "", ylab = "", axes = FALSE)
    abline(h = 0, col = "grey", lwd = 2)
    if(axes == TRUE){
      axis(side = 2)
      axis(side = 1, labels = FALSE, at = tcks)
    }
    if(grid == TRUE){
      abline(v = tcks, col = "grey", lty = 1)
      abline(h = pretty(x = RES), col = "grey", lty = 1)
      abline(h = 0, col = "black", lwd = 2)
      box(col = "black", lty = 1)
    }
    title(ylab = "Residuals, msec")
    abline(v = bot, col = "red")
    points(z[1:length(tt.hat)], RES, pch = 16, col = sigcol)
  }
}
