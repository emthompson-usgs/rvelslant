slowprofile <- function(mod, add = FALSE, col = "blue", lty = 1, lwd = 1,
                        new = TRUE, nticks = NULL, grid = FALSE, axes = TRUE,
                        depth = "v", bounds = TRUE){
  if(new == TRUE) par(mfrow = c(1, 1), las = 1, xaxs = "r", yaxs = "r", tcl = 0.2)
  bot <- mod$bot; h <- mod$h; top <- bot - h
  s <- 1000 * mod$s; s.upper <- 1000 * (mod$s + mod$se); s.lower <- 1000 * (mod$s - mod$se)
  n <- length(s); ylim <- c(max(mod$z), 0); xlim <- c(0, max(s.upper))
  if(!is.null(nticks)){
    tcks <- pretty(x = mod$z, n = nticks)
  }else{
    tcks <- pretty(x = mod$z)
  }
  if(add == FALSE){
    if(depth == "v"){
      plot(1, 1, ylim = ylim, xlim = xlim, main = "",
           xlab = "", ylab = "", type = "n", axes = FALSE)
      if(axes == TRUE){
        axis(side = 3)
        axis(side = 2, labels = FALSE, at = tcks)
        if(new == TRUE) axis(side = 2, tick = FALSE, at = tcks)
      }
      if(grid == TRUE){
        abline(h = tcks, col = "grey", lty = 1)
        abline(v = pretty(x = c(s.upper, 0)), col = "grey", lty = 1)
        box(col = "black", lty = 1)
      }
      if(new == TRUE) title(ylab = "Depth, m")
      title(main = "Slowness, sec/km", line = 2)
    }else{
      plot(1, 1, ylim = xlim, xlim = rev(ylim), main = "",
           xlab = "", ylab = "", type = "n", axes = FALSE)
      if(axes == TRUE){
        axis(side = 2)
        axis(side = 1, at = tcks)
      }
      if(grid == TRUE){
        abline(v = tcks, col = "grey", lty = 1)
        abline(h = pretty(x = c(s.upper, 0)), col = "grey", lty = 1)
        box(col = "black", lty = 1)
      }
      title(ylab = "Slowness, sec/km")
      title(xlab = "Depth, m")
    }
  }
  # Draw in the layers
  if(depth == "v"){
    if(bounds == TRUE){
      # Upper:
      segments(s.upper, top, s.upper, bot, col = col, lty = 3, lwd = lwd)
      segments(s.upper[1:(n - 1)], top[2:n], s.upper[2:n], top[2:n],
               col = col, lty = 3, lwd = lwd)
      # Lower:
      segments(s.lower, top, s.lower, bot, col = col, lty = 3, lwd = lwd)
      segments(s.lower[1:(n - 1)], top[2:n], s.lower[2:n], top[2:n],
               col = col, lty = 3, lwd = lwd)
    }
    # Mean:
    segments(s, top, s, bot, col = col, lwd = lwd, lty = lty)
    segments(s[1:(n - 1)], top[2:n], s[2:n], top[2:n], col = col, lwd = lwd, lty = lty)
  }else{
    if(bounds == TRUE){
      # Upper:
      segments(top, s.upper, bot, s.upper, col = col, lty = 3, lwd = lwd)
      segments(top[2:n], s.upper[1:(n - 1)], top[2:n], s.upper[2:n],
               col = col, lty = 3, lwd = lwd)
      # Lower:
      segments(top, s.lower, bot, s.lower, col = col, lty = 3, lwd = lwd)
      segments(top[2:n], s.lower[1:(n - 1)], top[2:n], s.lower[2:n],
               col = col, lty = 3, lwd = lwd)
    }
    # Mean:
    segments(top, s, bot, s, col = col, lwd = lwd, lty = lty)
    segments(top[2:n], s[1:(n - 1)], top[2:n], s[2:n], col = col, lwd = lwd, lty = lty)
  }
}
