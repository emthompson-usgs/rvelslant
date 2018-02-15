velprofile <- function(mod, add = FALSE, col = "blue", lty = 1, lwd = 1,
                       new = TRUE, nticks = NULL, grid = FALSE, axes = TRUE,
                       depth = "v", bounds = TRUE){
  if(new == TRUE) par(mfrow = c(1, 1), las = 1, xaxs = "r", yaxs = "r", tcl = 0.2)
  bot <- mod$bot; h <- mod$h; top <- bot - h
  v <- mod$v; v.upper <- mod$v.upper; v.lower <- mod$v.lower
  n <- length(v); ylim <- c(max(mod$z), 0); xlim <- c(0, max(v.upper))
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
        abline(v = pretty(x = c(v.upper, 0)), col = "grey", lty = 1)
        box(col = "black", lty = 1)
      }
      if(new == TRUE) title(ylab = "Depth, m")
      title(main = "Velocity Profile, m/s", line = 2)
    }else{
      plot(1, 1, ylim = xlim, xlim = rev(ylim), main = "",
           xlab = "", ylab = "", type = "n", axes = FALSE)
      if(axes == TRUE){
        axis(side = 2)
        axis(side = 1, at = tcks)
      }
      if(grid == TRUE){
        abline(v = tcks, col = "grey", lty = 1)
        abline(h = pretty(x = c(v.upper, 0)), col = "grey", lty = 1)
        box(col = "black", lty = 1)
      }
      title(ylab = "Velocity, m/s")
      title(xlab = "Depth, m")
    }
  }
  # Draw in the layers
  if(depth == "v"){
    if(bounds == TRUE){
      # Upper:
      segments(v.upper, top, v.upper, bot, col = col, lty = 3, lwd = lwd)
      segments(v.upper[1:(n - 1)], top[2:n], v.upper[2:n], top[2:n],
               col = col, lty = 3, lwd = lwd)
      # Lower:
      segments(v.lower, top, v.lower, bot, col = col, lty = 3, lwd = lwd)
      segments(v.lower[1:(n - 1)], top[2:n], v.lower[2:n], top[2:n],
               col = col, lty = 3, lwd = lwd)
    }
    # Mean:
    segments(v, top, v, bot, col = col, lwd = lwd, lty = lty)
    segments(v[1:(n - 1)], top[2:n], v[2:n], top[2:n], col = col, lwd = lwd, lty = lty)
  }else{
    if(bounds == TRUE){
      # Upper:
      segments(top, v.upper, bot, v.upper, col = col, lty = 3, lwd = lwd)
      segments(top[2:n], v.upper[1:(n - 1)], top[2:n], v.upper[2:n],
               col = col, lty = 3, lwd = lwd)
      # Lower:
      segments(top, v.lower, bot, v.lower, col = col, lty = 3, lwd = lwd)
      segments(top[2:n],v.lower[1:(n - 1)], top[2:n], v.lower[2:n],
               col = col, lty = 3, lwd = lwd)
    }
    # Mean:
    segments(top, v, bot, v, col = col, lwd = lwd, lty = lty)
    segments(top[2:n], v[1:(n - 1)], top[2:n], v[2:n], col = col, lwd = lwd, lty = lty)
  }
}
