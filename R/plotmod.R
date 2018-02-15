plotmod <- function(mod, nticks = NULL, grid = FALSE, depth = "v",
                    axes = TRUE){
  z <- mod$z; tt <- mod$tt; hoffset <- mod$hoffset
  bot <- mod$bot; h <- mod$h; top <- bot - h; s <- mod$s
  tt.hat <- mod$tt.hat; sig <- mod$sig
  sigcol <- rep("white", length(sig))
  sigcol[sig == 1] <- "black"; sigcol[sig == 2] <- "blue"
  sigcol[sig == 3] <- "cyan"; sigcol[sig == 4] <- "magenta"
  sigcol[sig == 5] <- "red"
  if (all(mod$theta == 0)) {
    t0 <- 0
  }else{
    t0 <- hoffset * s[1]
  }
  if(!is.null(nticks)){
    tcks <- pretty(x = z, n = nticks)
  }else{
    tcks <- pretty(x = z)
  }
  if(depth == "v"){
    plot(tt, z, ylim = c(max(z), 0), xlim = c(0, max(tt)), axes = FALSE,
      xlab = "", ylab = "", type = "n", main = "")
    if(axes == TRUE){
      axis(side = 3)
      axis(side = 2, at = tcks)
    }
    if(grid == TRUE){
      abline(h = tcks, col = "grey", lty = 1)
      abline(v = pretty(x = tt), col = "grey", lty = 1)
      box(col = "black", lty = 1)
    }
    title(ylab = "Depth, m")
    title(main = "Travel Time, sec", line = 2)
    lines(c(t0, tt.hat), c(0, z[1:length(tt.hat)]), col = "blue")
    abline(h = bot, col = "red")
    points(tt, z, pch = 16, col = sigcol)
  }else{
    plot(z, tt, xlim = c(0, max(z)), ylim = c(0, max(tt)), axes = FALSE,
      xlab = "", ylab = "", type = "n", main = "")
    if(axes == TRUE){
      axis(side = 2)
      axis(side = 1, labels = FALSE, at = tcks)
    }
    if(grid == TRUE){
      abline(v = tcks, col = "grey", lty = 1)
      abline(h = pretty(x = tt), col = "grey", lty = 1)
      box(col = "black", lty = 1)
    }
    title(ylab = "Travel Time, sec")
    lines(c(0, z[1:length(tt.hat)]), c(t0, tt.hat), col = "blue")
    abline(v = bot, col = "red")
    points(z, tt, pch = 16, col = sigcol)
  }
}
