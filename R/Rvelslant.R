Rvelslant <- function(data, snell = TRUE, bot = NULL, auto = FALSE, cex = 1,
                      nticks = NULL, grid = FALSE, depth = "v", profile = "slow")
{
  z <- data$z
  zmax <- max(z) # Check that added boundaries must be within the range
  zmin <- min(z) # of z measurements.
  tt.slant <- data$tt.slant; hoffset <- data$hoffset
  if(is.null(data$sig)) data$sig <- rep(1, length(z))
  if(is.null(data$hole.code)) data$hole.code <- "none"
  sig <- data$sig; wt <- 1/sig^2; data$wt <- wt
  if(length(z) > 0){
    ###################################################
    # Pick the layer boundaries
    if(is.null(bot)){
      bot <- max(z)
    }
#    else{
#      if(!all(bot >= zmin & bot <= zmax)){
#        cat("Rejected bot values:\n")
#        cat("-> All boundaries must be within the range of measured travel times!\n")
#        bot = max(z)
#      }
#    }
    if(auto == TRUE){
      cat("Calculating layer boundaries...")
      bot <- autopick(data, snell = snell)
      cat("done.\n")
    }
    if(snell == TRUE & hoffset > 0){
      cat("Calculating layer slowness with refractions:\n  i = ")
      mod <- snellregress(z = z, tt = tt.slant, bot = bot,
                          hoffset = hoffset, wt = wt, sig = sig)
      N <- length(mod$tt.hat)
      cat("done.\n")
      RSS <- sum((mod$tt[1:N] - mod$tt.hat)^2)
      K <- length(mod$s) + 1
      AICc <- 2 * K + N * log(RSS/N) + 2 * K * (K + 1)/(N - K - 1)
      cat(paste(sep = "", "AICc = ", AICc, "\n"))
    }else{
      cat("Calculating layer slowness without refractions...")
      mod <- regress(z = z, tt = tt.slant, bot = bot, hoffset = hoffset,
                     wt = wt, sig = sig)
      N <- length(mod$tt.hat)
      cat("done.\n")
      RSS <- sum((mod$tt[1:N] - mod$tt.hat)^2)
      K <- length(mod$s) + 1
      AICc <- 2 * K + N * log(RSS/N) + 2 * K * (K + 1)/(N - K - 1)
      cat(paste(sep = "", "AICc = ", AICc, "\n"))
    }
    if(depth == "v")
    {
      par(mfrow = c(1, 3), las = 1, xaxs = "r", yaxs = "r",
          cex = cex, mar = c(1, 4, 4, 1))
      plotmod(mod, nticks, grid, depth = depth)
      plotresid(mod, nticks, grid, depth = depth)
      if(profile == "slow")
        slowprofile(mod, new = FALSE, nticks = nticks, grid = grid, depth = depth)
      else
        velprofile(mod, new = FALSE, nticks = nticks, grid = grid, depth = depth)
    }
    else
    {
      par(mfrow = c(3, 1), las = 0, xaxs = "r", yaxs = "r",
          cex = cex, mar = c(4, 4, 1, 2))
      plotmod(mod, nticks, grid, depth = depth)
      plotresid(mod, nticks, grid, depth = depth)
      if(profile == "slow")
        slowprofile(mod, new = FALSE, nticks = nticks, grid = grid, depth = depth)
      else
        velprofile(mod, new = FALSE, nticks = nticks, grid = grid, depth = depth)
    }
    f <- TRUE
    cat(paste(sep = "", rep("-", 50), collapse = ""))
    cat("\nTo ADD a boundary, click on the travel-time or residual plots.\n")
    cat("To REMOVE a boundary, click near a boundary on the velocity profile.\n")
    cat("When finished, right-click on anywhere.\n")
    cat("    (in Windows, then select Stop).\n")
    cat("    (in Mac OSX, alternatively use ESC key).\n")
    while(f == TRUE){
      loc <- locator(1)
      if(depth == "h"){
        lx <- loc$x
        ly <- loc$y
        loc$x <- ly
        loc$y <- lx
        if(!is.null(loc)){
          if(profile == "slow"){
            if(loc$x > 1100 * max(mod$s + mod$se)) loc$x <- -1
          }else{
            if(loc$x > 1.1 * max(mod$v.upper)) loc$x <- -1
          }
        }
      }
      if(is.null(loc)){
        f <- FALSE
      }else{
        if(loc$y >= zmin & loc$y <= zmax){
          if(loc$x < 0){
            # Add boundary
            cat(paste(sep = "", " - New boundary: ", round(loc$y, 2), "\n"))
            bot <- append(loc$y, bot)
          }else{
            # Remove Boundary
            diff <- abs(bot - loc$y)
            min <- min(diff)
            rm <- diff == min
            bot <- bot[!rm]
            cat(paste(sep = "", " - Removed boundary: ", round(bot[rm], 2), "\n"))
          }
          
          # Regression after each layer is added
          bot <- sort(bot)
          if(snell == TRUE & hoffset > 0){
            cat("Calculating layer slowness with refractions:\n  i = ")
            mod <- snellregress(z = z, tt.slant, bot = bot, hoffset = hoffset,
                                wt = wt, sig = sig)
            N <- length(mod$tt.hat)
            cat("done.\n")
            RSS <- sum((mod$tt[1:N] - mod$tt.hat)^2)
            K <- length(mod$s) + 1
            AICc <- 2 * K + N * log(RSS/N) + 2 * K * (K + 1)/(N - K - 1)
            cat(paste(sep = "", "AICc = ", AICc, "\n"))
          }else{
            cat("Calculating layer slowness without refractions...")
            mod <- regress(z = z, tt.slant, bot = bot, hoffset = hoffset,
                           wt = wt, sig = sig)
            N <- length(mod$tt.hat)
            cat("done.\n")
            RSS <- sum((mod$tt[1:N] - mod$tt.hat)^2)
            K <- length(mod$s) + 1
            AICc <- 2 * K + N * log(RSS/N) + 2 * K * (K + 1)/(N - K - 1)
            cat(paste(sep = "", "AICc = ", AICc, "\n"))
          }
          if(depth == "v"){
            par(mfrow = c(1, 3), las = 1, xaxs = "r", yaxs = "r",
                cex = cex, mar = c(1, 4, 4, 1))
            plotmod(mod, nticks, grid, depth = depth)
            plotresid(mod, nticks, grid, depth = depth)
            if(profile == "slow"){ slowprofile(mod, new = FALSE, nticks = nticks, grid = grid, depth = depth)
            }else{ velprofile(mod, new = FALSE, nticks = nticks, grid = grid, depth = depth)}
          }else{
            par(mfrow = c(3, 1), las = 1, xaxs = "r", yaxs = "r",
                cex = cex, mar = c(4, 4, 1, 2))
            plotmod(mod, nticks, grid, depth = depth)
            plotresid(mod, nticks, grid, depth = depth)
            if(profile == "slow"){ slowprofile(mod, new = FALSE, nticks = nticks, grid = grid, depth = depth)
            }else{ velprofile(mod, new = FALSE, nticks = nticks, grid = grid, depth = depth)}
          }
        }
#        else{
#          cat("Rejected pick:\n")
#          cat("-> All boundaries must be within the range of measured travel times!\n")
#        }
      }
    }
    # Print model to screen:
    cat(paste(sep = "", rep("-", 50), collapse = ""))
    cat(paste(sep = "", "\nLayered model:\n"))
    cat(paste(sep = "", "AICc = ", AICc, "\n"))
    cat(paste(sep = "\t", "bot", "h", "v", "\n"))
    for(i in 1:length(bot)){
      cat(paste(sep = "\t", round(bot[i], 2), round(mod$h[i], 2),
                round(mod$v[i], 2), "\n"))
    }
    mod$hole.code <- data$hole.code
    mod$snell <- snell
    return(mod)
  }
}
