regress <- function(z, tt, bot, theta = 0, hoffset = 0, wt = NULL, sig = NULL){
  # NOTE: Only send tt.slant to regress() and let it calculate
  # the tt.vrt if needed when hoffset is greater than zero and
  # theta-matrix is given.
  tt.slant <- tt
  allz <- z
  alltt <- tt
  allwt <- wt
  allsig <- sig
  keep <- z <= max(bot)
  tt <- tt[keep]
  z <- z[keep]
  wt <- wt[keep]
  sig <- sig[keep]
  
  # Theta = array of angles of inclination for each layer
  require(MASS)
  # n = number of layers
  n <- length(bot)
  
  # k = number of measurements
  k <- length(tt)
  
  if(n > 1){
    top <- c(0, bot[1:(n - 1)])
  }else{
    top <- 0
  }
  
  # Layer thickness:
  h <- bot - top
  dim(h) <- c(n, 1)
  
  # Use tt.vrt if theta = 0 and hoffset is > 0
  # This is important for the first iteration of
  # snellregress() when no theta is given, but
  # there is an hoffset.
  if(all(theta == 0)){
####  if(all(theta == 0) & hoffset > 0){
    # tt <- tt * z/sqrt(hoffset^2 + z^2)
    theta <- atan2(hoffset, z)
  }else{
    theta <- theta[keep]
  }
  
  # N transits:
  N <- matrix(data = 0, nrow = k, ncol = n)
  for(i in 1:k){
    f1 <- bot < z[i]
    N[i, f1] <- 1
    f2 <- top < z[i]
    diff <- z[i] - top[f2][length(top[f2])]
    r <- diff/(h[f2][length(bot[f2])])
    N[i, length(bot[f2])] <- r
  }
  
  # Weight matrix:
  W <- matrix(data = 0, nrow = k, ncol = k)
  if(!is.null(wt)){
    W[cbind(1:k, 1:k)] <- wt
  }else{
    wt <- rep(1, k)
    W[cbind(1:k, 1:k)] <- wt
  }
  
  # Thickness matrix:
  H <- matrix(data = NA, nrow = k, ncol = n)
  for(i in 1:k){
    H[i, ] <- t(h)
  }
  
  A <- H * N / cos(theta)
  dim(tt) <- c(k, 1)
  
  ATA <- t(A) %*% W %*% (A)
  ATAinv <- ginv(ATA)
  
  s <- ATAinv %*% t(A) %*% W %*% (tt)
  v <- 1/s
  
  
  # Calculate the predicted layered model travel times:
  S <- matrix(data = NA, nrow = k, ncol = n)
  for(i in 1:length(z)){
    S[i, ] <- t(s)
  }
  tt.hat <- A %*% s
  
  res <- tt - tt.hat
  
  # Estimate sigma:
  sigma.hat <- sqrt(sum(res^2 * wt)/(k - n))
  
  # Standard error of coefficients:
  se <- sqrt(diag(ATAinv)) * sigma.hat
  
  v.upper <- 1/(s - se)
  v.lower <- 1/(s + se)
  
  mod  <- list(A = A, s = s, v = v, n = n, res = res, 
               k = k, h = h, bot = bot, tt = alltt, z = allz,
               hoffset = hoffset, N = N, theta = theta,
               tt.hat = tt.hat, wt = allwt, sig = allsig, se = se,
               sigma.hat = sigma.hat, v.upper = v.upper,
               v.lower = v.lower, tt.slant = tt.slant)
  return(mod)
}
