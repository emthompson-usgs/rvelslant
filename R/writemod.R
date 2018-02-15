writemod <- function(mod, prefix)
{
  snell <- mod$snell
  tt.slant <- mod$tt.slant
  tt.vrt <- tt.slant * mod$z/sqrt(mod$hoffset^2 + mod$z^2)
  N <- length(mod$tt.hat)
  # Calc the AICc:
  RSS <- sum((mod$tt[1:N] - mod$tt.hat)^2)
  K <- length(mod$s) + 1
  AICc <- 2 * K + N * log(RSS/N) + 2 * K * (K + 1)/(N - K - 1)
  ######## Write the model file
  zz <- file(paste(sep = "", prefix, ".mod"), "w")
  cat(paste(sep = "", "Hole Code ", mod$hole.code, "\n"), file = zz)
  cat(paste(sep = "", "Snell's Law = ", snell, " \n"), file = zz)
  cat(paste(sep = "", "hoffset = ", mod$hoffset, " \n"), file = zz)
  cat(paste(sep = "", rep("-", 80), collapse = ""), file = zz)
  cat(paste(sep = "", "\n"), file = zz)
  cat(paste(sep = "", "Velocity Model:\n"), file = zz)
  cat(paste(sep = "", "N = ", N, "\n"), file = zz)
  cat(paste(sep = "", "K = ", K, "\n"), file = zz)
  cat(paste(sep = "", "SSR = ", RSS, "\n"), file = zz)
  cat(paste(sep = "", "AICc = ", AICc, "\n"), file = zz)
  
  cat(paste(sep = "", "       d2b", " thickness", "  slowness", "  slow+1se", "  slow-1se",
            "       vel", "   vel+1se", "   vel-1se", "\n"), file = zz)
  for(i in 1:length(mod$bot))
  {
    chrs <- NULL
    chrs[1] <- fixwnum(mod$bot[i], 10, 2)
    chrs[2] <- fixwnum(mod$h[i], 10, 2)
    chrs[3] <- fixwnum(1000*mod$s[i], 10, 5)
    chrs[4] <- fixwnum(1000 * (mod$s[i] + mod$se[i]), 10, 5)
    chrs[5] <- fixwnum(1000 * (mod$s[i] - mod$se[i]), 10, 5)
    chrs[6] <- fixwnum(mod$v[i],  10, 2)
    chrs[7] <- fixwnum(mod$v.upper[i],  10, 2)
    chrs[8] <- fixwnum(mod$v.lower[i],  10, 2)
    cat(paste(sep = "", chrs[1], chrs[2], chrs[3], chrs[4], chrs[5],
              chrs[6], chrs[7], chrs[8], "\n"), file = zz)
  }
  cat(paste(sep = "", rep("-", 60), collapse = ""), file = zz)
  cat("\nTravel time data:\n", file = zz)
  cat(paste(sep = "", "         z", "    tt_obs", "   tt_pred", " residuals",
            "       sig", "      vavg","\n"), file = zz)
  for(i in 1:length(mod$tt.hat))
  {
    vavg <- mod$z[i]/tt.vrt[i]
    chrs <- NULL
    chrs[1] <- fixwnum(mod$z[i], 10, 2)
    chrs[2] <- fixwnum(tt.slant[i], 10, 5)
    chrs[3] <- fixwnum(mod$tt.hat[i], 10, 5)
    chrs[4] <- fixwnum(mod$res[i], 10, 5)
    chrs[5] <- fixwnum(mod$sig[i], 10, 2)
    chrs[6] <- fixwnum(vavg, 10, 2)
    cat(paste(sep = "", chrs[1], chrs[2], chrs[3],chrs[4], chrs[5], chrs[6], "\n" ),
        file = zz)
  }
  close(zz)
  ######## Write the stairstep file
  zz <- file(paste(sep = "", prefix, ".ss"), "w")
  bot <- mod$bot; h <- mod$h; top <- bot - h
  s <- mod$s; s.upper <- s + mod$se; s.lower <- s - mod$se
  v <- mod$v; v.upper <- mod$v.upper; v.lower <- mod$v.lower
  n <- length(v)
  if(n > 1)
    z <- sort(c(0, bot, bot[1:(n - 1)]))
  else
    z <- sort(c(0, bot))
  S <- NULL; Su <- NULL; Sl <- NULL
  V <- NULL; Vu <- NULL; Vl <- NULL
  for(i in 1:n)
  {
    S  <- append(S, c(s[i], s[i]))
    Su <- append(Su, c(s.upper[i], s.upper[i]))
    Sl <- append(Sl, c(s.lower[i], s.lower[i]))
    V  <- append(V, c(v[i], v[i]))
    Vu <- append(Vu, c(v.upper[i], v.upper[i]))
    Vl <- append(Vl, c(v.lower[i], v.lower[i]))
  }
  cat(paste(sep = "", "Hole Code ", mod$hole.code, "\n"), file = zz)
  cat(paste(sep = "", "Snell's Law = ", snell, " \n"), file = zz)
  cat(paste(sep = "", "hoffset = ", mod$hoffset, " \n"), file = zz)
  cat(paste(sep = "", rep("-", 70), collapse = ""), file = zz)
  cat(paste(sep = "", "\n"), file = zz)
  cat(paste(sep = "", "Stairstep model:\n"), file = zz)
  cat(paste(sep = "", "       d2b", "  slowness", "  slow+1se", "  slow-1se",
            "       vel", "   vel+1se", "   vel-1se", "\n"), file = zz)
  for(i in 1:length(z))
  {
    chrs <- NULL
    chrs[1] <- fixwnum(z[i], 10, 2)
    chrs[2] <- fixwnum(1000*S[i], 10, 5)
    chrs[3] <- fixwnum(1000*Su[i], 10, 5)
    chrs[4] <- fixwnum(1000*Sl[i], 10, 5)
    chrs[5] <- fixwnum(V[i], 10, 2)
    chrs[6] <- fixwnum(Vu[i],  10, 2)
    chrs[7] <- fixwnum(Vl[i],  10, 2)
    cat(paste(sep = "", chrs[1], chrs[2], chrs[3], chrs[4], chrs[5],
              chrs[6], chrs[7], "\n"), file = zz)
  }
  close(zz)
}
