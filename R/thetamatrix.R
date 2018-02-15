thetamatrix <- function(z, v, bot, hoffset){
  nlayers <- length(bot)
  ntt <- length(z)
  thetamatrix <- matrix(data = 0, nrow = ntt, ncol = nlayers)
  top <- c(0, bot[1:(nlayers - 1)])
  h <- bot - top
  for(i in 1:ntt){
    path <- path4sl(z = z[i], d2bot = bot, vel = v, hoffset = hoffset)
    thetamatrix[i, (1:length(path))] <- path
  }
  return(thetamatrix)
}
