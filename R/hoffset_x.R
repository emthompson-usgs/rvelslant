
hoffset_x <- function(p, vel, hoffset, ilay_z, z_in_layer){
# Computes the difference between hoffset and the distance the ray
# travels in reaching the receiver depth.
  x = 0.0
  for(i in ilay_z){
    pxvel <- p * vel[i]
    theta <- asin(pxvel)
    theta_d <- 57.30 * theta
    tan_theta <- tan(asin(pxvel))
    x <- x + z_in_layer[i] * tan_theta
  }
  hoffset_x <- hoffset - x
  return(hoffset_x)
}




