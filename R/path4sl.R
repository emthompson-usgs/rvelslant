path4sl <- function(z, d2bot, vel, hoffset){
# Calculates the take off angle for a measurement at depth z
# and returns the array of thetas for raypath.

# This function is based on the Fortran subroutine "path4sl" by 
# David Boore <boore@usgs.gov>
# U.S. Geological Survey
# Mail Stop 977
# 345 Middlefield Road
# Menlo Park, CA 94025 USA
  
  nlayers <- length(vel)
  eps <- 0.00001
  dtor <- pi/180
  thick <- diff(c(0, d2bot))
  top <- d2bot - thick
  # Find number of layers for z and assign
  # vertical distances travelend in this and
  # layers above this layer:
  z_in_layer <- rep(0, nlayers)
  f1 <- d2bot < z
  z_in_layer[f1] <- thick[f1]
  f2 <- top < z
  diff <- z - top[f2][length(top[f2])]
  z_in_layer[length(d2bot[f2])] <- diff

  ilay_z <- (1:nlayers)[z_in_layer > 0]
  vmax <- max(vel[ilay_z])
  pcrit <- 1/vmax
  # Set the bounds for the takeoff angles to be used in the root finding
  #  routine:
  p1 <- 0
  p2 <- (1 - eps) * pcrit
  while(p2 * vmax > 1){
    p2 <- p2 - eps * pcrit
  }
  
  # Check to see if root is bounded:
  xdiff <- hoffset_x(p = p2, vel = vel, hoffset = hoffset, ilay_z = ilay_z,
                     z_in_layer = z_in_layer)
  
  if (xdiff >= 0){
    #  Root not bounded
    cat(' *** Root not bounded ***\n')
    #    stop, return theta = NA
    theta <- NA
  }else{
    # Set the tolerance for the root finder:
    tol <- eps * pcrit
    
    # Find the root:
    p <- uniroot(f = hoffset_x, interval = c(p1, p2), tol = tol, vel = vel,
                 hoffset = hoffset, ilay_z = ilay_z, z_in_layer = z_in_layer)$root
    pxvel <- p * vel
    theta <- asin(pxvel[ilay_z])
    cos_theta <- cos(theta)
    tan_theta <- tan(theta)
    dist_in_layer <- z_in_layer[ilay_z]/cos_theta
    x_in_layer <- z_in_layer[ilay_z] * tan_theta
    xcum <- sum(x_in_layer)
    
    if(p == 0.0){
      vapp <- 9999.0
    }else{
      vapp <- 1.0/p
    }
    pxvel <- vel[x_in_layer > 0] * p
    emrgangl <- asin(pxvel[ilay_z])/dtor
    
    pxvel <- p * vel[1] 
    toar <- asin(pxvel[ilay_z])
    # Take off angle:
    toa <- toar / dtor
  }
  return(theta)
}

hoffset_x <- function(p, vel, hoffset, ilay_z, z_in_layer){
# Computes the difference between hoffset and the distance the ray
# travels in reaching the receiver depth.
  x = 0.0
  pxvel <- p * vel[ilay_z]
  theta <- asin(pxvel)
  theta_d <- 57.30 * theta
  tan_theta <- tan(asin(pxvel))
  x <- sum(z_in_layer[ilay_z] * tan_theta)
#  for(i in ilay_z){
#    pxvel <- p * vel[i]
#    theta <- asin(pxvel)
#    theta_d <- 57.30 * theta
#    tan_theta <- tan(asin(pxvel))
#    x <- x + z_in_layer[i] * tan_theta
#  }
  hoffset_x <- hoffset - x
  return(hoffset_x)
}




