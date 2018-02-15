\name{path4sl}
\alias{path4sl}
\title{Snell's Law Raypath}
\description{
  Calculates the array of angles of incidence for the raypath that is
  constrained by Snell's Law.
  
  This function is based on the FORTRAN subroutine \code{path4sl} by\cr
     David Boore <boore@usgs.gov>\cr
     U.S. Geological Survey\cr
     Mail Stop 977\cr
     345 Middlefield Road\cr
     Menlo Park, CA 94025 USA\cr

}
\usage{
path4sl(z, d2bot, vel, hoffset)
}
\arguments{
  \item{z}{array of depth to each travel time measurement.}
  \item{d2bot}{array of depth to bottom of layer interfaces.}
  \item{vel}{array of velocities for each layer.}
  \item{hoffset}{horizontal offset from borehole at surface.}
}
\value{returns the array of angles of incidence for the Snell's Law
  raypath.}
\seealso{\code{\link{thetamatrix}}}
\author{Eric M. Thompson <eric.thompson@tufts.edu>}
\examples{
# Used internally by thetamatrix()

}
\keyword{internal}
