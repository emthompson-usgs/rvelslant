\name{hoffset_x}
\alias{hoffset_x}
\title{Difference between hoffset and the distance from the borehole
  calculated for a particular ray parameter.}
\description{
  Calculates the difference between the hoffset and the horizontal
  distance that the ray travels in reaching the receiver depth.
}
\usage{
hoffset_x(p, vel, hoffset, ilay_z, z_in_layer)
}
\arguments{
  \item{p}{ray parameter.}
  \item{vel}{array of velocities for each layer.}
  \item{hoffset}{horizontal offset of source from borehole.}
  \item{ilay_z}{idex values for layers traversed by ray.}
  \item{z_in_layer}{vertical distance traveled in each layer by the
ray.}
}
\value{hoffset - x.}
\seealso{\code{\link{path4sl}}}
\author{Eric M. Thompson <eric.thompson@tufts.edu>}
\examples{
# Used internally by path4sl()

}
\keyword{internal}
