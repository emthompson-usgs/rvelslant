\name{autopick}
\alias{autopick}
\title{Automatically Pick Depths For A Layered Velocity Model}
\description{
  Finds depth to boundaries by finding where the travel time curve
  changes slope.  This function has lots of loops, so it is very slow.
}
\usage{
autopick(data, snell = TRUE)
}
\arguments{
  \item{data}{a list containing the following elements:
    \describe{
      \item{\code{z}:}{array of the depths of travel time measurements, in
        meters.}
      \item{\code{tt.slant}:}{array of observed travel time measurements
	in seconds.} 
      \item{\code{hoffset}:}{the horizontal offset at the surface from the
        borehole.} 
      \item{\code{sig}:}{array of the standard deviation of the travel time 
        measurements normalized to the standard deviation of the best
        pick.  Defaults to 1 if none is given. Used for weighting the 
        travel time measurements in the regression.}
      }
  }
  \item{snell}{logical value for if refractions at layer boundaries
    should be calculated.}
}
\value{numeric array of depth to layer interfaces.}
\seealso{\code{\link{Rvelslant}}}
\author{Eric M. Thompson <eric.thompson@tufts.edu>}
\examples{
# Used internally by Rvelslant()

}
\keyword{internal}
