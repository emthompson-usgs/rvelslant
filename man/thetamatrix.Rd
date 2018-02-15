\name{thetamatrix}
\alias{thetamatrix}
\title{Calculate The Matrix Of Angles of Incidence}
\description{
  Calculates the matrix that contains the angles of incidence for the
  the raypath for each travel time measurement. The matrix has ncols =
  number of layers in the velocity model, and nrows = number of
  travel  time measurements.
}
\usage{
thetamatrix(z, v, bot, hoffset)
}
\arguments{
  \item{z}{array (length = \eqn{k}) of depth to each travel time
    measurement.}
  \item{v}{array (length = \eqn{n}) of velocities that correspond to
    each layer defined by bot.} 
  \item{bot}{array (length = \eqn{n}) of the depth to bottom of each
    layer such that the  bottom of 
the last layer is the location the source (or receiver).}
  \item{hoffset}{\eqn{k} by \eqn{n} numeric value of horizontal offset
    from borehole at surface.}
}
\value{\eqn{k} by \eqn{n} matrix of angles of incidence.}
\seealso{\code{\link{snellregress}}, \code{\link{path4sl}}}
\author{Eric M. Thompson <eric.thompson@tufts.edu>}
\examples{
# Used internally by snellregress()

}
\keyword{internal}
