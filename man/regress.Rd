\name{regress, snellregress}
\alias{regress}
\alias{snellregress}
\title{Find the best fit slowness model}
\description{
  Finds the slownesses for layers given
  the observed data and depth to boundaries. \code{snellregress} is the
  same except that it uses the raypath calculated by
  \code{path4sl}. The raypath is function of the slowness model, and so
  this must be iterated to find a solution.
}
\usage{
regress(z, tt, bot, theta = 0, hoffset = 0, wt = NULL, sig = NULL)
snellregress(z, tt, bot, hoffset, wt = NULL, sig = NULL) 
}
\arguments{
  \item{z}{array of depths of each travel time measurement.}
  \item{tt}{array of corresponding observed travel times.}
  \item{bot}{array of depth to layer bottoms.}
  \item{theta}{array of angles of incidence of the raypath for each
    layer.}
  \item{hoffset}{horizontal offset from the borehole.}
  \item{wt}{array of weights for the observations calculated from sig.}
  \item{sig}{the standard deviation of the observations.}
}
\value{a list containing all the model parameters of the regression.}
\seealso{\code{\link{snellregress}}, \code{\link{path4sl}},
          \code{\link{thetamatrix}} }
\author{Eric M. Thompson <eric.thompson@tufts.edu>}
\references{
  Boore, D. M. (2003)
  \emph{A compendium of p- and s-wave velocities from
    surface-to-borehole logging: Summary and reanalysis of previously
    published data and analysis of unpublished data} U.S. Geological
  Survey Open-File Report 03-191.
  Faraway, J. J. (2005)
  \emph{Linear Models with R} Chapman & Hall/CRC. 
}
\examples{
# Used internally by Rvelslant()

}
\keyword{internal}
