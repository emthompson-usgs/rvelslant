\name{nlayers}
\alias{nlayers}
\title{Number of observations in each layer}
\description{
Calculates the number of travel-time observations that are located in
each layer given the depths to the travel time measurements \code{z} and
the depths to layer interfaces \code{bot}.
}
\usage{
nlayers(z, bot)
}
\arguments{
  \item{z}{depths of travel-time measurements.}
  \item{bot}{depths to layer interfaces.}
}
\value{an array the length of \code{bot} of the number of travel-time
  observations in each layer.}
\seealso{\code{\link{find.ini}}}
\author{Eric M. Thompson <eric.thompson@tufts.edu>}
\examples{
# Used internally by find.ini()

}
\keyword{internal}
