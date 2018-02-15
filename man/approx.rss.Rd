\name{approx.rss}
\alias{approx.rss}
\title{Approximate Residual Sum of Squares}
\description{
  Only to be used by \code{autopick}. Finds the approximate residual sum
  of squares as a function of an added interface, \code{newZ}. Note that
  when a model returns a negative velocity, the RSS is calculated as
  simply the sum of the travel-time values squared so that it is
  guaranteed to be very large relative to the other RSS, so that these
  models will never be chosen by \code{autopick}. 
}
\usage{
approx.rss(newZ, mod)
}
\arguments{
  \item{newZ}{depth of new layer interface.}
  \item{mod}{the model that the interface is to be added to. Returned
    from \code{regress} or \code{snellregress}.}
}
\value{approximate residual sum of squares.}
\seealso{\code{\link{autopick}}}
\author{Eric M. Thompson <eric.thompson@tufts.edu>}
\examples{
# Used internally by autopick()

}
\keyword{internal}
