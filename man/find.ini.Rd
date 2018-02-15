\name{find.ini}
\alias{find.ini}
\title{Find initial values for optimization}
\description{
  Only to be used by \code{autopick}. Sorts the layers based on the
  n-weighted RSS.
}
\usage{
find.ini(mod)
}
\arguments{
  \item{mod}{the model that the interface is to be added to. Returned
    from \code{regress} or \code{snellregress}.}
}
\value{list with the following elements:
  \item{top}{ordered array of depths to the tops of each layer.}
  \item{mid}{ordered array of depths to the middle of each layer.}
  \item{bot}{ordered array of depths to the bottom of each layer.}
}
\seealso{\code{\link{autopick}}}
\author{Eric M. Thompson <eric.thompson@tufts.edu>}
\examples{
# Used internally by autopick()

}
\keyword{internal}
