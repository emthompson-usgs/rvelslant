\name{fixwnum}
\alias{fixwnum}
\title{Fixed Width Number}
\description{
  Returns a fixed with CHARACTER that corresponds to the input
  number. This is called by \code{writemod}, and helps make the files
  more readable.
}
\usage{
fixwnum(num, l, dp)
}
\arguments{
  \item{num}{number to be formatted.}
  \item{l}{total length of resulting character, aka the width.}
  \item{dp}{number of decimal places.}
}
\value{properly formatted character.}
\seealso{\code{\link{writemod}}}
\author{Eric M. Thompson <eric.thompson@tufts.edu>}
\examples{
# Used internally by writemod()

}
\keyword{internal}
