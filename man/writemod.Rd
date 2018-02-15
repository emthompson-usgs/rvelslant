\name{writemod}
\alias{writemod}
\title{Save Layered Velocity Model}
\description{
  Write two  fixed-width text files. One is the layered velocity model
  that also contains the observed travel times, the predicted travel
  times and has the extension \sQuote{.mod}. The other file is a stair-step
  function for the velocity model for making plotting the model easier
  and has the extension \sQuote{.ss}.
}
\usage{
writemod(mod, prefix)
}
\arguments{
  \item{mod}{a velocity model returned from \code{velslant}.}
  \item{prefix}{prefix to be used in the name of the  of files. If path
    is not included then it is saved to the current directory.}
}
\value{none}
\seealso{\code{\link{Rvelslant}}}
\author{Eric M. Thompson <eric.thompson@tufts.edu>}
\examples{
# See ?Rvelslant for example.

}
\keyword{}
