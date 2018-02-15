%---------------------------------------------------
\name{tt.s, tt.p}
\docType{data}
\alias{tt.p}
\alias{tt.s}
\title{P- and S-Wave Travel Time Measurements}
\description{Dataframe of travel-time measurements for P- and S-waves.}
\usage{
data(tt.p)
data(tt.s)
}
\format{\code{tt.p} is a dataframe containing 5151 rows and \code{tt.s}
  is a dataframe containing 5364 rows. Each dataframe has the folowing
  columns:
  \tabular{rll}{
  [,1] \tab hole.code \tab An integer identifier for the site.\cr
  [,2] \tab hoffset   \tab Horizontal offset of the seismic source from borehole in meters.\cr
  [,3] \tab z         \tab Depth of each measurement in meters.\cr
  [,4] \tab tt.slant  \tab Travel-time of each measurement in seconds.\cr
  [,5] \tab sig       \tab Estimates of the standard deviation of each pick relative to the best pick.\cr
  }
}
\source{\url{http://quake.wr.usgs.gov/~boore/data_online.htm}}
\section{Background}{See reference for more information including
  descriptions and details of each hole code.}
\author{Eric M. Thompson <eric.thompson@tufts.edu>}
\references{
  Boore, D. M. (2003)
  \emph{A compendium of p- and s-wave velocities from
    surface-to-borehole logging: Summary and reanalysis of previously
    published data and analysis of unpublished data} U.S. Geological
  Survey Open-File Report 03-191.
}
\keyword{datasets}
\examples{

data(tt.p)
data(tt.s)

dim(tt.p)
# [1] 5151    5
names(tt.p)
# [1] "hole.code" "hoffset"   "z"         "tt.slant"  "sig"

dim(tt.s)
# [1] 5364    5
names(tt.s)
# [1] "hole.code" "hoffset"   "z"         "tt.slant"  "sig"

}
