\name{slowprofile, velprofile}
\alias{velprofile}
\alias{slowprofile}
\title{Display the slowness or velocity profile}
\description{
  This function plots the layered velocity or slowness model versus
  depth.
}
\usage{
velprofile(mod, add = FALSE, col = "blue", lty = 1, lwd = 1, new = TRUE,
           nticks = NULL, grid = FALSE, axes = TRUE, depth = "v",
           bounds = TRUE)
slowprofile(mod, add = FALSE, col = "blue", lty = 1, lwd = 1,
            new = TRUE, nticks = NULL, grid = FALSE, axes = TRUE,
            depth = "v", bounds = TRUE)
}
\arguments{
  \item{mod}{a velocity model returned from \code{Rvelslant}.}
  \item{add}{logical value for if the model will be added to a previous
    profile for comparison.}
  \item{col}{line color for the profile.}
  \item{lty}{line type for the profile.}
  \item{lwd}{line width for the profile.}
  \item{new}{logical value for if a new window will be created for the
    profile.}
  \item{nticks}{approximate number of tick marks desired for depth-axis
    on travel-time plots. See \code{pretty} function in R package
    \sQuote{base}.}
  \item{grid}{logical value for plotting grid lines.}
  \item{axes}{see \code{plot.default}.}
  \item{depth}{layout of plots. Value can be \sQuote{v} for vertical or
    \sQuote{h} for horizontal orientation of depth on the travel-time
    plots.} 
  \item{bounds}{logical value for if the upper and lower bounds should
    be plotted.}
}
\value{none}
\seealso{\code{\link{Rvelslant}}}
\author{Eric M. Thompson <eric.thompson@tufts.edu>}
\examples{
# See ?Rvelslant for example.

}
\keyword{}
