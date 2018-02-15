\name{plotmod, plotresid}
\alias{plotmod}
\alias{plotresid}
\title{Plot Travel Times And Residuals}
\description{
  \code{plotmod} plots the predicted versus observed travel time
  curves.\cr
  \code{plotresid} plots the residuals.
}
\usage{
plotmod(mod, nticks = NULL, grid = FALSE, depth = "v", axes = TRUE)
plotresid(mod, nticks = NULL, grid = FALSE, depth = "v", axes = TRUE)
}
\arguments{
  \item{mod}{list of velocity model parameters returned by
    \code{regress}.}
  \item{nticks}{approximate number of tick marks desired for travel time
    plots.}
  \item{grid}{logical value for plotting grid lines.}
  \item{depth}{layout of plots. Value can be "v" for vertical or "h" for
    horizontal orientation of depth on the travel time plots.}
  \item{axes}{see \code{plot.default}.}
}
\value{none}
\seealso{\code{\link{Rvelslant}}}
\author{Eric M. Thompson <eric.thompson@tufts.edu>}
\examples{
# Used internally by Rvelslant() to create the interactive
# plots for adding and removing boundaries by clicking
# with mouse.

# User can also call: see ?Rvelslant for example

}
\keyword{}
