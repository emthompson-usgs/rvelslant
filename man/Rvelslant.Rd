\name{Rvelslant}
\alias{Rvelslant}
\title{Downhole Seismic Analysis In R}
\description{
  The function \code{Rvelslant} is the main function for analyzing
  downhole seismic data and interpreting layered velocity models.

  The travel time at depth \eqn{z} is given by the equation:

\deqn{\sum_{i=1}^N(n_i h_i s_i) = tt_z}{sum(n_i * h_i * s_i) = tt_z}

for \eqn{i = 1, 2, \ldots, N} where \eqn{h_i} is layer
thickness, \eqn{s_i} is the layer slowness (\eqn{v = 1/s}), and
\eqn{n_i} is the number of transits of each layer (1 for all layers
above the measurement depth \eqn{z}, 0 for all layers below). When
refracted waves are considered, \eqn{h_i} is adjusted to be the
length of the raypath in each layer according to Snell's law, then:

The slowness, \eqn{s_i} for \eqn{i = 1, 2, \ldots, N}, of each layer,
is calculated by 

\deqn{A = \sum_{i=1}^N(n_i h_i s_i)}{A = sum(n_i * h_i * s_i)}

and

\deqn{s_i = (A^TA)^{-1} A^T  tt_z }{s_i = (A'A)^(-1) * A' * tt_z}

Two-point ray-tracing is calculated with the function \code{path4sl}
which is based on the FORTRAN subroutine \code{path4sl} by:

David Boore <boore@usgs.gov>\cr
U.S. Geological Survey\cr
Mail Stop 977\cr
345 Middlefield Road\cr
Menlo Park, CA 94025 USA\cr

}

\usage{
Rvelslant(data, snell = TRUE, bot = NULL, auto = FALSE, cex = 1, 
          nticks = NULL, grid = FALSE, depth = "v", profile = "slow")
}
\arguments{
  \item{data}{a list containing the following elements:
    \describe{
      \item{\code{z}:}{array of the depths of travel-time measurements, in
        meters (length = \eqn{k}).}
      \item{\code{tt.slant}:}{array of observed travel-time measurements
	in seconds (length = \eqn{k}).} 
      \item{\code{hoffset}:}{the horizontal offset at the surface from the
        borehole.} 
      \item{\code{sig}:}{array of the standard deviation of the travel-time 
        measurements normalized to the standard deviation of the best
        pick.  Defaults to 1 if none is given. Used for weighting the 
        travel-time measurements in the regression.}
      }
  }
  \item{snell}{logical value for calculating the  raypath according
    to Snell's Law, defaults is TRUE.  If \code{snell == FALSE}, then
    the raypath is approximated by a slanted line and the inversion is
    much faster.} 
  \item{bot}{an optional array (length = \eqn{n}) of depth to layer
    interfaces for the starting model. If not provided, then the initial
    model is a single layer extending from the surface to the depth of
    the last measurement.}
  \item{auto}{logical value for if the depths to boundaries should be
    autopicked for initial model. Default is TRUE}
  \item{cex}{see \code{par} function in R package \sQuote{graphics}.}
  \item{nticks}{approximate number of tick marks desired for depth-axis
    on travel-time plots. See \code{pretty} function in R package
    \sQuote{base}.}
  \item{grid}{logical value for plotting grid lines on travel-time
    plots.}
  \item{depth}{layout of plots. Value can be \sQuote{v} for vertical or
    \sQuote{h} for horizontal orientation of the depth-axis on travel-time plots.}
  \item{profile}{defaults to \sQuote{slow} for plotting the slowness profile
    with the travel-times and residuals. Any other value, i.e. \sQuote{vel}
    will plot the velocity profile instead.}

}
\value{a list containing model info and original data. The specific
  elements of the list are:
  \item{A}{ \eqn{A = \sum_{i=1}^N(n_i h_i s_i)}{A = sum(n_i * h_i * s_i)}}
  \item{s}{array containing the slowness of each layer (length = \eqn{n}).}
  \item{v}{array containing the velocity of each layer (length = \eqn{n}).}
  \item{n}{integer number of layers in the velocity model.}
  \item{k}{number of travel-time measurements.}
  \item{h}{array of thicknesses of each layer (length = \eqn{n}).}
  \item{bot}{array of depth to bottoms of each layer (length = \eqn{n}).}
  \item{tt}{array of observed travel-time arrivals (length = \eqn{k}).}
  \item{z}{array of depths of each travel-time measurement, in meters
    (length = \eqn{k}).}
  \item{hoffset}{horizontal offset at the surface from the borehole.}
  \item{N}{\eqn{k} by \eqn{n} matrix of transits.}
  \item{theta}{\eqn{k} by \eqn{n} matrix of angles of incidence when
    \code{snell == TRUE}. Otherwise defaults to 0.}
  \item{LM.tt}{array of predicted travel-times (length = \eqn{k}).}
  \item{wt}{array of weights used in the regression  (length = \eqn{k}).
    Defaults to 1 if sig is unspecified. Calculated as \eqn{1/sig^2}}
  \item{sig}{array of the standard deviation of the travel-time
      measurements normalized to the standard deviation of the best
      pick (length = \eqn{k}).}
  \item{se}{array of standard error of slowness for each layer (length =
    \eqn{n}). Used to calculate the upper and lower bounds in the
    velocity model.} 
  \item{sigma.hat}{
    \eqn{\hat{\sigma}=\sqrt{\sum(res^2 wt)/(k - n)} }{sqrt(sum(res^2*wt)/(k-n))}
  }
  \item{v.upper}{array of layer velocities corresponding to \eqn{s_i -
      se_i} (length = \eqn{n}).}
  \item{v.lower}{array of layer velocities corresponding to \eqn{s_i +
      se_i} (length = \eqn{n}).}
  \item{tt.slant}{array of observed travel-time measurements (length =
    \eqn{k}).}
  \item{snell}{logical value for if the Snell's Law raypaths should be
    used. If FALSE, then raypaths are assumed to be straight lines from
    source to receiver.}
  
}
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
# Load table of Dave Boore's downhole seismic data:
data(tt.s) # S-wave arrivals

# Select hole code 293:
f <- tt.s$hole.code == 293
example <- tt.s[f, ]
data <- list(tt.slant = example$tt.slant,
             hoffset = example$hoffset[1],
             z = example$z,
             sig = example$sig,
             hole.code = 293)

# If Using the Windows GUI, it is best to uncheck the 'Buffered output'
# option in the Misc pull-down menu. This will allow you to see the
# outputs to the console as they occur rather than all at once at the
# end.

# The command to calculate the layered model is:
mod1 <- Rvelslant(data)

# The default is to use depth on the vertical axis. However, if you
# prefer depth to be displayed on the horizontal axis, use:

mod1 <- Rvelslant(data, depth = "h")

  #####################################################
  #    Notes:
  # You can select layer boundaries by clicking on the
  # travel-time or residual plots. You can remove a
  # boundary by left clicking on it in the velocity
  # profile plot.
  
  # Right-click anywhere once your have found a model
  # that you want to save.
  #  -> In WINDOWS must select 'stop' after right-click
  #  -> In MAC OS, can use ESC key instead if you don't
  #     have a second mouse button.
  
  # The layered model depth to bottom, thickness, and
  # velocity will be printed to the terminal.
  
  # The default is to calculate the refracted ray path
  # according to Snell's law and iteratively update
  # the velocity mode. To override this, and do the
  # inversion assuming the raypaths are straight lines
  # from source to receiver, use:

mod2 <- Rvelslant(data, snell = FALSE, ntick = 10, grid = TRUE)

# To use the auto picker:
mod3 <- Rvelslant(data, auto = TRUE)

# To manually enter layer interfaces:
mod4 <- Rvelslant(data, bot = c(6.2, 9.5, 14.5, 29.5,
                    47, 54.5, 74.5, 89.5)) 

# To view the profiles, type:

velprofile(mod1)
velprofile(mod2, col = "green")

# If you have more than one model and you want to compare the profiles:

velprofile(mod1, col = "black")
velprofile(mod2, add = TRUE, col = "red")
velprofile(mod3, add = TRUE, col = "blue")

# To view the fit of travel-times and the residuals:
par(mfrow = c(1, 2))
plotmod(mod1)
plotresid(mod1)

# To save the model to a file
writemod(mod1, prefix = "hc293")

}
\keyword{}
