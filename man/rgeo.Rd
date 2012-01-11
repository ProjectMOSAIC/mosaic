\name{deg2rad}
\alias{.googleMapURL}
\alias{.googleMapURL2}
\alias{deg2rad}
\alias{googleMap}
\alias{latlon2xyz}
\alias{rad2deg}
\alias{rgeo}
\alias{rgeo2}
\alias{xyz2latlon}
\title{Sample longitude and latitude on a sphere}
\usage{
  deg2rad(x)

  rad2deg(x)

  xyz2latlon(x, y, z)

  latlon2xyz(latitude, longitude)

  rgeo(n = 1, latlim = c(-90, 90), lonlim = c(-180, 180),
    verbose = FALSE)

  rgeo2(n = 1, latlim = c(-90, 90), lonlim = c(-180, 180),
    verbose = FALSE)

  googleMap(latitude, longitude, position = NULL,
    zoom = 12,
    maptype = c("roadmap", "satellite", "terrain", "hybrid"),
    mark = FALSE, radius = 0, browse = TRUE, ...)

  .googleMapURL(latitude, longitude, position = NULL,
    zoom = 11,
    maptype = c("roadmap", "satellite", "terrain", "hybrid"),
    mark = FALSE, radius = 0)

  .googleMapURL2(latitude, longitude, position = NULL,
    zoom = 12, width = 600, height = 400,
    maptype = c("roadmap", "satellite", "terrain", "hybrid"),
    mark = FALSE)
}
\arguments{
  \item{x}{a numeric vector}

  \item{y,z}{numeric vectors}

  \item{latitude,longitude}{vectors of latitude and
  longitude values}

  \item{n}{number of random locations}

  \item{latlim,lonlim}{range of latitudes and longitudes to
  sample within, only implemented for \code{rgeo}.}

  \item{verbose}{return verbose output that includes
  Euclidean coordinates on unit sphere as well as longitude
  and lattitude.}

  \item{position}{a data frame containing latitude and
  longitude positions}

  \item{zoom}{zoom level for initial map (1-20)}

  \item{maptype}{one of \code{'roadmap'},
  \code{'satellite'}, \code{'terrain'}, and
  \code{'hybrid'}}

  \item{mark}{a logical indicating whether the location
  should be marked with a pin}

  \item{radius}{a vector of radii of circles centered at
  position that are displayed on the map}

  \item{browse}{a logical indicating whether the URL should
  be browsed (else only returned as a string)}

  \item{\dots}{additional arguments passed to
  \code{browseURL}}

  \item{width,height}{width and height of window containing
  google map}
}
\value{
  a numeric vector

  a numeric vector

  a matrix each row of which contains a latitude and a
  longitude value

  a matrix each row of which contains the x, y, and z
  coordinates of a point on a unit sphere

  a data frame with variables \code{long} and \code{lat}.
  If \code{verbose} is TRUE, then x, y, and z coordinates
  are also included in the data frame.

  a data frame with variables \code{long} and \code{lat}.
  If \code{verbose} is TRUE, then x, y, and z coordinates
  are also included in the data frame.

  a string containing a URL.  Optionally, as a side-effect,
  the URL is visited in a browser

  a URL as a string
}
\description{
  Randomly samples longitude and latitude on earth so that
  equal areas are (approximately) equally likely to be
  sampled. (Approximation assumes earth as a perfect
  sphere.)
}
\details{
  Some additional utility functions are documented here as
  well.

  \code{rgeo} and \code{rgeo2} differ in the algorithms
  used to generate random positions. Each assumes a
  spherical globe.  \code{rgeo} uses that fact that each of
  the x, y and z coordinates is uniformly distributed (but
  not independent of each other).  Furthermore, the angle
  about the z-axis is uniformly distributed and independent
  of z.  This provides a straightforward way to generate
  Euclidean coordinates using \code{runif}.  These are then
  translated into latitude and longitude.

  \code{rgeo2} samples points in a cube by independently
  sampling each coordinate.  It then discards any point
  outside the sphere contained in the cube and projects the
  non-discarded points to the sphere.  This method must
  oversample to allow for the discarded points.
}
\examples{
deg2rad(180)
rad2deg(2*pi)
xyz2latlon(1,1,1)     # point may be on sphere of any radius
xyz2latlon(0,0,0)     # this produces a NaN for latitude
latlon2xyz(45,45)
rgeo(4)
# sample from a region that contains the continental US
rgeo( 4, latlim=c(25,50), lonlim=c(-65,-125) )
rgeo2(4)
}
\author{
  Randall Pruim (\email{rpruim@calvin.edu})
}
\seealso{
  \code{\link{googleMap}}
}
\keyword{geometry}
\keyword{internal}
\keyword{random}

