#' Convert between degrees and radians
#' 
#' Facilitates conversion between degrees and radians.
#' @rdname deg2rad
#' @return a numeric vector
#' @param x a numeric vector
#' @examples
#' deg2rad(180)
#' @export

deg2rad <- function(x) {
	x/180 * base::pi
}

#' @rdname deg2rad
#' @examples
#' rad2deg(2*pi)
#' @seealso \code{\link{latlon2xyz}}, \code{\link{googleMap}}, and \code{\link{rgeo}}.
#' @export

rad2deg <- function(x) {
	x / base::pi * 180
}

#' Convert back and forth between latitude/longitude and XYZ-space
#' 
#' @rdname latlon2xyz
#' @return a matrix each row of which describes the latitudes and longitudes
#' @param x,y,z numeric vectors
#' @examples
#' xyz2latlon(1, 1, 1)     # point may be on sphere of any radius
#' xyz2latlon(0, 0, 0)     # this produces a NaN for latitude
#' @export

xyz2latlon <- function(x,y,z) {
	# rescale to unit sphere
	R <- sqrt( x^2 + y^2 + z^2)
	x <- x/R; y <- y/R; z <- z/R;

	r <- sqrt( x^2 + y^2)
	lat <- rad2deg( asin(z) )

	long <- (r>0) * rad2deg( acos(x/r) ) 
	long <- ( 1 - 2 * (y < 0) ) * long
	long [ is.na(long) ] <- 0
	return( cbind(lat=lat, lon=long) )
}

#' @rdname latlon2xyz
#' @param latitude,longitude vectors of latitude and longitude values
#' @return a matrix each row of which contains the x, y, and z coordinates of a point on a unit sphere
#' 
#' @examples
#' latlon2xyz(30, 45)
#' lonlat2xyz(45, 30)
#' @seealso \code{\link{deg2rad}}, \code{\link{googleMap}}, and \code{\link{rgeo}}.
#' @export

latlon2xyz <- function(latitude,longitude) {
	z <- sin(deg2rad(latitude))
	r <- sqrt(1 - z^2)
	x <- rad2deg(cos( longitude ))
	y <- rad2deg(sin( longitude ))
	return(cbind( x=x, y=y, z=z ))
}

#' @rdname latlon2xyz
#' @export
longlat2xyz <- function(longitude, latitude) {
  latlong2xyz(latitude, longitude)
}

#' @rdname rgeo
#' @export
rlatlon <- function(...) { 
  rgeo(...)
}


#' @rdname rgeo
#' @export
rlonlat <- function(...){
  rgeo(...)[,c(2,1)]
}

#' Sample longitude and latitude on a sphere
#' 
#' Randomly samples longitude and latitude on earth so that equal areas are
#' (approximately) equally likely to be sampled.  
#' (Approximation assumes earth as a perfect sphere.)
#' 
#' @rdname rgeo
#' @param n number of random locations
#' 
#' @param latlim,lonlim  
#'   range of latitudes and longitudes to sample within, only implemented for \code{rgeo}.
#' 
#' @param verbose 
#'   return verbose output that includes Euclidean coordinates on unit sphere as well as 
#' longitude and latitude.
#'
#' @return a data frame with variables \code{long} and \code{lat}.  If \code{verbose} is
#' TRUE, then x, y, and z coordinates are also included in the data frame.
#' @examples
#' rgeo(4)
#' # sample from a region that contains the continental US
#' rgeo( 4, latlim=c(25,50), lonlim=c(-65,-125) )
#' @details
#' \code{rgeo} and \code{rgeo2} differ in the algorithms used to generate random positions.  
#' Each assumes a spherical globe.  \code{rgeo} uses that fact that each of the x, y and z
#' coordinates is uniformly distributed (but not independent of each other).  Furthermore, the 
#' angle about the z-axis is uniformly distributed and independent of z.  This provides 
#' a straightforward way to generate Euclidean coordinates using \code{runif}.  These are then
#' translated into latitude and longitude.
#' 
#' \code{rlatlon} is an alias for \code{rgeo} and 
#' \code{rlonlat} is too, expect that it reverses the 
#' order in which the lattitude and longitute values are 
#' returned.
#' 
#' \code{rgeo2} samples points in a cube by independently sampling each coordinate.  It then
#' discards any point outside the sphere contained in the cube and projects the non-discarded points
#' to the sphere.  This method must oversample to allow for the discarded points.
#' 
#' @seealso \code{\link{deg2rad}}, \code{\link{googleMap}} and \code{\link{latlon2xyz}}.
#' 
#' @keywords random 
#' @keywords geometry 
#' @keywords map 
#' @export

rgeo <- function( n=1, latlim=c(-90,90), lonlim=c(-180,180), verbose=FALSE ) {

	zlim <- sin(sort(deg2rad(latlim)))
	z <- runif( n,  zlim[1], zlim[2] )

	#beta <- runif( n, min(deg2rad(lonlim)), max(deg2rad(lonlim)) )
	blim <- deg2rad(sort(lonlim))
	beta <- runif( n, blim[1], blim[2] )

	r <- sqrt(1-z^2)
	x <- r * cos(beta)
	y <- r * sin(beta)

	# now convert this to latitude and longitude 
	latlon <- xyz2latlon(x,y,z)
	if (verbose) {
		return(data.frame(lat=latlon[,1], lon=latlon[,2], x=x, y=y, z=z))
	}
	return(data.frame(lat=latlon[,1], lon=latlon[,2]))
}

#' @rdname rgeo
#' @examples
#' rgeo2(4)
#' @export

rgeo2 <- function( n=1, latlim=c(-90,90), lonlim=c(-180,180), verbose=FALSE ) {

	# oversample pts in a cube

	m <- 10 + 3*n
	x <- runif(m,-1,1)
	y <- runif(m,-1,1)
	z <- runif(m,-1,1)

	# select pts inside unit sphere and project them onto surface

	r <- sqrt( x^2 + y^2 + z^2 )
	ids <- which( r < 1 ) [1:n]
	x <- x[ids]
	y <- y[ids]
	z <- z[ids]
	r <- r[ids]
	x <- x/r
	y <- y/r
	z <- z/r

	# now convert this to latitude and longitude 
	latlon <- xyz2latlon(x,y,z)
	if (verbose) {
		return(data.frame(lat=latlon[,1], lon=latlon[,2], x=x, y=y, z=z))
	}
	return(data.frame(lat=latlon[,1], lon=latlon[,2]))
}
#' Display a point on earth on a Google Map
#' 
#' Creates a URL for Google Maps for a particular latitude and
#' longitude position.

#' @rdname googleMap
#' @param latitude,longitude vectors of latitude and longitude values
#' @param position a data frame containing latitude and longitude positions
#' @param zoom zoom level for initial map (1-20)
#' @param maptype one of \code{'roadmap'}, \code{'satellite'}, \code{'terrain'}, and \code{'hybrid'}
#' @param mark a logical indicating whether the location should be marked with a pin
#' @param radius a vector of radii of circles centered at position that are displayed on the map
#' @param browse a logical indicating whether the URL should be browsed (else only returned as a string)
#' @param \dots additional arguments passed to \code{browseURL}
#' @return a string containing a URL.  Optionally, as a side-effect, the URL is visited in a browser
#' @examples
#' \dontrun{
#' googleMap(40.7566, -73.9863, radius=1)   # Times Square
#' googleMap(position=rgeo(2), radius=1)    # 2 random locations
#' }
#' @seealso \code{\link{deg2rad}}, \code{\link{latlon2xyz}} and \code{\link{rgeo}}.
#' @export

googleMap <- function(latitude, longitude, position=NULL,
	zoom=12, 
	maptype=c('roadmap','satellite','terrain','hybrid'),
	mark=FALSE,
	radius=0,
	browse=TRUE,
	...
	)
{
	urls <- .googleMapURL( 
				latitude=latitude, longitude=longitude,
				position=position, zoom=zoom, 
				maptype = maptype, mark=mark, radius=radius
			)

	if (browse) {
		return(invisible( sapply( urls, function(x) { browseURL(x,...) } ) ))
	} else {
		return(invisible(urls))
	}
}


#' rgeo internal functions
#' 
#' These are not really intended for public consumption.
#'
#' @name rgeo-internals
#' @rdname rgeo-internals
#' @return a URL as a string
#' @inheritParams googleMap
#' @keywords internal
#' 
.googleMapURL <- function(latitude, longitude, position=NULL,
	zoom=11, 
	maptype=c('roadmap','satellite','terrain','hybrid'),
	mark=FALSE,
	radius=0
	) 
{
	filename <- "googlemap3.html"

	if (FALSE) { # can't get browseURL to accept a file with parameters added on
		package <- "mosaic"
		paths <- .find.package(package, verbose = TRUE)
		paths <- paths[file_test("-d", file.path(paths, "google"))]
		paths <- file.path(paths, "google")
		paths <- paths[file_test("-f", file.path(paths, filename))]
		url <- file.path(paths, filename)
		url <- paste("file://",url,sep="")
	}
	url <- paste('http://mosaic-web.org/go/',filename,sep="")
	if (is.null(position)) {
		position <- data.frame(lat=latitude,lon=longitude)
	}
	latitude  <- position[,1]
	longitude <- position[,2]
	maptype <- match.arg(maptype)
	center <- paste(latitude,",",longitude,sep="")
	markString <- ""
	if (mark == TRUE) { markString <- paste('&mlat=',round(latitude,6),'&mlon=',round(longitude,6) ,sep="") } 

	return(invisible(paste(
		url,
		'?lat=', round(latitude,6),
		'&lon=', round(longitude,6),
		markString,
		'&zoom=', zoom,
		'&radius=', paste(as.character(radius),collapse=","),
		sep="")))
}

#' @rdname rgeo-internals
#' @param width,height width and height of window containing google map
#' @keywords internal
#'
.googleMapURL2 <- function(latitude, longitude, position=NULL,
	zoom=12, 
	width=600, 
	height=400, 
	maptype=c('roadmap','satellite','terrain','hybrid'),
	mark=FALSE
	) 
{
	latitude  <- position[,1]
	longitude <- position[,2]
	url <- "http://maps.google.com/maps/api/staticmap?"
	maptype <- match.arg(maptype)
	center <- paste(latitude,",",longitude,sep="")
	size <- paste(width,'x',height,sep="")
	markString <- ""
	if (mark == TRUE) { markString <- paste('&markers=size:tiny|', center,sep="") } 

	return(invisible(paste(
		url,
		'center=', center,
		markString,
		'&zoom=', zoom,
		'&size=', size,
		'&sensor=false', 
		'&maptype=', maptype,
		sep="")))
}
