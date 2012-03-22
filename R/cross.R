#' Factor cross products
#' 
#' Construct a product of factors.
#'
#' @param \dots  factors to be crossed.
#' @param sep  separator between levels
#' @param drop.unused.levels should levels that do not appear in cross product be dropped?
#' 
#' @return a factor
#' 
#' @export
#' @examples
#' x <- letters[1:3]
#' y <- c(1,2,1,1,3,1,3)
#' cross(x, y)
#' cross(x, y, drop.unused.levels=TRUE)
#' 
#' @keywords manipulate 
#' 

cross <- function(..., sep=":", drop.unused.levels=FALSE) {
	factors <- list(...)
	factors <- lapply( factors, function(x) { as.factor(x) } )
	if ( length(factors) < 1 ) {
		stop('No factors specified.')
	}
	levelsList <- lapply(factors, levels)

	result <- factors[[1]]
	levels <- levels(result)
	factors[[1]] <- NULL
	while( length(factors) > 0 ) {
		levels <- as.vector( 
					outer (levels(factors[[1]]), levels, function(x,y) { paste(y,x,sep=sep) } ) 
					)
		if (drop.unused.levels ) {
			result <- factor( paste( result, factors[[1]], sep=sep))
		} else {
			result <- factor( paste( result, factors[[1]], sep=sep), levels=levels)
		}
		factors[[1]] <- NULL
	}
	return(result)
}
