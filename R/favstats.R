#' Some favorite statistical summaries
#' 
#' Computes mean, standard deviation, quartiles, sample size and number of missing values for a data vector.
#' @param x  numeric vector 
#' @param data a data frame
#' @param na.rm  boolean indicating whether missing data should be ignored 
#' @param ... additional arguments (currently ignored)
#' 
#' 
#' @return A vector of statistical summaries
#' @export
#' @examples
#' favstats(1:10)
#' favstats(faithful$eruptions)
#' 
#' @keywords stats


setGeneric( 
	"favstats", 
	function (x, ..., na.rm = TRUE) {
		standardGeneric('favstats')
	}
)


#' @rdname favstats
#' @aliases favstats,matrix,ANY-method
#' @export
#' @usage
#' \S4method{favstats}{matrix,ANY}(x, ..., na.rm = TRUE) 
setMethod(
		  'favstats',
		  'matrix',
		  function (x, ..., na.rm = TRUE) 
		  {
			  if (min(dim(x)) != 1) warning("Not respecting matrix dimensions.  Hope that's OK.")
			  x <- as.vector(x)
			  qq <- quantile(x, na.rm = na.rm)
			  val <- data.frame(qq[1],  qq[2], qq[3], qq[4], qq[5],
								mean(x, na.rm = na.rm), 
								sd(x, na.rm = na.rm), 
								sum(! is.na(x)),
								sum( is.na(x) )
								)
			  rownames(val) <- ""
			  names(val) <- c("min", "Q1", "median", "Q3", "max", "mean", "sd", "n", "missing")
			  return(val)
		  }
)


#' @rdname favstats
#' @aliases favstats,numeric,ANY-method
#' @export
#' @usage
#' \S4method{favstats}{numeric,ANY}(x, ..., na.rm = TRUE) 
setMethod(
		  'favstats',
		  'numeric',
		  function (x, ..., na.rm = TRUE) 
		  {
			  qq <- quantile(x, na.rm = na.rm)
			  val <- data.frame(qq[1],  qq[2], qq[3], qq[4], qq[5],
								mean(x, na.rm = na.rm), 
								sd(x, na.rm = na.rm), 
								sum(! is.na(x)),
								sum( is.na(x) )
								)
			  rownames(val) <- ""
			  names(val) <- c("min", "Q1", "median", "Q3", "max", "mean", "sd", "n", "missing")
			  return(val)
		  }
)


#' @rdname favstats
#' @aliases favstats,formula,ANY-method
#' @export
#' @usage
#' \S4method{favstats}{formula,ANY}(x, data=parent.frame(), ..., na.rm = TRUE) 
setMethod(
		  'favstats',
		  c('formula'),
		  function (x, data=parent.frame(), ..., na.rm = TRUE) 
		  {
			  formula <- x 
			  return(maggregate(formula, data=data, FUN=favstats, multiple=TRUE))
		  }
)

#' @rdname favstats
#' @aliases favstats,ANY,data.frame-method
#' @export
#' @usage
#' \S4method{favstats}{ANY,data.frame}(x, data=parent.frame(), ..., na.rm = TRUE) 
setMethod(
		  'favstats',
		  c('ANY', 'data.frame'),
		  function (x, data=parent.frame(), ..., na.rm = TRUE) 
		  {
			  x <- eval(x, data) 
			  return(favstats(x))
		  }
)
