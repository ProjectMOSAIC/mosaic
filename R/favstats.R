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
#' fav_stats(1:10)
#' fav_stats(faithful$eruptions)
#' 
#' @keywords stats


setGeneric( 
	"fav_stats", 
	function (x, ..., na.rm = TRUE) {
		standardGeneric('fav_stats')
	}
)


#' @rdname fav_stats
#' @aliases fav_stats,matrix,ANY-method
#' @export
#' @usage
#' \S4method{fav_stats}{matrix,ANY}(x, ..., na.rm = TRUE) 
setMethod(
		  'fav_stats',
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


#' @rdname fav_stats
#' @aliases fav_stats,numeric,ANY-method
#' @export
#' @usage
#' \S4method{fav_stats}{numeric,ANY}(x, ..., na.rm = TRUE) 
setMethod(
		  'fav_stats',
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


#' @rdname fav_stats
#' @aliases fav_stats,formula,ANY-method
#' @export
#' @usage
#' \S4method{fav_stats}{formula,ANY}(x, data=parent.frame(), ..., na.rm = TRUE) 
setMethod(
		  'fav_stats',
		  c('formula'),
		  function (x, data=parent.frame(), ..., na.rm = TRUE) 
		  {
			  formula <- x 
			  return(maggregate(formula, data=data, FUN=fav_stats, multiple=TRUE))
		  }
)

#' @rdname fav_stats
#' @aliases fav_stats,ANY,data.frame-method
#' @export
#' @usage
#' \S4method{fav_stats}{ANY,data.frame}(x, data=parent.frame(), ..., na.rm = TRUE) 
setMethod(
		  'fav_stats',
		  c('ANY', 'data.frame'),
		  function (x, data=parent.frame(), ..., na.rm = TRUE) 
		  {
			  x <- eval(x, data) 
			  return(fav_stats(x))
		  }
)
