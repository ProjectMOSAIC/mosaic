#' Some favorite statistical summaries
#' 
#' Computes mean, standard deviation, quartiles, sample size and number of missing values for a data vector.
#' @param x  numeric vector 
#' @param na.rm  boolean indicating whether missing data should be ignored 
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
#' @aliases favstats,numeric-method
#' @export
setMethod(
		  'favstats',
		  c(x='numeric'),
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
#' @aliases favstats,formula-method
#' @export
setMethod(
		  'favstats',
		  c(x='formula'),
		  function (x, data, ..., na.rm = TRUE) 
		  {
			  formula <- x 
			  return(maggregate(formula, data=data, FUN=favstats, multiple=TRUE))
		  }
)
