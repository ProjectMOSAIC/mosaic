#' Some favorite statistical summaries
#' 
#' Computes mean, standard deviation, quartiles, sample size and number of missing values for a numeric vector.
#' @param x  numeric vector 
#' @param na.rm  boolean indicating whether missing data should be ignored 
#' @param ... additional arguments (currently ignored)
#' 
#' @return A vector of statistical summaries
#' @keywords stats
#' @export
#' @examples
#' fav_stats(1:10)
#' fav_stats(faithful$eruptions)
#' 

fav_stats <- function (x, ..., na.rm = TRUE) 
{
  if (!is.null(dim(x)) && min(dim(x)) != 1) 
    warning("Not respecting matrix dimensions.  Hope that's OK.")
  x <- as.vector(x)
  qq <- quantile(x, na.rm = na.rm)
  val <- data.frame(qq[1],  qq[2], qq[3], qq[4], qq[5],
                    base::mean(x, na.rm = na.rm), 
                    stats::sd(x, na.rm = na.rm), 
                    base::sum(! is.na(x)),
                    base::sum( is.na(x) )
  )
  rownames(val) <- ""
  names(val) <- c("min", "Q1", "median", "Q3", "max", "mean", "sd", "n", "missing")
  return(val)
}

