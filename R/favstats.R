#' Some favorite statistical summaries
#' 
#' Likely you mean to be using \code{\link{favstats}}.  Each of these computes the 
#' mean, standard deviation, quartiles, sample size and number of missing values for a numeric vector,
#' but \code{\link{favstats}} can take a formula describing how these summary statistics 
#' should be aggregated across various subsets of the data.
#' @param x  numeric vector 
#' @param na.rm  boolean indicating whether missing data should be ignored 
#' @param type an integer between 1 and 9 selecting one of the nine quantile algorithms detailed
#'  in the documentation for \code{\link[stats]{quantile}}
#' @param ... additional arguments (currently ignored)
#' 
#' @return A vector of statistical summaries
#' @keywords stats
#' @examples
#' fav_stats(1:10)
#' fav_stats(faithful$eruptions)
#' favstats(Sepal.Length ~ Species, data=iris)  # Note: this is favstats() rather than fav_stats()
#' @export

fav_stats <- function (x, ..., na.rm = TRUE, type = 7) 
{
  if (!is.null(dim(x)) && min(dim(x)) != 1) 
    warning("Not respecting matrix dimensions.  Hope that's OK.")
  # x <- as.vector(x)
  if (! is.numeric(x)) {
    warning("Auto-converting ", class(x), " to numeric.")
    x <- as.numeric(x)
    if (!is.numeric(x)) stop("Auto-conversion to numeric failed.")
  }
  
  qq <- if (na.rm) 
    stats::quantile(x, na.rm = na.rm, type = type)
  else 
    rep(NA, 5)
  val <- data.frame(
    min=qq[1],  
    Q1 = qq[2], 
    median = qq[3], 
    Q3 = qq[4], 
    max = qq[5],
    iqr = stats::IQR(x, na.rm = na.rm, type =type),
    mean = base::mean(x, na.rm = na.rm), 
    sd = stats::sd(x, na.rm = na.rm), 
    n = base::sum(! is.na(x)),
    missing = base::sum( is.na(x) )
  )
  rownames(val) <- ""
#  names(val) <- c("min", "Q1", "median", "Q3", "max", "iqr", "mean", "sd", "n", "missing")
  return(val)
}

