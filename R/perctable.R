#' Cross tabulation displayed as percents or proportions
#' 
#' \code{perctable} and \code{proptable} use the cross-classifying factors to build a 
#' contingency table of the percents or proportions at each combination of factor levels.
#' @param \dots  
#'   arguments passed directly to \code{\link{table}};  typically
#'   one or more objects which can be interpreted as factors (including character strings), 
#'   or a list (or data frame) whose components can be so interpreted. 
#' 
#' @details See \code{\link{table}}.
#' @return a contingency table, an object of class "table", an array of percentage or proportion
#' values. Note that unlike S the result is always an array, a 1D array if one factor is given.
#' @author Randall Pruim (\email{rpruim@@calvin.edu})
#' @export
#' @examples
#' perctable(rbinom(1000,10,.5))
#' with(airquality,
#'    perctable(OzHi=Ozone > 80, Month, useNA="ifany"))
#' with(airquality,
#'    perctable(OzHi=Ozone > 80, Month, useNA="always"))
#' 
#' @keywords manipulate 

perctable <- function(...) 
{
	t <- table(...)
	t/sum(t) * 100
}

#' @rdname perctable
proptable <- function(...) 
{
	t <- table(...)
	t/sum(t) 
}

