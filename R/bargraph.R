#' Create bar graphs from raw data
#'
#' \code{\link[lattice]{barchart}} from the \code{lattice} package makes bar graphs from
#' pre-tabulated data.  Raw data can be tabulated using \code{\link{xtabs}}, but the syntax
#' is unusual compared to the other lattice plotting functions.  \code{bargraph} provides 
#' an interface that is consistent with the other \code{lattice} functions.
#'
#' @param x a formula describing the plot
#' @param data a data frame in which the formula \code{x} is evaluated
#' @param groups a variable or expression used for grouping.  See \code{\link[lattice]{barchart}}.
#' @param horizontal a logical indicating whether bars should be horizontal
#' @param \dots additional arguments passed through to \code{\link[lattice]{barchart}}
#' @param origin beginning point for bars.  For the default behavior used by 
#'        \code{\link[lattice]{barchart}} set \code{origin} to \code{NULL}, but
#'         0 is a better default, and if 0 is not good, perhaps you should use
#'        a different kind of plot anyway.
#' @return a trellis object describing the plot
#' @seealso \code{link[lattice]{barchart}}
#'
#' @note
#' The current implementation may not work if one of the variables used in the plot
#' is called \code{Freq}, since that variable is created by an underlying call to 
#' \code{\link{xtabs}}.
#'
#' @examples
#' bargraph( ~ substance, data=HELPrct)
#' bargraph( ~ substance, data=HELPrct, horizontal=TRUE)
#' bargraph( ~ substance | sex, groups=homeless, data=HELPrct)

bargraph <- function(x, data, groups, horizontal=FALSE, origin=0, ...) {
  sgroups <- substitute(groups)
  haveGroups <- !missing(groups)
  formula <- paste("~", deparse(rhs(x)))
  if (!is.null (condition(x)) ) formula <- paste(formula, "+" , deparse(condition(x)) )
  if (haveGroups ) formula <- paste(formula, "+" , sgroups )
  formula <- as.formula(formula)
  xtab <- as.data.frame(xtabs( formula, data=data))
  if (horizontal) {
	  if (! is.null(condition(x))){
		formula <- as.formula( paste(deparse(rhs(x)), " ~ Freq | ", deparse(condition(x)) ) )
	  } else {
		formula <- as.formula( paste(deparse(rhs(x)), " ~ Freq") )
	  }
  } else {
	  if (! is.null(condition(x))){
		formula <- as.formula( paste("Freq ~", deparse(rhs(x)), "|", deparse(condition(x)) ) )
	  } else {
		formula <- as.formula( paste("Freq ~", deparse(rhs(x))) )
	  }
  }
  if (haveGroups)
    barchart( formula, data=xtab, groups=eval(sgroups), origin=origin, ... ) 
  else
    barchart( formula, data=xtab, origin=origin, ... )   
}
