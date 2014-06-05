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
#' @param \dots additional arguments passed to \code{\link[lattice]{barchart}}
#' @param origin beginning point for bars.  For the default behavior used by 
#'        \code{\link[lattice]{barchart}} set \code{origin} to \code{NULL}, but
#'         0 is often a better default. If 0 is not good, perhaps you should use
#'        a different kind of plot as the results may be misleading.
#' @param subset a vector used to subset \code{data}.  This may be an expression that
#' will be evaluated within \code{data}.
#' @param ylab a character vector of length one used for the y-axis label
#' @param xlab a character vector of length one used for the x-axis label
#' @return a trellis object describing the plot
#' @seealso \code{\link[lattice]{barchart}}
#' @details \code{bargraph(formula, data=data, ...)} works by creating a new data frame from \code{xtabs(formula, data=data)}
#' and then calling \code{\link[lattice]{barchart}} using modified version of the formula and this
#' new data frame as inputs.  This has implications on, for example, conditional plots where 
#' one desires to condition on some expression that will be evaluated in \code{data}.  This typically
#' does not work becuase the required variables do not exist in the output of \code{xtabs}.  One solution
#' is to first add a new variable to \code{data} first and then to condition using this new variable.
#' See the examples.
#'
#' @export
#' @examples
#' data(HELPrct)
#' bargraph( ~ substance, data=HELPrct)
#' bargraph( ~ substance, data=HELPrct, horizontal=TRUE)
#' bargraph( ~ substance | sex, groups=homeless, auto.key=TRUE, data=HELPrct)
#' bargraph( ~ substance, groups=homeless, auto.key=TRUE, data=HELPrct, subset=sex=="male")
#' HELPrct2 <- transform( HELPrct, older = age > 40 )
#' bargraph( ~ substance | older, data = HELPrct2 )

bargraph <- function(x, data=parent.frame(), groups, horizontal=FALSE, origin=0, 
                     ylab=ifelse(horizontal,"","Frequency"), 
                     xlab=ifelse(horizontal,"Frequency",""), 
                     subset,
                     ...) {
  haveGroups <- !missing(groups)
  sgroups <- substitute(groups)
  
  # if ( .is.formula(x) ) formula <- x else formula <- paste("~", deparse(rhs(x)))
  formula <- paste("~", deparse(rhs(x)))
  if (!is.null (condition(x)) ) formula <- paste(formula, "+" , deparse(condition(x)) )
  if (haveGroups ) formula <- paste(formula, "+" , sgroups )
  formula <- as.formula(formula)
  
  if (missing(subset)) {
    xtab <- as.data.frame(xtabs( formula, data=data) )
  } else {
    xtab <- as.data.frame(
      do.call(xtabs, list( formula, data=data, subset=substitute(subset) ) )
      )
  }
  # grab the last variable name, to handle the case that 
  # there is a variable called "Freq"
  lastvar = names(xtab)[ncol(xtab)]
  if (horizontal) {
	  if (! is.null(condition(x))){
		formula <- as.formula( paste(deparse(rhs(x)), " ~ ", lastvar, " | ", deparse(condition(x)) ) )
	  } else {
		formula <- as.formula( paste(deparse(rhs(x)), " ~ ", lastvar) )
	  }
  } else {
	  if (! is.null(condition(x))){
		formula <- as.formula( paste(lastvar, " ~", deparse(rhs(x)), "|", deparse(condition(x)) ) )
	  } else {
		formula <- as.formula( paste(lastvar, " ~", deparse(rhs(x))) )
	  }
  }
  if (haveGroups)
    barchart( formula, data=xtab, groups=eval(sgroups), origin=origin, ylab=ylab, xlab=xlab, ... ) 
  else
    barchart( formula, data=xtab, origin=origin, ylab=ylab, xlab=xlab, ... )   
}
