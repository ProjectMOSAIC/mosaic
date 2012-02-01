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
#' @param \dots additional arguments passed through to \code{\link[lattice]{barchart}}
#' @return a trellis object describing the plot
#' @seealso \code{link[lattice]{barchart}}
#'
#' @examples
#' bargraph( ~ substance, data=HELPrct)
#' bargraph( ~ substance | sex, groups=homeless, data=HELPrct)

bargraph <- function(x, data, groups, ...) {
  sgroups <- substitute(groups)
  formula <- paste("~", deparse(rhs(x)))
  if (!is.null (condition(x)) ) formula <- paste(formula, "+" , deparse(condition(x)) )
  if (!missing(groups) ) formula <- paste(formula, "+" , sgroups )
  formula <- as.formula(formula)
  xtab <- as.data.frame(xtabs( formula, data=data))
  if (! is.null(condition(x))){
    formula <- as.formula( paste("Freq ~", deparse(rhs(x)), "|", deparse(condition(x)) ) )
  } else {
    formula <- as.formula( paste("Freq ~", deparse(rhs(x))) )
  }
  barchart( formula, data=xtab, groups=eval(sgroups), ... )
}
