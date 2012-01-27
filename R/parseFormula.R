#' Parse formulas
#'
#' utilities for exptracting portions of formulas.
#'
#' @rdname parseFormula
#' @param formula, a formula
#' @param \dots additional arguments, current ignored
#' @param x, an object (currently a \code{formula} or \code{parsedFormula})
#' @return an object of class \code{parsedFormula} from which information is easy to extract
#' @details
#' currently this is primarily concerned with extractly the operator, left hand side, right hand 
#' side (minus any condition) and the condition. Improvements/extensions may come in the future.
#' 
parse.formula <- function(formula, ...) {
  op <- formula[[1]]
  condition <- NULL
  if (length(formula) == 2) {
    rhs <- formula[[2]]
    lhs <- NULL
  } else if (length(formula) == 3) {
    rhs <- formula[[3]]
    lhs <- formula[[2]]
  } else {
    stop('Invalid formula type.')
  }
  
  if (inherits(rhs, "call") && rhs[[1]] == '|') {
    condition <- rhs[[3]]
    rhs <- rhs[[2]]
  }
  return( structure(list(op=op,lhs=lhs,rhs=rhs,condition=condition), class='parsedFormula') )
}


#' @rdname parseFormula
rhs <- function(x, ...) {
  UseMethod('rhs')
}

#' @rdname parseFormula
lhs <- function(x, ...) {
  UseMethod('lhs')
}

#' @rdname parseFormula
condition <- function(x, ...) {
  UseMethod('condition')
}

#' @rdname parseFormula
operator <- function(x, ...) {
  UseMethod('operator')
}

#' @rdname parseFormula
#' @method rhs formula
rhs.formula <- function(x,...) rhs( parse.formula(x, ...) )
#' @rdname parseFormula
#' @method lhs formula
lhs.formula <- function(x,...) lhs( parse.formula(x, ...) )
#' @rdname parseFormula
#' @method lhs formula
condition.formula <- function(x,...) condition( parse.formula(x, ...) )
#' @rdname parseFormula
#' @method lhs formula
operator.formula <- function(x,...) operator( parse.formula(x, ...) )

#' @rdname parseFormula
#' @method rhs parsedFormula
rhs.parsedFormula <- function(x,...) x$rhs
#' @rdname parseFormula
#' @method lhs parsedFormula
lhs.parsedFormula <- function(x,...) x$lhs
#' @rdname parseFormula
#' @method operator  parsedFormula
operator.parsedFormula <- function(x,...) x$operator
#' @rdname parseFormula
#' @method condition  parsedFormula
condition.parsedFormula <- function(x,...) x$condition

#listOfTerms <- function(x,...) {
#	if (is.null(x)) return (NULL)
#	if (length(x) <= 1) return (list(x))
#	return( c(listOfTerms(lhs(x)), listOfTerms(rhs(x))) )
#}


