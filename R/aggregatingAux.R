#' Convert formulas into standard shapes
#' 
#' These functions convert formauls into standard shapes, including by 
#' incorporating a groups argument.  
#' @rdname mosaicformula
#' @param formula a formula
#' @param groups a name used for grouping
#' @param max.slots an integer specifying the maximum number of slots for the resulting formula.  
#' An error results from trying to create a formula that is too complex.
#' @param envir the environment in which the resulting formula may be evaluated. 
#' May also be \code{NULL}, a list, a data frame, or a pairlist.
#' 
#' @details
#' \code{mosaic_formula_q} uses nonstandard evaluation of \code{groups} that may be
#' necessary for use within other functions.  \code{mosaic_formula} is a wrapper
#' around \code{mosaic_formula_q} and quotes \code{groups} before passing it along. 
#' @export
#' @examples
#' mosaic_formula( ~ x | z )
#' mosaic_formula( ~ x, groups=g ) 
#' mosaic_formula( y ~ x, groups=g )
#' # this is probably not what you want for interactive use.
#' mosaic_formula_q( y ~ x, groups=g )
#' # but it is for programming
#' foo <- function(x, groups=NULL) {
#'     mosaic_formula_q(x, groups=groups, envir=parent.frame())
#' }
#' foo( y ~ x , groups = g)

mosaic_formula <- function( 
  formula, 
  groups=NULL, 
  envir=parent.frame(),
  max.slots=3
  ) 
{
  mosaic_formula_q( 
    formula=formula,
    groups=quote(groups),
    envir=envir,
    max.slots=max.slots)
}

#' @rdname mosaicformula
#' @export
mosaic_formula_q <- function( formula, 
                                groups=NULL, 
                                envir=parent.frame(), 
                                max.slots = 3
                                ) {
  
  slots <- list()
  slots <- c(slots, lhs(formula), rhs(formula), condition(formula), substitute(groups, parent.frame()))
  
  if (length(slots) > max.slots) {
    print(slots)
    stop("Invalid formula specification.  Too many slots (",  
            length(slots), ">", max.slots, ").")
    return(NULL)
  }

  if (length(slots) == 1) {
    res <- ~ x
    res[[2]] <- slots[[1]]
  } else if (length(slots)==2) {
    res <- y ~ x 
    res[[2]] <- slots[[1]]
    res[[3]] <- slots[[2]]
  } else if (length(slots)==3) {
    res <- y ~ x | z
    res[[2]] <- slots[[1]]
    res[[3]][[2]] <- slots[[2]]
    res[[3]][[3]] <- slots[[3]]
  } else {
    res <- formula
  }
  environment(res) <- envir
  return(res)
}

.fetchFromDots <- function( dots, name, class='data.frame', n=1, default=NULL ) {
  result <- dots[[name]]
  if (is.null(result)) {
    if (length(result) < n) return(default)
    result <- dots[[n]]
    if (! inherits(result, 'class') ) result <- default
  }
  return(result)
}

#' Check if formula
#' 
#' @param x an object
#' @return TRUE for a formula, FALSE otherwise, even if evaluation throws an error
#'
#' @rdname mosaic-internal
#' @keywords internal

.is.formula <- function(x)  
  tryCatch( inherits(x, 'formula'), error = function(e) {FALSE} )

#' Check for simple formula
#'
#' @param x a formula
#'
#' @return TRUE if formula has no left-hand side or a simple right-hand side 
#' (e.g., \code{NULL}, ., 1,  or 0)
#'
#' @rdname mosaic-internal
#' @keywords internal
.is.simple.formula <-  function(x){
  inherits(x, "formula") &&
    (length(x)==2 || is.null(x[[3]]) ||
       (length(x[[3]])==1 &&
          ((is.numeric(x[[3]]) && (x[[3]]==0 || x[[3]]==1)) ||  (all.names(x[[3]]) %in% c(".")))))
}

# This could use a better name and a better desription

#' Extract simple part from formula
#'
#' @param x a formula
#'
#' @return simple part of formula or NULL if formula is not simple
#'
#' @rdname mosaic-internal
#' @keywords internal

.simple.part <- function(x) {
  if (! .is.simple.formula(x) ) {
    return(NULL) 
  } else {
    return(x[[2]])
  }
}

.flatten <- function(x) {
  result <- c()
  for (item in x) result <- c(result, item)
  return(result)
}


#' Aggregate for mosaic
#'
#' Compute function on subsets of a variable in a data frame.
#'
#' @rdname aggregatingAux
#' @return  a vector
#' @param formula a formula.  Left side provides variable to be summarized.  Right side and condition
#'                            describe subsets.  If the left side is empty, right side and condition are
#'                            shifted over as a convenience.
#' @param data a data frame
#' @param FUN a function to apply to each subset 
#' @param subset a logical indicating a subset of \code{data} to be processed.
#' @param drop a logical indicating whether unused levels should be dropped.
#' @param format,overall currently unused
#' @param groups grouping variable that will be folded into the formula (if there is room for it).  
#' This offers some additional flexibility in how formulas can be specified.
#' @param multiple logical indicating whether FUN returns multiple values
#' @param \dots additional arguments passed to \code{FUN}
#'
#' @export
#' @examples
#' maggregate( cesd ~ sex, HELPrct, FUN=mean )
#' # using groups instead
#' maggregate( ~ cesd, groups = sex, HELPrct, FUN=sd )
#' # the next four all do the same thing
#' maggregate( cesd ~ sex & homeless, HELPrct, FUN=mean )
#' maggregate( cesd ~ sex | homeless, HELPrct, FUN=sd )
#' maggregate( ~ cesd | sex , groups= homeless, HELPrct, FUN=sd )
#' maggregate( cesd ~ sex, groups = homeless, HELPrct, FUN=sd )
#' # this is unusual, but also works.
#' maggregate( cesd ~ NULL , groups = sex, HELPrct, FUN=sd )
#'
maggregate <- function(formula, data=parent.frame(), FUN, subset, 
                       overall=mosaic.par.get("aggregate.overall"), 
                       format=c('default'), drop=FALSE, multiple=FALSE, groups=NULL, ...) {
  dots <- list(...)
  formula <- mosaic_formula_q(formula, groups=groups, as.environment(data))

  format <- match.arg(format)
  evalF <- evalFormula(formula, data=data)
  
  if (!missing(subset)) {
    subset <- eval(substitute(subset), data, environment(formula))
    if (!is.null(evalF$left))           evalF$left <- evalF$left[subset,]
    if (!is.null(evalF$right))         evalF$right <- evalF$right[subset,]
    if (!is.null(evalF$condition)) evalF$condition <- evalF$condition[subset,]
  }
  
  if ( is.null( evalF$left ) ) {
    evalF$left <- evalF$right
    evalF$right <- evalF$condition
    evalF$condition <- NULL
  }
  
  #if ( ! is.null(evalF$condition) ) stop('Conditioning not allowed in this type of formula.')
  
  if ( is.null(evalF$right) || ncol(evalF$right) < 1 )  {
    if (ncol(evalF$left) > 1) message("Too many variables; ignoring all but first.")
    return( do.call(FUN, c(list(evalF$left[,1]), ...) ) )
  } else {
    if (ncol(evalF$left) > 1) message("Too many variables; ignoring all but first.")
    res <- lapply( split( evalF$left[,1], joinFrames(evalF$right, evalF$condition), drop=drop),
                   function(x) { do.call(FUN, c(list(x), ...) ) }
    )
  }
  if (! multiple ) res <- unlist(res)
  
  if (! is.null(evalF$condition) ) {
    if (ncol(evalF$left) > 1) message("Too many variables; ignoring all but first.")
    res2 <- lapply( split( evalF$left[,1], evalF$condition, drop=drop),
                    function(x) { do.call(FUN, c(list(x), ...) ) }
    )
    if (!multiple) {
      res <- c( res , unlist(res2) )
    } else {
      res <- c(res, res2)
    }
  }
  if (multiple) {
    result <- res
    res <- result[[1]]
    for (item in result[-1]) {
      res <- rbind(res,item)
    }
    rownames(res) <- names(result)
  }
  return( res )
}

