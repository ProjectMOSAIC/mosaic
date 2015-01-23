#' Conditionally convert vectors to factors
#' 
#' A generic function and several instances for creating factors from
#' other sorts of data.  The primary use case is for vectors that contain
#' few unique values and might be better considered as factors.  When
#' applied to a data frame, this is applied to each variable in the data frame.
#' 
#' @param x an object
#' @param max.levels an integer.  Only convert if the number of unique values is no 
#' more than \code{max.levels}.
#' @param ... additional arguments (currently ignored)
#' 
#' @export
#' @examples
#' data(KidsFeet, package="mosaicData")
#' str(KidsFeet)
#' factorize(KidsFeet$birthyear)
#' str(factorize(KidsFeet))
 
factorize <- function(x,  ...) {
  UseMethod("factorize")
}

#' @rdname factorize
#' @export
factorize.default <- function(x, ...) {
  x
}

#' @rdname factorize
#' @export
factorize.numeric <- function(x, max.levels = 5L, ...){
  if (length(unique(x)) <=  max.levels) return ( factor(x, levels=sort(unique(x))) )  
  x
}

#' @rdname factorize
#' @export
factorize.character <- function(x, max.levels = 5L, ...){
  if (length(unique(x)) <=  max.levels) return ( factor(x, levels=sort(unique(x))) )  
  x
}

#' @rdname factorize
#' @export
factorize.data.frame <- function(x, max.levels=5L, ...) {
  as.data.frame( lapply(x, factorize, max.levels=max.levels) )
}
