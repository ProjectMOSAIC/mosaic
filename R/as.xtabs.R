#' Convert objects to xtabs format
#' 
#' Currently able to convert a data frame or a matrix into an \code{xtabs}
#' object.
#' 
#' The intended use it to convert a two-way contingency table stored in a data
#' frame or a matrix into an \code{xtabs} object.
#' 
#' @aliases as.xtabs as.xtabs.data.frame as.xtabs.matrix
#' @param x object (typically a data frame) to be converted to \code{xtabs}
#' format
#' @param ... additional arguments to be passed to or from methods.
#' @return An \code{xtabs} object.
#' @author Randall Pruim (\email{rpruim@@calvin.edu})
#' @keywords manip
#' @rdname as-xtabs
#' @export
#' @examples
#' # example from example(fisher.test)
#' df <- data.frame( X=c('Tea','Milk'), Tea=c(3,1), Milk=c(1,3) )
#' xt <- as.xtabs(df, rowvar="Guess", colvar="Truth"); xt
#' if (require(vcd)) { mosaic(xt) }

as.xtabs <- function(x, ...) { UseMethod('as.xtabs') }

#' @rdname as-xtabs
#' @method as.xtabs data.frame
#' @param rowvar name of the row variable as character string
#' @param colvar name of the column variable as character string
#' @param labels Column of data frame that contains the lables of the row
#' variable.
#' @export
as.xtabs.data.frame <- function(x, rowvar=NULL, colvar=NULL, labels=1, ...) {

  if (labels >= 1) {
  	cnames <- names(x)[-1]
  	m <- as.matrix(x[,-c(labels)])
  } else {
  	cnames <- names(x)
	m <- as.matrix(x)
  }

  rnames <- x[,labels]
  rownames(m) <- rnames
  if (! is.character(rowvar) ) { rowvar <- "variable.1" }
  if (! is.character(colvar) ) { colvar <- "variable.2" }
  
  dn <- list( rnames, cnames)
  names(dn) <- c(rowvar, colvar)
  attr(m,'dimnames') <- dn
  class(m) <- c('xtabs', 'table')
  return(m)
}

#' @rdname as-xtabs
#' @method as.xtabs matrix
#' @export
as.xtabs.matrix <- function(x, rowvar=NULL, colvar=NULL, ...)  {
  rnames <- rownames(x)
  cnames <- colnames(x)
  rownames(m) <- rnames
  if (! is.character(rowvar) ) { rowvar <- "variable.1" }
  if (! is.character(colvar) ) { colvar <- "variable.2" }
  
  dn <- list( rnames, cnames)
  names(dn) <- c(rowvar, colvar)
  attr(m,'dimnames') <- dn
  class(m) <- c('xtabs', 'table')
  return(m)
}
