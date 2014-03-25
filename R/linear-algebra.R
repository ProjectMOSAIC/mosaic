#' Functions for teaching linear algebra.
#'
#' These functions provide a formula based interface to the construction
#' of matrices from data and for fitting.  You can use them both for numerical vectors
#' and for functions of variables in data frames.
#' These functions are intended to support teaching basic linear algebra
#' with a particular connection to statistics.
#'
#' @rdname linear.algebra
#' @name linear.algebra
# @aliases mat singvals dot
#'
#' @param A a formula.  In \code{mat} and \code{singvals},
#' only the right-hand side is used.
#'
# @param x a numeric vector, formula, or matrix
#' 
#' @param u a numeric vector
#' 
#' @param data a data frame from which to pull out numerical values
#' for the variables in the formula
#'
#' @param \dots additional arguments (currently ignored)
#'
#' 
#' \code{mat} returns a model matrix
#' 
#' To demonstrate singularity, use \code{singvals}.
#' 
#' @return \code{mat} returns a matrix 
#' 
#' @seealso \code{\link{project}}
#'
# @usage mat(A,data=NULL)
#' @seealso \code{\link{linearModel}}, which returns a function.
#' @examples
#' a <- c(1,0,0); b <- c(1,2,3); c <- c(4,5,6); x <- rnorm(3)
#' # Formula interface
#' mat(~a+b)
#' mat(~a+b+1)
#' mat(~length+sex, data=KidsFeet)
#' singvals(~length*sex*width, data=KidsFeet)

mat <- function(A, data=parent.frame()) {
  if( class(A) != "formula" ) stop("Must provide a formula, e.g., ~ a or ~ a + b ")
  A <- update(A, ~-1+.) # kill off automatic Intercept term
  if( is.null(data) )
    M <- model.matrix( A )
  else
    M <- model.matrix( A, data=data ) 

  attr(M, "assign") <- NULL
  rownames(M) <- NULL

  return(M)
}

#
##################
#' @rdname linear.algebra
#' @return \code{singvals} gives singular values for each column in the model matrix
#' @export
singvals <- function(A, data=parent.frame()){
  M <- mat(A, data=data)
  # formulated to give one singular value for each column in A
	svs <- La.svd(M, nv=ncol(M), nu=ncol(M))$d;
  c( svs, rep(0, ncol(M) - length(svs)));
}
##################





