#' Functions for teaching linear algebra.
#'
#' These functions provide a formula based interface to the construction
#' of matrices from data and for fitting.  You can use them both for numerical vectors
#' and for functions of variables in data frames.
#' @rdname linear.algebra
#'
#' @name linear.algebra
#' @aliases project mat singvals dot
#'
#' @param A a formula.  In \code{mat} and \code{singvals},
#' only the right-hand side is used.
#' In \code{project}, both sides are used, but the left-hand side should
#' be a single quantity
#'
#' @param x a numeric vector, formula, or matrix
#' 
#' @param u a numeric vector
#' 
#' @param data a data frame from which to pull out numerical values
#' for the variables in the formula
#'
#' @param \dots additional arguments (currently ignored)
#'
#' @details
#' These functions are intended to support teaching basic linear algebra
#' with a particular connection to statistics.
#' \code{project} (preferably pronounced "PRO-ject" as in "projection") 
#' does either of two related things:
#' (1) Given two vectors as arguments, it will project the first onto the 
#' second, returning the point in the subspace of the second that is as
#' close as possible to the first vector.  (2) Given a formula as an argument,
#' will work very much like \code{lm()}, constructing a model matrix from
#' the right-hand side of the formula and projecting the vector on the 
#' left-hand side onto the subspace of that model matrix.  In (2), rather than 
#' returning the projected vector, \code{project()} returns the coefficients
#' on each of the vectors in the model matrix.
#' UNLIKE \code{lm()}, the intercept vector is NOT included by default.  If 
#' you want an intercept vector, include \code{+1} in your formula.
#' 
#' \code{mat} returns a model matrix
#' 
#' To demonstrate singularity, use \code{singvals}.
#' 
#' @return \code{mat} returns a matrix 
#'
#' @usage project(x, u, ...)           
#' @usage mat(A,data=NULL)
#' @seealso \code{\link{linearModel}}, which returns a function.
#' @examples
#' a <- c(1,0,0); b <- c(1,2,3); c <- c(4,5,6); x <- rnorm(3)
#' dot(b,c)   # dot product
#' # projection onto the 1 vector gives the mean vector
#' mean(x)            
#' project(x, 1)
#' # return the length of the vector, rather than the vector itself
#' project(x, 1, type='length')
#' # Formula interface
#' mat(~a+b)
#' mat(~a+b+1)
#' mat(~length+sex, data=KidsFeet)
#' project(a~b)
#' project(width~length+sex, data=KidsFeet)
#' project(log(width) ~ I(length^2)+sin(length)+sex, data=KidsFeet)
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

#' @rdname linear.algebra
#' @docType methods
#' @export
#' @usage project(x, u, ...) 

setGeneric( 
	"project", 
	function(x, u, ... )  {
		standardGeneric('project')
	}
)
##################
#' @rdname linear.algebra
#' @aliases project,formula-method
#' @export
#' @return \code{project} returns the projection of \code{x} onto \code{u} 
#' (or its length if \code{u} and \code{v} are numeric vectors and \code{type == "length"})
#'

setMethod(
		  'project',
		  signature=c('formula', 'ANY'),
		  function( x, u=NULL, data=parent.frame()) {
			  # x is the formula
			  # u is just a placeholder
			  foo <- model.frame( x, data=data )
			  u <- model.response(foo)
			  M <- mat( x, data=data )
			  n <- svd(M);
			  zeros <- n$d < .0000001;
			  recip <- n$d;
			  recip[!zeros] <- 1/recip[!zeros];
        if (length(recip)==1) recip = matrix(recip)
        else recip = diag(recip)
			  coefs <- n$v%*%recip%*%t(n$u)%*%u
        rownames(coefs) <- colnames(M)
        return(coefs)
		  }
		  )
##################

# This is used in fastR and should not go away.

#' @rdname linear.algebra
#' @aliases project,numeric-method
setMethod(
	'project',
	signature=c('numeric','ANY'),
	function (x, u = rep(1, length(x)), type = c("vector", "length"), ...) {
		type <- match.arg(type)
		u <- rep(u, length.out=length(x))
		switch(type, vector = u * (dot(x, u)/dot(u, u)), length = dot(x, u)/sqrt(dot(u, u)), )
	}
)

#' @rdname linear.algebra
#' @aliases project,matrix-method
setMethod(
		  'project',
		  signature=c('matrix', 'ANY'),
		  function(x, u, data=parent.frame()) {
			  A <- x; b <- u
			  b <- cbind(b)
			  # See if A is just a single 1 --- if so, turn it into a vector of ones
			  # This would happen automatically if mat is used to combine 1 with
			  # another vector, but there is the possibility that someone might try
			  # project(1,b)
			  if( length(A[])==1 ) {
				  A <- mat(cbind(1+0*b))
			  }
			  foo <- dim(A);
			  goo <- dim(b);
			  if (goo[1] !=1 & goo[2] != 1){
				  stop('u must be a vector');
			  }
			  if (foo[1] != length(b)) {
				  stop('x must contain vectors of the same dimension as u');
			  }
			  if (foo[2] <= foo[1]) {
				  return(  cbind(lsfit(A,b,intercept=FALSE)$coef ) );
			  }
			  n <- svd(A);
			  zeros <- n$d < .0000001;
			  recip <- n$d;
			  recip[!zeros] <- 1/recip[!zeros];
			  n$v%*%diag(recip)%*%t(n$u)%*%b
		  }
		  )

#' @rdname linear.algebra
#  @param u a numeric vector
#' @param v a numeric vector
#' @return \code{dot} returns the dot product of \code{u} and \code{v}

dot <- function(u, v){
	return( sum(u*v) )
}



