#' Functions for teaching linear algebra.
#'
#' These functions provide a formula based interface to the construction
#' of matrices from data and for fitting.  You can use them both for numerical vectors
#' and for functions of variables in data frames.
#' @rdname linear-algebra
#'
#' @name linear-algebra
#' @aliases project mat singvals
#'
#' @param A a formula.  In \code{mat()} and \code{singvals()},
#' only the right-hand side is used.
#' In \code{project()}, both sides are used, but the left-hand side should
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
#' \code{mat()} enerates a model matrix from a formula while \code{project()}
#'  carries out the operation of least-squares fitting using a 
#' singular value method. This means that even when the matrix is singular, 
#' a solution, either exact or least-squares, will be found.
#' To demonstrate singularity, use \code{singvals()}.
#' NOTE: unlike the standard formula expansion in \code{lm()}, 
#' these linear algebra functions do NOT include an intercept by default.
#' If you want
#' an intercept, put \code{+1} as a term in your formula.  (See the examples.)
#'
#' @return \code{mat} returns a matrix 
#' @author Daniel Kaplan (\email{kaplan@@macalester.edu})
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
#' project(x, 1, type='length')
#' mat(~a+b)
#' mat(~a+b+1)
#' kids = fetchData("KidsFeet.csv")
#' mat(~length+sex,data=kids)
#' project(a~b)
#' project(width~length+sex,data=kids)
#' project(log(width)~I(length^2)+sin(length)+sex,data=kids)
#' singvals(~length*sex*width,data=kids)

mat <- function(A, data=NULL) {
  if( class(A) != "formula" ) stop("Must provide a formula, e.g., ~ a or ~ a + b ")
  A <- update(A, ~-1+.) # kill off automatic Intercept term
  if( is.null(data) )
    M <- model.matrix( A )
  else
    M <- model.matrix( A, data=data ) 

  attr(M,"assign") <- NULL
  rownames(M) <- NULL

  return(M)
}

#
##################
#' @rdname linear-algebra
#' @return \code{singvals} gives singular values for each column in the model matrix
#' @export
singvals <- function(A,data=NULL){
        M = mat(A,data=data)
        # formulated to give one singular value for each column in A
	svs <- La.svd(M, nv=ncol(M), nu=ncol(M))$d;
        c( svs, rep(0,ncol(M) - length(svs)));
}
##################

setGeneric( 
	"project", 
	function(x, u, ... )  {
		standardGeneric('project')
	}
)
##################
#' @rdname linear-algebra
#' @aliases project,formula,ANY-method
#' @export
#' @return \code{project} returns the projection of \code{x} onto \code{u} 
#' (or its length if \code{u} and \code{v} are numeric vectors and \code{type == "length"})
#'

setMethod(
		  'project',
		  signature=c('formula', 'ANY'),
		  function( x, u=NULL, data=NULL) {
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

#' @rdname linear-algebra
#' @aliases project,formula,ANY-method
setMethod(
	'project',
	signature=c('numeric','ANY'),
	function (x, u = rep(1, length(x)), type = c("vector", "length"), ...) {
		type <- match.arg(type)
		switch(type, vector = u * (dot(x, u)/dot(u, u)), length = dot(x, u)/sqrt(dot(u, u)), )
	}
)

#' @rdname linear-algebra
#' @aliases project,formula,ANY-method
setMethod(
		  'project',
		  signature=c('matrix', 'ANY'),
		  function(x, u, data=NULL) {
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

#' @rdname linear-algebra
#' @param v a numeric vector
#' @return \code{dot} returns the dot product of \code{u} and \code{v}

dot <- function(u, v){
	return( sum(u*v) )
}



