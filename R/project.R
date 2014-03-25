#' Projections
#' 
#' Compute projections onto the span of a vector or a model space, dot products, and
#' vector lengths in Euclidean space.
#'
#' @rdname project-methods
#' @docType methods
#' @export
# @usage project(x, u, data, ...) 
#' @param x a numeric vector (all functions) or a formula (only for \code{project}).  
#' Left-hand sides of formulas should be a single quantity
#' @param u a numeric vector 
#' @param v a numeric vector
#' @param data a data frame.
#' @param type one of \code{"length"} or \code{"vector"} determining the type of the 
#' returned value
#' @param ... additional arguments

#' @details
#' \code{project} (preferably pronounced "pro-JECT" as in "projection") 
#' does either of two related things:
#' (1) Given two vectors as arguments, it will project the first onto the 
#' second, returning the point in the subspace of the second that is as
#' close as possible to the first vector.  (2) Given a formula as an argument,
#' will work very much like \code{lm()}, constructing a model matrix from
#' the right-hand side of the formula and projecting the vector on the 
#' left-hand side onto the subspace of that model matrix.  
#' 
#' In (2), rather than 
#' returning the projected vector, \code{project()} returns the coefficients
#' on each of the vectors in the model matrix.
#' UNLIKE \code{lm()}, the intercept vector is NOT included by default.  If 
#' you want an intercept vector, include \code{+1} in your formula.

setGeneric( 
	"project", 
	function(x, u, data=parent.env(), ... )  {
		standardGeneric('project')
	}
)
##################
#' @rdname project-methods
# @aliases project,formula,ANY-method
# @aliases project,formula-method
#' @export
#' @return \code{project} returns the projection of \code{x} onto \code{u} 
#' (or its length if \code{u} and \code{v} are numeric vectors and \code{type == "length"})
#'
#' @examples
#' a <- c(1,0,0); b <- c(1,2,3); c <- c(3,4,5); x <- rnorm(3)
#' # projection onto the 1 vector gives the mean vector
#' mean(x)            
#' project(x, 1)
#' # return the length of the vector, rather than the vector itself
#' project(x, 1, type='length')
#' project(c ~ a + b) -> pr; pr
#' # recover the actual vector
#' cbind(a,b) %*% pr -> v; v
#' dot( c-v, v ) # left over should be orthogonal to projection, so this should be ~ 0
#' project(width~length+sex, data=KidsFeet)
# @usage
# \S4method{project}{formula}( x, u=NULL, data=parent.frame(), ...) 
setMethod(
		  'project',
		  signature=c('formula', 'ANY'),
		  function( x, u=NULL, data=parent.frame(),...) {
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
			  coefs <- n$v %*% recip %*% t(n$u) %*% u
			  rownames(coefs) <- colnames(M)
			  return(coefs)
		  }
		  )
##################

# This is used in fastR and should not go away.


#' @rdname project-methods
# @aliases project,numeric,ANY-method
# @aliases project,numeric-method
# @usage
# \S4method{project}{numeric}(x, u = rep(1, length(x)), type = c("vector", "length"), ...) 
setMethod(
	'project',
	signature=c('numeric','ANY'),
	function (x, u = rep(1, length(x)), type = c("vector", "length"), ...) {
		type <- match.arg(type)
		u <- rep(u, length.out=length(x))
		switch(type, vector = u * (dot(x, u)/dot(u, u)), length = dot(x, u)/sqrt(dot(u, u)), )
	}
)

#' @rdname project-methods
# @aliases project,matrix,ANY-method
# @aliases project,matrix-method
# @usage
# \S4method{project}{matrix}(x, u, data=parent.frame())
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
 
#' @rdname project-methods
#' @return \code{vlength} returns the length of the vector 
#' (i.e., the square root of the sum of the squares of the components)
#' @seealso \code{link{project}}
#' @export
#' @examples
#' vlength(rep(1,4))
#' m <- lm( length ~ width, data=KidsFeet )
#' # These should be the same
#' vlength( m$effects )  
#' vlength( KidsFeet$length)
#' # So should these
#' vlength( tail(m$effects, -2) )
#' sqrt(sum(resid(m)^2))
#' v <- c(1,1,1); w <- c(1,2,3)
#' u <- v / vlength(v)  # make a unit vector
#' # The following are equivalent
#' dot( w, u )
#' vlength( project( w, u) )
#' vlength( project( w, v) )
#' project( w, v, type='length' )

vlength <- function(x, ...) {
   sqrt(sum(x^2))
}

#' @rdname project-methods
#' @return \code{dot} returns the dot product of \code{u} and \code{v}
#' @export


dot <- function(u, v){
  return( sum(u*v) )
}
