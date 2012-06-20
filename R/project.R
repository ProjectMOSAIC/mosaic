#' Projections
#' 
#' Compute projections onto the span of a vector or a model space.
#'
#' @rdname project-methods
#' @docType methods
#' @export
#' @usage project(x, u, data, ...) 
#' @param x a vector or formula.  Left-hand sides of formulas should be a 
#' single quantity
#' @param u a vector 
#' @param data a data frame.
#' @param type  one of \code{length} or \code{vector} determining the type of the returned value

#' @details
#' \code{project} (preferably pronounced "pro-JECT" as in "projection") 
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

setGeneric( 
	"project", 
	function(x, u, data=parent.env(),... )  {
		standardGeneric('project')
	}
)
##################
#' @rdname project-methods
#' @aliases project,formula-method
#' @export
#' @return \code{project} returns the projection of \code{x} onto \code{u} 
#' (or its length if \code{u} and \code{v} are numeric vectors and \code{type == "length"})
#'
#' @examples
#' a <- c(1,0,0); b <- c(1,2,3); c <- c(4,5,6); x <- rnorm(3)
#' # projection onto the 1 vector gives the mean vector
#' mean(x)            
#' project(x, 1)
#' # return the length of the vector, rather than the vector itself
#' project(x, 1, type='length')
#' project(a~b)
#' project(width~length+sex, data=KidsFeet)
#' project(log(width) ~ I(length^2)+sin(length)+sex, data=KidsFeet)

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
			  coefs <- n$v%*%recip%*%t(n$u)%*%u
			  rownames(coefs) <- colnames(M)
			  return(coefs)
		  }
		  )
##################

# This is used in fastR and should not go away.

#' @rdname project-methods
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

#' @rdname project-methods
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

