# It is unclear why the CRAN checks are finding a problem with u.
# this masks the problem, but it would be better to find and remove
# the issue.

utils::globalVariables(c('u'))

#' Projections
#' 
#' Compute projections onto the span of a vector or a model space, dot products, and
#' vector lengths in Euclidean space.
#'
#' @rdname project-methods
#' @docType methods
#' @param x a numeric vector (all functions) or a formula (only for `project`).  
#' Left-hand sides of formulas should be a single quantity
#' @param u a numeric vector 
#' @param v a numeric vector
#' @param data a data frame.
#' @param type one of `"length"` or `"vector"` determining the type of the 
#' returned value
#' @param coefficients For `project(y ~ x)` indicates whether the projection
#' coeffients should be returned or the projection vector.
#' @param ... additional arguments

#' @details
#' `project` (preferably pronounced "pro-JECT" as in "projection") 
#' does either of two related things:
#' (1) Given two vectors as arguments, it will project the first onto the 
#' second, returning the point in the subspace of the second that is as
#' close as possible to the first vector.  (2) Given a formula as an argument,
#' will work very much like `lm()`, constructing a model matrix from
#' the right-hand side of the formula and projecting the vector on the 
#' left-hand side onto the subspace of that model matrix.  
#' 
#' In (2), rather than 
#' returning the projected vector, `project()` returns the coefficients
#' on each of the vectors in the model matrix.
#' UNLIKE `lm()`, the intercept vector is NOT included by default.  If 
#' you want an intercept vector, include `+1` in your formula.
#' @export

setGeneric( 
	"project", 
	function(x, ... )  {
		standardGeneric('project')
	}
)
##################
#' @rdname project-methods
#' @return `project` returns the projection of `x` onto `u` 
#' (or its length if `u` and `v` are numeric vectors and `type == "length"`)
#'
#' @examples
#' x1 <- c(1,0,0); x2 <- c(1,2,3); y1 <- c(3,4,5); y2 <- rnorm(3)
#' # projection onto the 1 vector gives the mean vector
#' mean(y2)            
#' project(y2, 1)
#' # return the length of the vector, rather than the vector itself
#' project(y2, 1, type='length')
#' project(y1 ~ x1 + x2) -> pr; pr
#' # recover the projected vector 
#' cbind(x1,x2) %*% pr -> v; v
#' project( y1 ~ x1 + x2, coefficients=FALSE )
#' dot( y1 - v, v ) # left over should be orthogonal to projection, so this should be ~ 0
#' if (require(mosaicData)) {
#' project(width~length+sex, data=KidsFeet)
#' }
#' @export

setMethod(
		  'project',
		  signature=c('formula'),
		  function( x, u=NULL, data=parent.frame(2), coefficients=TRUE, ...) {
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
        # convert to a vector
        vcoefs <- as.vector(coefs)
        names(vcoefs) <- colnames(M)
        if (coefficients) 
          return(vcoefs)
        else {
          return(as.vector( M %*% vcoefs ) )
        }
    
		  }
		  )
##################

# This is used in fastR and should not go away.


#' @rdname project-methods
#' @export

setMethod(
	'project',
	signature=c('numeric'),
	function (x, u = rep(1, length(x)), type = c("vector", "length", "coef"), ...) {
		type <- match.arg(type)
		u <- rep(u, length.out=length(x))
		
		if(dot(u,u) <= 0) {
		  return(
		    switch(type, 
		       vector = rep(0, length(u)),
		       length = 0,
		       coef   = 0
		       )
		  )
		}
		
		switch(type, 
		       vector = u * (dot(x, u)/dot(u, u)), 
		       length = abs(dot(x, u))/sqrt(dot(u, u)), 
		       coef   =  dot(x, u)/dot(u, u) 
		       )
	}
)

#' @rdname project-methods
#' @export

setMethod(
		  'project',
		  signature=c('matrix'),
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
#' @return `vlength` returns the length of the vector 
#' (i.e., the square root of the sum of the squares of the components)
#' @seealso \code{link{project}}
#' @examples
#' vlength(rep(1,4))
#' if (require(mosaicData)) {
#' m <- lm( length ~ width, data=KidsFeet )
#' # These should be the same
#' vlength( m$effects )  
#' vlength( KidsFeet$length)
#' # So should these
#' vlength( tail(m$effects, -2) )
#' sqrt(sum(resid(m)^2))
#' }
#' v <- c(1,1,1); w <- c(1,2,3)
#' u <- v / vlength(v)  # make a unit vector
#' # The following should be the same:
#' project(w,v, type="coef") * v 
#' project(w,v)
#' # The following are equivalent
#' abs(dot( w, u ))
#' vlength( project( w, u) )
#' vlength( project( w, v) )
#' project( w, v, type='length' )
#' @export

vlength <- function(x, ...) {
   sqrt(sum(x^2))
}

#' @rdname project-methods
#' @return `dot` returns the dot product of `u` and `v`
#' @export

dot <- function(u, v){
  return( sum(u*v) )
}
