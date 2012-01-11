# functions for doing linear algebra.

mat <- function(A, data=NULL) {
  # Need to figure out how to suppress the automatic creation of an intercept
  # looks like deparse(A)
  if( class(A) != "formula" ) stop("Must provide a formula, e.g., ~ a or ~ a + b ")
  if( is.null(data) )
    M <- model.matrix( A )
  else
    M <- model.matrix( A, data=data )

  attr(M,"assign") <- NULL
  rownames(M) <- NULL

  return(M)
}



setGeneric( 
	"project", 
	function(x, u, ... )  {
		standardGeneric('project')
	}
)

setMethod(
	'project',
	signature=c('numeric','ANY'),
	function (x, u = rep(1, length(x)), type = c("vector", "length"), ...) {
		type <- match.arg(type)
		switch(type, vector = u * (dot(x, u)/dot(u, u)), length = dot(x, u)/sqrt(dot(u, u)), )
	}
)  

setMethod(
		  'project',
		  signature=c('formula', 'ANY'),
		  function( x, u=NULL, data=NULL) {
			  # x is the formula
			  # u is just a placeholder
			  foo <- model.frame( x, data=data )
			  u <- model.response(foo)
			  M <- mat( x, data=data )
			  project( M, u )
		  }
		  )


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


dot <- function(u,v){
#	if (length(u) != length(v)) {
#		stop('Vectors not of equal length.');
#        }
	return( sum(u*v) )
}

singvals <- function(A){ 
        # formulated to give one singular value for each column in A
	foo <- La.svd(A, nv=ncol(A), nu=ncol(A))$d;
        c( foo, rep(0,ncol(A) - length(foo)));
}

