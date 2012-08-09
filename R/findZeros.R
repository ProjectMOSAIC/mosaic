#' Find the zeros of a function
#' 
#' Compute numerically the zeros of a function.
#' @param expr A formula.  The right side names the variable with respect to which the zeros should be found.  
#' The left side is an expression, e.g. \code{sin(x) ~ x}.  
#' All free variables (all but the variable on the right side) named in the expression must be assigned 
#' a value via \code{\ldots}
#' @param \dots Specific numerical values for the free variables in the expression.
#' @param xlim The range of the dependent variable to search for zeros. \code{Inf} is a legitimate value, 
#' but is interpreted in the numerical sense as the non-Inf largest floating point number.  This can also
#' be specified replacing \code{x} with the name of the variable.  See the examples.
#' @param near a value near which zeros are desired
#' @param within only look for zeros at least this close to near.  \code{near} and \code{within} provide an
#' alternative to using \code{xlim} to specify the search space.
#' @param nearest the number of nearest zeros to return.  Fewer are returned if fewer are found.
#' @param iterate maximum number of times to iterate the search. Subsequent searches take place with the range
#'        of previously found zeros.  Choosing a large number here is likely to kill performance without 
#'        improving results, but a value of 1 (the default) or 2 works well when searching in \code{c(-Inf,Inf)} for
#'        a modest number of zeros near \code{near}.
#' @param npts How many sub-intervals to divide the \code{xlim} into when looking for candidates for zeros.  
#' The default is usually good enough.
#' If \code{Inf} is involved, the intervals are logarithmically spaced up to the largest finite floating point number.  
#' There is no guarantee that all the roots will be found.
#' @param sortBy specifies how the zeros found will be sorted. Options are 'byx', 'byy', or 'radial'.
#' 
#' @details
#' Searches numerically using \code{uniroot}.
#' 
#' @return A dataframe of zero or more numerical values.  Plugging these into the
#' expression on the left side of the formula should result in values near zero.
#'
#' @author Daniel Kaplan (\email{kaplan@@macalester.edu}) 
#' 
#' @export
#' @examples
#' findZeros( sin(t) ~ t, xlim=c(-10,10) )
#' # Can use tlim or t.lim instead of xlim if we prefer
#' findZeros( sin(t) ~ t, tlim=c(-10,10) )
#' findZeros( sin(theta) ~ theta, near=0, nearest=20)
#' findZeros( A*sin(2*pi*t/P) ~ t, xlim=c(0,100), P=50, A=2)
#' # Interval of a normal at half its maximum height.
#' findZeros( dnorm(x,mean=0,sd=10) - 0.5*dnorm(0,mean=0,sd=10) ~ x )
#' # A pathological example
#' # There are no "neareset" zeros for this function.  Each iteration finds new zeros.
#' f <- function(x) { if (x==0) 0 else sin(1/x) }
#' findZeros( f(x) ~ x, near=0 )
#' # Better to look nearer to 0
#' findZeros( f(x) ~ x, near=0, within=100 )
#' findZeros( f(x) ~ x, near=0, within=100, iterate=0 )
#' findZeros( f(x) ~ x, near=0, within=100, iterate=3 )
#' #Zeros in multiple dimensions
#' findZeros(x^2+y^2+z^2-5~x&y&z, nearest=3000, within = 5)
#' findZeros(x*y+z^2~z&y&z, z+y~x&y&z, npts=10)
#' @keywords calculus 
findZeros <- function(expr, ..., xlim=c(near-within, near+within), near=0, within=Inf, 
                      nearest=10, npts=1000, iterate=1, sortBy=c('byx', 'byy', 'radial')) {
  dots <- list(...)
  sortBy <- match.arg(sortBy)
  rhsVars <- all.vars(rhs(expr))
  if (is.list(iterate)) { # this is a recursive call
    ignore.limits <- iterate[['ignore.limits']]
    iterate <- iterate[['iterate']]
  } else { # this is the original call
    ignore.limits <- FALSE
  }
  
  if( length(rhsVars) != 1 ){
    if(within==Inf)
      within=100
    return(unique(signif(findZerosMult(expr,..., npts=nearest, rad=within, near = near, sortBy = sortBy), 7)))
  }
  
  pfun <- function(x){  # removed . from name, was .x
    mydots <- dots
    mydots[[rhsVars]] <- x
    eval( lhs(expr), envir=mydots, enclos=parent.frame() )
  }
  
  if (! ignore.limits ) {
    xlim <- inferArgs( dots=dots, vars=rhsVars, defaults=list(xlim=xlim) )[['xlim']]
  }
  tryCatch( xlim <- range(xlim), error = function(e) stop(paste('Improper limits value --', e)))
  
  if( xlim[1] >= xlim[2] )  
    stop(paste("Left limit (", xlim[1], ") must be less than right limit (", xlim[2], ")."))
  internal.near <- near
  if ( internal.near  < xlim[1] || internal.near  > xlim[2]) { 
    internal.near  <- mean(xlim[1],xlim[2]) 
  }
  mx <- max(xlim - internal.near )  # max amount to add to internal.near  when searching
  mn <- min(xlim - internal.near )  # min amount to add to internal.near  when searching (will be negative)
  if (mx < 0) stop('Bug alert: near outside search interval.')
  if (mn > 0) stop('Bug alert: near outside search interval.')
  
  # Deal with very large numbers for the interval, e.g. Inf
  verybig <- .Machine$double.xmax
  plainbig <- npts^(.75) # linear spacing below this.
  mx <- max( min( verybig,mx), -verybig)
  mn <- min( max(-verybig,mn),  verybig)
  rightseq  <- NULL
  leftseq   <- NULL
  middleseq <- NULL
  if( mx > plainbig ) { 
    rightseq <- exp( seq( log(max(plainbig,mn)),log(mx),length=npts) )
  }
  middleseq <- seq( max(-plainbig,mn), min(plainbig,mx), length=npts)
  if( mn < -plainbig ){
    leftseq <- -exp( seq( log(-mn), log(-min(-plainbig,mx)), length=npts))
  }
  
  searchx <- sort(unique(internal.near   + c(0, leftseq, middleseq, rightseq)))
  
  y <- sapply( searchx, pfun )
  testinds <- which( diff(sign(y)) != 0 )
  if (length(testinds) < 1 ) {
    warning("No zeros found.  You might try modifying your search window or increasing npts.")
    return( numeric(0) )
  } else {
    testinds <- testinds[ order(abs(searchx[testinds] - near)) ]
    N <- min( length(testinds), 2*nearest )
    zeros <- rep(NA, N )
    for (k in 1:N) {  # look in subinterval k, i.e., between testinds[k] and  testinds[k+1]
      if ( searchx[testinds[k]] < searchx[testinds[k]+1] ) {
        ur <- uniroot(function(qqzz){ sapply( qqzz, pfun) }, lower=searchx[testinds[k]], upper=searchx[testinds[k]+1])
        zeros[k] <- round( ur$root, digits=trunc(-log10(ur$estim.prec)) )
      } else {
        warning("Potential bug alert: Attempting to search in region where signs of function at endpoints are equal.  Skipping this interval.")
      }
    }
  }
  
  o <- order( abs(zeros - near) )
  result <- sort(unique(zeros[o[1:min(nearest,length(zeros))]]))
  if ( iterate > 0 && length(result) > 1 )  {
    adjust <- min(diff(result))
    # Note: negative value of iterate to indicate that we will be in a secondary iteration
    return ( findZeros( expr, ..., xlim=range(c(result-adjust, result+adjust)), 
                        near=near, within=within, 
                        nearest=nearest, 
                        npts=npts, 
                        iterate= list(iterate=iterate - 1, ignore.limits = TRUE) ) )
  } else {
    result <- data.frame(result)
    colnames(result)<- rhsVars
    return(unique(signif(result, 7)))
  }
}
