#' Central portion of a distribution
#' 
#' This function determines the critial values for isolating 
#' a central portion of a distribution with a specified probability.
#' This is designed to work especially well for symmteric distributions,
#' but it can be used with any distribution.
#' 
#' @param dist a character string naming a distribution family (e.g., "norm").
#' This wil work for any family for which the usual d/p/q functions exist.
#' @param p the proportion to be in the central region, with equal proportions 
#' in either "tail".
#' @param ... additional arguments passed to the distribution functions.  Typically
#' these specify the parameters of the particular distribution desired.  See the examples.
#' @param tail one of \code{"upper"} or \code{"lower"} specifying whether 
#' the lower or upper critical value is returned.  
#' @param warn a logical indicating whether a warning should be given when
#' using a distribution that is not symmetric.
#' @note This function is still experimental and changes the input or output
#' formats are possible in future versions of the package.
#' 
#' @export
#' @examples
#' cdist( "norm", .95)
#' cdist( "t", c(.90, .95, .99), df=5)
#' cdist( "t", c(.90, .95, .99), df=50)
#' cdist( "t", .95, df=c(3,5,10,20) )
#' cdist( "norm", .95, mean=500, sd=100 )
#' cdist( "chisq", c(.90, .95), df=3 )
#' cdist( "chisq", c(.90, .95), df=3, tail="lower" )

cdist <- function( dist, p, ... , tail=c("upper","lower"), warn=TRUE) {
  tail = match.arg(tail)
  alpha <- (1-p)/2
  lo <- alpha
  hi <- 1 - alpha
  qdist <- paste0("q", dist)
  loT <- do.call( qdist, c(list(lo), list(...) ) ) 
  hiT <- do.call( qdist, c(list(hi), list(...) ) ) 
  if ( any( abs(hiT) - abs(loT) > 1e-5 ) ) {
    if (warn) warning(paste0("It looks like your distribution is not symmetric.  I'm providing ", tail, " tails.") )
  }
  
  return(
    switch( tail,
            "lower" = loT,
            "upper" = hiT
    )
  )
}
# another possible implementation
.cdist <- function( dist, p, ... ) {
  alpha <- (1-p)/2
  lo <- alpha
  hi <- 1 - alpha
  qdist <- paste0("q", dist)
  data.frame( prob=p, lo= do.call( qdist, c(list(lo), ... ) ), hi=do.call( qdist, c(list(hi), ... ) ) )
}
