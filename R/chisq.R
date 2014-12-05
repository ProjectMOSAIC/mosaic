
#' Extract Chi-squared statistic
#' 
#' Extract Chi-squared statistic
#' 
#' @param x,... Either an object of class \code{"htest"} coming from a Chi-squared test or
#' the inputs to \code{\link{chisq.test}}.
#' @seealso \code{\link{stat}}
#' @examples
#' 
#' if(require(mosicData)) {
#'   Mites.table <- tally( ~ outcome + treatment, data=Mites )
#'   Mites.table 
#'   chisq.test(Mites.table)
#'   chisq(Mites.table)
#'   chisq(chisq.test(Mites.table))
#'   ## Randomization test.  Increase replications to decrease Monte Carlo error.
#'   do(3) * chisq( tally( ~ outcome + shuffle(treatment),  data=Mites ) )
#'   Mites.rand <- do(1000) * chisq( tally( ~ outcome + shuffle(treatment),  data=Mites ) )
#'   tally( ~(X.squared >= chisq(Mites.table)), data=Mites.rand, format="proportion")
#' }
#'   
#' @export
chisq <- function(x, ...) {
  UseMethod("chisq")
}

#' @rdname chisq
#' @export
chisq.htest <- function(x, ...) {
  if (! grepl ("Chi-squared", x$method) ) {
    stop("This doesn't look like a Chi-sqaured test to me.")
  }
  return(x$statistic)
}
  
#' @rdname chisq
#' @export
chisq.default <- function(x, ...) {
  chisq.test(x, ...)$statistic
}