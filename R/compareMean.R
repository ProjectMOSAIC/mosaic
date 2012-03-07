#' Compare means between 2 groups
#' 
#' A function to facilitate 2 group permutation tests for a continuous outcome variable
#' 
#' @rdname compareMean
#' @param formula a formula 
#' @param data a data frame in which \code{x} is evaluated if \code{x} is a
#' formula.
#' @param \dots other arguments
#' @return the difference in means between the second and first group
#' @author Nicholas Horton (\email{nhorton@@smith.edu})
#' @seealso \code{\link{do}}, \code{\link{compareProportion}}, \code{\link{displayNullDist}} and \code{\link{shuffle}}
#' @keywords resampling
#' @export
#' @examples
#' # calculate the observed difference
#' mean(age ~ sex, data=HELPrct)
#' obs <- compareMean(age ~ sex, data=HELPrct); obs
#' # calculate the permutation distribution
#' nulldist <- do(100) * compareMean(age ~ shuffle(sex), 
#'   data=HELPrct) 
#' xhistogram(~ result, groups=(result >= obs), nulldist, 
#'   xlab="difference in means")
compareMean = function(formula, data=NULL, ...) {
  means = mean( formula, data=data, ... )
  if (length(means$S) != 2) {
  	stop("number of levels for grouping variable must be 2\n")
  }
  return(diff(means$S))
}
