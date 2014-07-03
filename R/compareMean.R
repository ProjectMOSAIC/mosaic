#' Compare means between 2 groups
#' 
#' A function to calculate the difference between the means of a continuous
#' variable for two groups.
#' 
#' @rdname compareMean
#' @param formula a formula 
#' @param data a data frame in which \code{x} is evaluated if \code{x} is a
#' formula.
#' @param \dots other arguments
#' @return the difference in means between the second and first group
#' @seealso \code{\link{do}}, \code{\link{compareProportion}} and \code{\link{shuffle}}
#' @keywords iteration
#' @keywords stats
#' @examples
#' data(HELPrct)
#' # calculate the observed difference
#' mean(age ~ sex, data=HELPrct)
#' obs <- compareMean(age ~ sex, data=HELPrct); obs
#' # calculate the permutation distribution
#' nulldist <- do(100) * compareMean(age ~ shuffle(sex), 
#'   data=HELPrct) 
#' histogram(~ result, groups=(result >= obs), nulldist, 
#'   xlab="difference in means")
#' @export
compareMean = function(formula, data=NULL, ...) {
  means = mean( formula, data=data, ... )
  if (length(means) != 2) {
  	stop("number of levels for grouping variable must be 2\n")
  }
  names(means) <- NULL
  return(diff(means))
}
