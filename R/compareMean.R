#' Compare means between 2 groups
#' 
#' A function to calculate the difference between the means of a continuous
#' variable for two groups.
#' 
#' @rdname compareMean
#' @param formula a formula 
#' @param data a data frame in which `x` is evaluated if `x` is a
#' formula.
#' Note that the default is `data=parent.frame()`.  This makes it convenient to
#' use this function interactively by treating the working envionment as if it were 
#' a data frame.  But this may not be appropriate for programming uses.  
#' When programming, it is best to use an explicit `data` argument
#' -- ideally supplying a data frame that contains the variables mentioned
#' @param \dots other arguments
#' @return the difference in means between the second and first group
#' @note This function has been deprecated. Use [diffmean()] instead.
#' @seealso [do()], [compareProportion()] and [shuffle()]
#' @keywords iteration
#' @keywords stats
#' @examples
#' if (require(mosaicData)) {
#'   data(HELPrct)
#'   # calculate the observed difference
#'   mean(age ~ sex, data=HELPrct)
#'   obs <- diffmean(age ~ sex, data=HELPrct); obs
#'   # calculate the permutation distribution
#'   nulldist <- do(100) * diffmean(age ~ shuffle(sex), 
#'     data=HELPrct) 
#'   histogram(~ diffmean, groups=(diffmean >= obs), nulldist, 
#'     xlab="difference in means")
#' }
#' @export
compareMean <- function(formula, data=parent.frame(), ...) {
  .Deprecated("diffmean")
  means <- mean( formula, data=data, ... )
  if (length(means) != 2) {
  	stop("number of levels for grouping variable must be 2\n")
  }
  names(means) <- NULL
  return(diff(means))
}
