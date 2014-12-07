#' Compare proportions between 2 groups
#' 
#' A function to facilitate 2 group permutation tests for a categorical outcome variable
#' 
#' @rdname compareProportion
#' @param formula a formula 
#' @param data a data frame in which \code{x} is evaluated if \code{x} is a
#' formula.
#' @param \dots other arguments
#' @return the difference in proportions between the second and first group
#' @note This funciton has been deprecated. Use \code{\link{diffprop}} instead.
#' @keywords iteration
#' @keywords stats
#' @examples
#' if (require(mosaicData)) {
#'   data(HELPrct)
#'   # calculate the observed difference
#'   mean(homeless=="housed" ~ sex, data=HELPrct)
#'   obs <- diffprop(homeless=="housed" ~ sex, data=HELPrct); obs
#'   # calculate the permutation distribution
#'   nulldist <- do(100) * diffprop(homeless=="housed" ~ shuffle(sex), data=HELPrct)
#'   histogram(~ diffprop, groups=(diffprop>= obs), nulldist, 
#'     xlab="difference in proportions")
#' }
#' @export
compareProportion <- function(formula, data=NULL, ...) {
  .Deprecated("diffprop")
  means <- mean( formula, data=data, ... )
  if (length(means) != 2) {
  	stop("number of levels for grouping variable must be 2\n")
  }
  names(means) <- NULL
  return(diff(means))
}
