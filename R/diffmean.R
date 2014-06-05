#' Difference in means
#' 
#' A wrapper around \code{diff(mean(...))} that facilitates better naming of the result
#' 
#' @param ... arguments passed to \code{mean}
#' @examples
#' diffmean( age ~ substance, data=HELPrct)
#' do(3) * diffmean(age ~ substance, data=HELPrct)
#' diffmean( age ~ sex, data=HELPrct)
#' do(3) * diffmean(age ~ sex, data=HELPrct)
 
#' @export
diffmean <- function( ... ) {
  m <- mean(...)
  nms <- names(m)
  res <- diff(m)
  names(res) <- if (length(nms) < 3) "diffmean" else paste(tail(nms,-1), head(nms, -1), sep="-")
  res
}
