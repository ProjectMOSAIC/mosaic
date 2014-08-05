#' Difference in means and proportions
#' 
#' Wrappers around \code{diff(mean(...))} and \code{diff(prop(...))} that 
#' facilitate better naming of the result
#' 
#' @param ... arguments passed to \code{mean}
#' @examples
#' diffprop( homeless ~ sex , data=HELPrct)
#' do(3) * diffprop( homeless ~ shuffle(sex) , data=HELPrct)
#' diffmean( age ~ substance, data=HELPrct)
#' do(3) * diffmean(age ~ shuffle(substance), data=HELPrct)
#' diffmean( age ~ sex, data=HELPrct)
#' do(3) * diffmean(age ~ shuffle(sex), data=HELPrct)
#' @export
diffmean <- function( ... ) {
  m <- mean(...)
  nms <- names(m)
  res <- diff(m)
  names(res) <- if (length(nms) < 3) "diffmean" else paste(tail(nms,-1), head(nms, -1), sep="-")
  res
}

#' @rdname diffmean
#' @export
diffprop<- function( ... ) {
  p <- prop(...)
  nms <- names(p)
  res <- diff(p)
  names(res) <- if (length(nms) < 3) "diffprop" else paste(tail(nms,-1), head(nms, -1), sep="-")
  res
}
