#' Difference in means and proportions
#' 
#' Wrappers around `diff(mean(...))` and `diff(prop(...))` that 
#' facilitate better naming of the result
#' 
#' @param x,data,... as in [mosaic::mean()] or [mosaic::prop()]
#' @param only.2 a logical indicating whether differences should only be computed
#'   between two groups.
#' @examples
#' if (require(mosaicData)) {
#' diffprop( homeless ~ sex , data=HELPrct)
#' do(3) * diffprop( homeless ~ shuffle(sex) , data=HELPrct)
#' diffmean( age ~ substance, data=HELPrct, only.2=FALSE)
#' do(3) * diffmean(age ~ shuffle(substance), data=HELPrct, only.2=FALSE)
#' diffmean( age ~ sex, data=HELPrct)
#' do(3) * diffmean(age ~ shuffle(sex), data=HELPrct)
#' }
#' @export
diffmean <- function( x, ..., data=parent.frame(), only.2=TRUE ) {
  m <- mean_(x, ..., data=data)
  nms <- names(m)
  res <- diff(m)
  names(res) <- 
    if (length(nms) < 3) "diffmean" else paste(tail(nms,-1), head(nms, -1), sep="-")
  if (length(nms) > 2 && only.2) {
    stop("To compare more than two means, set only.2=FALSE")
  }
  res
}

#' @rdname diffmean
#' @export
diffprop <- function( x, ..., data = parent.frame(), only.2 = TRUE ) {
  p <- prop(x, ..., data = data)
  nms <- names(p)
  res <- diff(p)
  names(res) <- if (length(nms) < 3) "diffprop" else paste(tail(nms,-1), head(nms, -1), sep="-")
  if (length(nms) > 2 && only.2) {
    stop("To compare more than two proportions, set only.2=FALSE")
  }
  res
}
