#' All pairs mean and sum of absolute differences
#' 
#' All pairs mean and sum of absolute differences
#' 
#' @param x a numeric vector
#' @param ... if present, appended to x
#' @param na.omot a logical indicating whether NAs should be removed before
#' calculaing.
#' @return the mean or sum of the absolute differences between each pair
#' of values in \code{c(x,...)}.
#' @seealso \code{link{mad}}
#' @export
#' @examples
#' SAD(1:3)
#' MAD(1:3)
MAD <- function(x, ..., na.rm=getOption("na.omit", FALSE)) {
  SAD(x, ..., na.rm=na.rm) / length(x)
}

#' @rdname MAD
SAD <- function(x, ..., na.omit=getOption("na.omit", FALSE)) {
  x <- c(x,...)
  x <- na.omit(x)
  M <- outer(x, x, "-")
  base::sum( upper.tri(M) * abs(M) )
}