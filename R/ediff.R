#' Lagged Differences with equal length
#' 
#' Often when creating lagged differences, it is awkward that the differences
#' vector is shorter than the original.  \code{ediff} pads with \code{NA}s to
#' make its output the same length as the input.
#' 
#' @seealso \code{\link{diff}} since 
#' \code{ediff} is a thin wrapper around \code{\link{diff}}.
#' 
#' @param x a numeric vector or a matrix containing the values to be differenced
#' @param lag an integer indicating which lag to use
#' @param differences an integer indicating the order of the difference
#' @param frontPad logical indicating whether padding is on the front (default) or 
#' back end.
#' @param \dots further arguments to be passed to or from methods
#' @examples
#' ediff(1:10)
#' ediff(1:10, 2)
#' ediff(1:10, 2, 2)
#' x <- cumsum(cumsum(1:10))
#' ediff(x, lag = 2)
#' ediff(x, differences = 2)
#' ediff(.leap.seconds)
#' Men <- subset(SwimRecords, sex=="M")
#' Men <- transform(Men, change=ediff(time), interval=ediff(year))
#' head(Men) 
#' @export

ediff <- function(x, lag=1, differences=1, frontPad = TRUE, ...) {
  res <- diff(x, lag=lag, difference=differences, ...)
  if (is.matrix(res)) {
    pad <- matrix(NA, nrow=lag, ncol=ncol(res))
    FUN <- rbind
  } else {
    pad <- rep(NA,lag)
    FUN <- c
  }
  if (frontPad) {
    return( FUN(pad, res) )
  }
  return( FUN(res, pad) )
}
  
