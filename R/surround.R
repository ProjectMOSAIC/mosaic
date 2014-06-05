#' Format strings for pretty output
#' 
#' @param x a vector 
#' @param pre text to prepend onto string
#' @param post text to postpend onto string
#' @param width desired width of string
#' @param \dots additional arguments passed to \code{\link{format}}
#' @return a vector of strings padded to the desired width
#' @examples
#' surround(rbinom(10,20,.5), " ", " ", width=4)
#' surround(rnorm(10), " ", " ", width=8, digits = 2, nsmall = 2)
#' @export

surround <- function (x, pre = " ", post = " ", width = 8, ...) 
{
    x <- format(as.vector(x), ...)
    l <- length(x)
    format(paste(rep(pre, l), x, rep(post, l), sep = ""), width = width, just = "centre")
}
