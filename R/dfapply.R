#' apply-type function for data frames
#' 
#' An \code{apply}-type function for data frames.
#' @param data  data frame 
#' @param FUN 
#'   a function to apply to (some) variables in the data frame
#' @param select 
#'   function used to select variables to which \code{FUN} is applied.  See examples.
#' @param \dots arguments passed along to \code{FUN} 
#' 
#' @seealso \code{\link{apply}},
#' \code{\link{sapply}},
#' \code{\link{tapply}},
#' \code{\link{lapply}}
#' 
#' @examples
#' dfapply(iris, favstats)
#' if (require(mosaicData)) {
#' dfapply(HELPrct, table, select=is.factor)
#' }
#' @export

dfapply <-
function (data, FUN, select = is.numeric, ...) 
{
    if (is.function(select)) {
        select <- sapply(data, select)
    }
    if (is.logical(select)) {
        select <- rep(select, length.out = dim(data)[2])
        select <- which(select)
    }
    if (!is.numeric(select)) {
        stop("Unusable selection parameter.")
    }
    lapply(data[, select, drop = F], FUN, ...)
}
