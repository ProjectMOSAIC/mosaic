#' apply-type function for data frames
#' 
#' An \code{apply}-type function for data frames.
#' @param data  data frame 
#' @param FUN 
#'   a function to apply to (some) variables in the data frame
#' @param select 
#'   function used to select variables to which \code{FUN} is applied.  See examples.
#' @param \dots  arguments passed allong to \code{FUN} 
#' 
#' @author Randall Pruim (\email{rpruim@@calvin.edu})
#' @seealso \code{\link{apply}},
#' \code{\link{sapply}},
#' \code{\link{tapply}},
#' \code{\link{lapply}}
#' 
#' @export
#' @examples
#' dfapply(iris, favstats)
#' dfapply(HELPrct, table, select=is.factor)
#' 
#' 
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
    apply(data[, select, drop = F], 2, FUN, ...)
}
