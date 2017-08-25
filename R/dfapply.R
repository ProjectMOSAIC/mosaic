#' apply-type function for data frames
#' 
#' An `apply`-type function for data frames.
#' @param data  data frame 
#' @param FUN 
#'   a function to apply to (some) variables in the data frame
#' @param select a logical, character (naming variables), or numeric vector or a 
#'   function used to select variables to which `FUN` is applied.  If a function,
#'   it should take a vector as input and return a single logical. See examples.
#' @param \dots arguments passed along to `FUN` 
#' 
#' @seealso [apply()],
#' [sapply()],
#' [tapply()],
#' [lapply()],
#' [inspect()]
#' 
#' @examples
#' dfapply(iris, favstats, select = is.numeric)
#' dfapply(iris, favstats, select = c(TRUE, TRUE, FALSE, FALSE, FALSE))
#' dfapply(iris, favstats, select = c(1,2))
#' dfapply(iris, favstats, select = c("Sepal.Length", "Petal.Length"))
#' dfapply(HELPrct, table, select = is.factor)
#' do.call(rbind, dfapply(HELPrct, favstats, select = is.numeric))
#' @export

dfapply <-
function (data, FUN, select = TRUE, ...) 
{
    if (is.function(select)) {
        select <- sapply(data, select)
    }
    if (is.logical(select)) {
        select <- rep(select, length.out = dim(data)[2])
        select <- which(select)
    }
    if (is.character(select)) {
       select <- sapply(select, function(x) which (x == names(data)))
    }
    if (!is.numeric(select)) {
        stop("Unusable selection parameter.")
    }
    lapply(data[, select, drop = F], FUN, ...)
}
