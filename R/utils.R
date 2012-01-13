#' @details
#' \code{.do.safe.call} avoids conflicts between named arguments and ... by taking named arguments 
#' preferentially.
#'
#' @rdname mosaic-internal
#'
#' @param what either a function or a non-empty character string naming the function to be called.
#' @param args a list of arguments to the function call. The names attribute of args gives the argument names.
#' @param quote a logical value indicating whether to quote the arguments.
#' @param envir an environment within which to evaluate the call. 
#'	This will be most useful if what is a character string and the arguments are symbols or quoted expressions.
#' @return The result of the (evaluated) function call.
#'
#' @seealso \code{\link{do.call}}
#' @keywords internal

.do.safe.call <- function (what, args, quote = FALSE, envir = parent.frame(), ...) {
	dots <- list(...)
	args <- modifyList(dots, args)
	do.call(what, args, quote, envir)
}

