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

# patterned after similar function in ggplot2

.try_require <-function(package) {
  available <- suppressMessages(suppressWarnings(sapply(package, 
                                                        require, quietly = TRUE, 
                                                        character.only = TRUE, 
                                                        warn.conflicts = FALSE)))
  missing <- package[!available]
  if (length(missing) > 0) 
    stop("Missing packages.  Please retry after installing the following: ", 
         paste(package, collapse = ", "), 
         call. = FALSE)
}

.require_manipulate <-function() {
  available <- suppressMessages(suppressWarnings(
    require("manipulate", quietly = TRUE, character.only = TRUE, warn.conflicts = FALSE)))
  if (! available) 
    stop("The manipulate package (available only in RStudio) is required.") 
}