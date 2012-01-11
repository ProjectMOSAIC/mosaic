# utilities, for internal use primarily

# this avoid conflicts between named arguments and ... by taking named arguments preferentially

.do.safe.call <- function (what, args, quote = FALSE, envir = parent.frame(), ...) {
	dots <- list(...)
	args <- modifyList(dots, args)
	do.call(what, args, quote, envir)
}

