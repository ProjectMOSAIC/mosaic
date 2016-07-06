
# tryCatch(utils::globalVariables(c('RStudioGD')), 
#          error=function(e) message('Looks like you should update R.'))

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
# a bit flakey at the moment, so I'm eliminating its use in favor of directly calling
# requireNamespace.  ---rjp


.try_requireNamespace <-function(package) {
  available <- suppressMessages(suppressWarnings(sapply(package, 
                                                        requireNamespace, quietly = TRUE, 
                                                        character.only = TRUE, 
                                                        warn.conflicts = FALSE)))
  missing <- package[!available]
  if (length(missing) > 0) 
    stop("Missing packages.  Please retry after installing the following: ", 
         paste(missing, collapse = ", "), 
         call. = FALSE)
}

.require_manipulate_namespace <-function() {
  if (! rstudio_is_available())
    stop("RStudio required for manipulate.") 
  if (! requireNamespace("manipulate", quietly=TRUE)) 
    stop("Unable to locate the manipulate package.  Please install with `install.packages(\"manipulate\")'")
  requireNamespace("manipulate", quietly=TRUE)
}

#' Check whether RStudio is in use
#' 
#' This functions checks that RStudio is in use.  It will likely be removed
#' from this package once the versions of RStudio in popular use rely on the 
#' manipulate package on CRAN which will provide its own version.
#' 
#' @return a logical
#' 
#' @rdname rstudio
#' 
#' @export
rstudio_is_available <- function() {
  identical(.Platform$GUI, "RStudio")
}

is_object_not_found_error <- function(e, name = "") {
  grepl("object", e$message) && grepl(name, e$message) && grepl("not found", e$message)
}

# f_new <- function (rhs, lhs = NULL, env = parent.frame()) {
#   if (!is_lang(rhs) && !is.null(rhs)) {
#     stop("`rhs` must be a language object", call. = FALSE)
#   }
#   if (!is_lang(lhs) && !is.null(lhs)) {
#     stop("`lhs` must be a language object", call. = FALSE)
#   }
#   if (!is.environment(env)) {
#     stop("`env` must be an environment", call. = FALSE)
#   }
#   if (is.null(lhs)) {
#     f <- call_new("~", rhs)
#   }
#   else {
#     f <- call_new("~", lhs, rhs)
#   }
#   structure(f, class = "formula", .Environment = env)
# }
# 
# lazy2f <- function(x) {
#   f_new(x$expr, env = x$env)
# }
