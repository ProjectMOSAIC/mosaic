#' Avoiding inter-package collisions
#' 
#' Several popular packages export functions with the same names.  The \pkg{mosaic}
#' package attempts to help these other package co-exist by creating generic functions
#' with the same name and dispatching to the appropriate function based on the (first)
#' arguemnt supplied.
#' 
#' @rdname collisions
#' @name collisions
#' 
#' @param object and R object
#' @param ... additional arguments
#' @export
select <- function(object, ...) {
  UseMethod("select")
}

#' @rdname collisions
#' @name collisions
#' @export
select.tbl <- function(object, ...) {
  dplyr:::select.tbl(object, ...)
}

#' @rdname collisions
#' @name collisions
#' @export
select.data.frame  <- function(object, ...) {
  dplyr:::select.data.frame(object, ...)  
}

#' @rdname collisions
#' @name collisions
#' @export
select.ridgelm <- function(object, ...) {
  MASS:::select.ridgelm(formula=object, ...)
}

#' @rdname collisions
#' @name collisions
#' @export
select.default <- function(object, ...) {
  message("mosaic is aware of select functions in the utils, MASS and dplyr packages.")
  message("But your usage does not appear to conform to one of these.  If you are trying")
  message("to use some other select, please use a fully qualified name.")
}
