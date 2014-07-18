#' Avoiding inter-package collisions
#' 
#' Several popular packages export functions with the same names.  The \pkg{mosaic}
#' package attempts to help these other packge co-exist by creating generic functions
#' with the same name and dispatching to the appropriate function based on the (first)
#' arguemnt supplied.
#' 
#' @rdname collisions
#' @name collisions
#' 
#' @param object and R object
#' @export
select <- function(object, ...) {
  UseMethod("select")
}

#' @rdname collisions
#' @name collisions
#' @export
select.tbl <- function(object, ...) {
  if (! "packge:dplyr" %in% search()) {
    stop("The `dplyr' package needs to be installed and loaded to use select this way.")
  }
  dplyr::select(.data=object, ...)
}

#' @rdname collisions
#' @name collisions
#' @export
select.ridgelm <- function(object, ...) {
  if (! "packge:MASS" %in% search()) {
    stop("The `MASS' package needs to be installed and loaded to use select this way.")
  }
  MASS::select(formula=object, ...)
}

#' @rdname collisions
#' @name collisions
#' @export
select.default <- function(object, ...) {
  message("mosaic is aware of select functions in the MASS and dplyr packages.")
  message("But your usage does not conform to either of these.  If you are trying")
  message("to use some other select, please use a fully qualified name.")
}