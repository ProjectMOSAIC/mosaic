tryCatch(utils::globalVariables(c('.mosaicEnv')),
		 error=function(e) message('Looks like you should update R.'))

#' Setting options for mosaic package functions
#'
#' A mechanism for setting options in the mosaic package.
#'
#' @rdname mosaic.options
#' @name mosaic.options
#' @aliases mosaicGetOption mosaic.par.set 
#'
#' @param name the name of the option being set
#' @param value the value to which to set the option
#' @export

## copied from lattice

mosaic.options <- function (...) 
{
    new <- list(...)
    if (is.null(names(new)) && length(new) == 1 && is.list(new[[1]])) 
        new <- new[[1]]
    old <- .mosaicEnv$mosaic.options
    if (length(new) == 0) 
        return(old)
    nm <- names(new)
    if (is.null(nm)) 
        return(old[unlist(new)])
    isNamed <- nm != ""
    if (any(!isNamed)) 
        nm[!isNamed] <- unlist(new[!isNamed])
    retVal <- old[nm]
    names(retVal) <- nm
    nm <- nm[isNamed]
    .mosaicEnv$mosaic.options <- .updateList(old, new[nm])
    return(invisible(retVal))
}
#' @rdname mosaic.options
#' @export

mosaic.getOption <- function (name) 
{
    get("mosaic.options", envir = .mosaicEnv)[[name]]
}
#' @rdname mosaic.options
#' @param theme a list appropriate for a mosaic theme
#' @param warn a logical.  UNUSED at present.
#' @param strict a logical or numeric.
#' @param \dots additional arguments that are turned into a list if a list cannot be inferred from 
#' \code{theme}, \code{name}, and \code{value}.
#' @export

mosaic.par.set <- function (name, value, ..., theme, warn = TRUE, strict = FALSE) 
{
    mosaic.theme <- get("mosaic.theme", envir = .mosaicEnv)
	old.mosaic.theme <- mosaic.theme
    if (missing(theme)) {
        if (!missing(value)) {
            theme <- list(value)
            names(theme) <- name
        }
        else if (!missing(name) && is.list(name)) {
            theme <- name
        }
        else theme <- list(...)
    } else {
        if (is.character(theme)) 
            theme <- get(theme)
        if (is.function(theme)) 
            theme <- theme()
        if (!is.list(theme)) {
            warning("Invalid 'theme' specified")
            theme <- NULL
        }
    }
    if (strict) {
        if (strict > 1L) 
            mosaic.theme <- theme
        else mosaic.theme[names(theme)] <- theme
    }
    else mosaic.theme <- .updateList(mosaic.theme, theme)
    assign("mosaic.theme", mosaic.theme, envir = .mosaicEnv)
    return(invisible(old.mosaic.theme))
}
#' @rdname mosaic.options
#' @export

mosaic.par.get <- function (name = NULL) 
{
    mosaic.theme <- get("mosaic.theme", envir = .mosaicEnv)

    if (is.null(mosaic.theme)) {
        mosaic.theme <- get("mosaic.theme", envir = .mosaicEnv)
    }
    if (is.null(name)) 
        mosaic.theme
    else if (name %in% names(mosaic.theme)) 
        mosaic.theme[[name]]
    else NULL
}
#' @keywords ignore
.updateList <- function(x, val)
{
  if (is.null(x)) x <- list()
  modifyList(x, val)
}
#' @keywords ignore
.defaultMosaicOptions <- function()
  list(na.rm=TRUE,
       aggregate.overall=FALSE,
       graphics='lattice'
       )
#' @rdname mosaic.options
#' @details \code{restoreLatticeOptions} returns any lattice
#' options that were changed when the mosiac package was loaded
#' back to their pre-mosaic state.
#' @export

restoreLatticeOptions <- function() {
  do.call(lattice::lattice.options, .mosaicEnv$original.lattice.options)
}

#' @rdname mosaic.options
#' @details \code{mosaicLatticeOptions} sets a number 
#' of defaults for lattice graphics.
#' @export

mosaicLatticeOptions <- function() {
  lattice::lattice.options(
    histogram.breaks = xhistogramBreaks,
    prepanel.default.histogram = prepanel.xhistogram,
    panel.histogram = panel.xhistogram)
}
