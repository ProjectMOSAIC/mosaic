
## copied from lattice
.updateList <- function(x, val)
{
    if (is.null(x)) x <- list()
    modifyList(x, val)
}

.defaultMosaicOptions <- function()
    list(na.rm=TRUE,
         aggregate.overall=FALSE,
	 graphics='lattice'
		 )

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
    invisible(retVal)
}

mosaic.getOption <- function (name) 
{
    get("lattice.options", envir = .LatticeEnv)[[name]]
}

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
    invisible(old.mosaic.theme)
}

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
