
.mosaicEnv <- new.env()
assign("mosaic.theme",   list(), envir = .mosaicEnv)
assign("mosaic.options", list(), envir = .mosaicEnv)

.onLoad <- function(libname, pkgname) 
{
    ## library.dynam("mosaic", pkgname, libname )
    mosaic.par.set(.defaultMosaicOptions())
}

.noGenerics <- FALSE

.onUnload <- function(libpath)
    ## library.dynam.unload("mosaic", libpath)


## If present, .First.lib will be used if the NAMESPACE file is
## missing.  This is useful during development, thanks to C-c C-l in
## Emacs/ESS. It won't be used if a NAMESPACE file is present.  Note:


.First.lib <- function(lib, pkg) 
{
    cat(gettext("Note: you shouldn't be seeing this message unless\nyou are using a non-standard version of mosaic"),
        fill = TRUE)
    library.dynam("mosaic", pkg, lib )
    ## having the next line causes a warning from R CMD check
    ## if (!require("grid")) stop("The grid package couldn't be loaded.\nPlease check your installation of R")
    mosaic.par.set(.defaultMosaicOptions())
}


