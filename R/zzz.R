
.mosaicEnv <- new.env()
assign("mosaic.theme",   list(), envir = .mosaicEnv)
assign("mosaic.options", list(), envir = .mosaicEnv)

.onLoad <- function(libname, pkgname) {
    ## library.dynam("mosaic", pkgname, libname )
    mosaic.par.set(.defaultMosaicOptions())
}

.onAttach <- function(libname, pkgname) {
	# have histogram use xhistogram stuff by default
  lattice::lattice.options(
    histogram.breaks = xhistogramBreaks,
    prepanel.default.histogram = prepanel.xhistogram,
    panel.histogram = panel.xhistogram)
}

.noGenerics <- FALSE

.onUnload <- function(libpath) {
    ## library.dynam.unload("mosaic", libpath)
}


