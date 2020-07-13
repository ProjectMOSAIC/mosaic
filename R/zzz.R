
.mosaicEnv <- new.env()
assign("mosaic.theme",   list(), envir = .mosaicEnv)
assign("mosaic.options", list(), envir = .mosaicEnv)

.onLoad <- function(libname, pkgname) {
  ## library.dynam("mosaic", pkgname, libname )
 
  pks <- invisible(suppressPackageStartupMessages(
    sapply(pkgs_to_attach, requireNamespace, quietly = TRUE)
  ))
  for (p in pkgs_to_attach) {
    if (! is_attached(p)) suppressPackageStartupMessages(attachNamespace(p))
  }
  
  mosaic.par.set(.defaultMosaicOptions())
}

.onAttach <- function(libname, pkgname) {

  # have histogram use xhistogram stuff by default
  origLatticeOptions <- lattice::lattice.options(
    histogram.breaks = xhistogramBreaks,
    prepanel.default.histogram = prepanel.xhistogram,
    panel.histogram = panel.xhistogram)
  assign("original.lattice.options", origLatticeOptions, envir = .mosaicEnv)
  packageStartupMessage(
    paste0("\nThe 'mosaic' package masks several functions from core packages ",
           "in order to add \nadditional features.  ",
           "The original behavior of these functions should not be affected by this."
           # "\n\nNote: If you use the Matrix package, be sure to load it BEFORE loading mosaic.",
           # "\n\nHave you tried the ggformula package for your plots?"
           ),
    appendLF = TRUE)
}

is_attached <- function(x) {
  paste0("package:", x) %in% search()
}

.noGenerics <- FALSE

.onUnload <- function(libpath) {
    ## library.dynam.unload("mosaic", libpath)
}


pkgs_to_attach <-
  c("lattice", "ggplot2", "Matrix", "dplyr", "ggstance", "ggformula", "mosaicData", "ggridges")
