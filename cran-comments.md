## Test environments

  * local OS X install: R version 3.2.2 Patched (2015-10-06 r69484)
  
  * win-builder (devel and release) via devtools

## R CMD check results

There were no ERRORs or WARNINGs. 

## Downstream dependencies

I've informed all of the maintainers and checked with devtools::revdep_check().  

FDRreg doesn't install in revdep_check(), but does when I install manually.

In userfriendlyscience, there appears to be an error passing unknown arguments 
to a ggplot2 geom (Error: Unknown parameters: size)  This does not appear to be 
related to the mosaic package.

