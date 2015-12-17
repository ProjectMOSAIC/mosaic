## Test environments

  * local OS X install: 
    *  R version 3.2.3 Patched (2015-12-10 r69760)
    * Platform: x86_64-apple-darwin13.4.0 (64-bit)
    * Running under: OS X 10.11.1 (El Capitan)
  
  * win-builder (devel and release) via devtools

## R CMD check results

There were no ERRORs or WARNINGs. 

## Downstream dependencies

updated versions of fastR and mosaicData are being submitted in conjunction with this release.

FDRreg doesn't install in revdep_check() and hasn't been updated on CRAN in over 18 months, but my
previously installed version appears to work.  (The package only has two functions and the 
examples for each run with the new version of this package installed.)

In userfriendlyscience, there appears to be errors related to ggplot2.  The maintainer
has been notified by Hadley.  There do not appear to be mosaic-related issues.

