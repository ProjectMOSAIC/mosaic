## Context

This is a minor revision primarily motivated by the need to repond to a change in `formals()` and 
the desire to respond to a few feature requests/bug reports.

## Test environments

* local OS X install 
  * R version 3.5.0 (2018-04-23)
  * Platform: x86_64-apple-darwin15.6.0 (64-bit)
  * Running under: macOS High Sierra 10.13.4
  
* win-builder via devtools::build_win()

## R CMD check results

There were no ERRORs or WARNINGs locally.  One note about size (5.1Mb) seems to come
and go.  I seem to be riding the edge there.  I'll see if I can find any more places 
to trim (or move some more things into `mosaicCore`) in the next round of submissions,
which needs to happen before `ggplot2` is released next month (as I found out yesterday).

## Reverse dependencies

revdep_check() report:

Checked abd        : 0 errors | 0 warnings | 0 notes
Checked fastR      : 0 errors | 0 warnings | 0 notes
Checked fastR2     : 0 errors | 0 warnings | 0 notes
Checked FDRreg     : 1 error  | 0 warnings | 0 notes **
Checked ggformula  : 0 errors | 0 warnings | 0 notes
Checked Lock5withR : 0 errors | 0 warnings | 0 notes
Checked mdsr       : 0 errors | 0 warnings | 1 note  ***
Checked mosaicCalc : 0 errors | 0 warnings | 0 notes
Checked mosaicCore : 1 error  | 0 warnings | 0 notes *
Checked mosaicData : 0 errors | 0 warnings | 0 notes
Checked mosaicModel: 0 errors | 0 warnings | 0 notes
Checked NHANES     : 0 errors | 0 warnings | 0 notes
Checked Sleuth2    : 0 errors | 0 warnings | 0 notes
Checked Sleuth3    : 0 errors | 0 warnings | 0 notes
Checked supernova  : 0 errors | 0 warnings | 0 notes
Checked tigerstats : 0 errors | 0 warnings | 0 notes


* mosaicCore error should go away when `mosaic`, `mosaicCore`, and `ggformula` have all been updated.  (Local checks all pass with versions being submitted.)

** FDRreg doesn't install in revdep_check() and hasn't been updated on CRAN since
2014-03-05, but my locally installed version appears to work.  (The package only
has two functions and the examples for each run with the new version of this
package installed.)

*** The note for `mdsr` is unrelated to `mosaic`.

