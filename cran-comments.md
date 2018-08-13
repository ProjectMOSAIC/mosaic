## Context

This release fixes a few minor bugs, improves some behavior, and moves some functionality
from `mosaic` to `mosaicCore` (on CRAN earlier today).


## Test environments

* local OS X install 
  * R version 3.5.1 (2018-07-02)
  * Platform: x86_64-apple-darwin15.6.0 (64-bit)
  * Running under: macOS High Sierra 10.13.6
  
* win-builder via devtools::build_win()


## R CMD check results


## Reverse dependencies

revdep_check() report:

Checked abd        : 0 errors | 0 warnings | 0 notes
Checked fastR      : 0 errors | 0 warnings | 0 notes
Checked fastR2     : 0 errors | 0 warnings | 0 notes
Checked FDRreg     : 1 error  | 0 warnings | 0 notes *
Checked ggformula  : 0 errors | 0 warnings | 0 notes
Checked Lock5withR : 0 errors | 0 warnings | 0 notes
Checked mdsr       : 0 errors | 0 warnings | 1 note  **
Checked MMAC       : 0 errors | 0 warnings | 0 notes
Checked mosaicCalc : 0 errors | 0 warnings | 0 notes
Checked mosaicCore : 0 errors | 0 warnings | 0 notes
Checked mosaicData : 0 errors | 0 warnings | 0 notes
Checked mosaicModel: 0 errors | 0 warnings | 0 notes
Checked NHANES     : 0 errors | 0 warnings | 0 notes
Checked Sleuth2    : 0 errors | 0 warnings | 0 notes
Checked Sleuth3    : 0 errors | 0 warnings | 0 notes
Checked supernova  : 0 errors | 0 warnings | 0 notes
Checked tigerstats : 0 errors | 0 warnings | 0 notes



* FDRreg doesn't install in revdep_check() and hasn't been updated on CRAN since
2014-03-05.  I can install locally and the examples for the two functions in
the package work.

** The note for `mdsr` is unrelated to `mosaic`.

