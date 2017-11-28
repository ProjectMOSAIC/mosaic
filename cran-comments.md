## Context

This is a minor revision primarily motivated by the need to repond to a change in `formals()` and 
the desire to respond to a few feature requests/bug reports.

## Test environments

  * local OS X install: 
    * R version 3.4.2 (2017-09-28)
    * Platform: x86_64-apple-darwin15.6.0 (64-bit)
  
  * win-builder via devtools

## R CMD check results

There were no ERRORs or WARNINGs locally or via Win Builder.

## Reverse dependencies

revdep_check() report:

Checked abd                : 0 errors | 0 warnings | 0 notes
Checked fastR              : 0 errors | 0 warnings | 0 notes
Checked fastR2             : 0 errors | 0 warnings | 0 notes
Checked FDRreg             : 1 error  | 0 warnings | 0 notes   **
Checked ggformula          : 0 errors | 0 warnings | 0 notes
Checked Lock5withR         : 0 errors | 0 warnings | 0 notes
Checked mdsr               : 0 errors | 0 warnings | 1 note    *
Checked mosaicCalc         : 0 errors | 0 warnings | 0 notes
Checked mosaicCore         : 1 error  | 0 warnings | 0 notes   **
Checked mosaicData         : 0 errors | 0 warnings | 0 notes
Checked mosaicModel        : 0 errors | 0 warnings | 0 notes
Checked NHANES             : 0 errors | 0 warnings | 0 notes
Checked Sleuth2            : 0 errors | 0 warnings | 0 notes
Checked Sleuth3            : 0 errors | 0 warnings | 0 notes
Checked statisticalModeling: 1 error  | 1 warning  | 0 notes   **
Checked tigerstats         : 0 errors | 0 warnings | 0 notes


FDRreg doesn't install in revdep_check() and hasn't been updated on CRAN since
2014-03-05, but my locally installed version appears to work.  (The package only
has two functions and the examples for each run with the new version of this
package installed.)

I can't recreate the issue with `mosaicCore`.  Also, `mosaicCore` was just submitted to CRAN and checked cleanly both locally and on win-builder.  All tests passing on CRAN currently, when tesing `mosaicCore` with win-builder and locally with the new versions of both packages:

==> devtools::test()

Loading mosaicCore
Loading required package: testthat
Testing mosaicCore
✔ | OK F W S | Context
✔ | 23       | df_stats() [1.9 s]

══ Results ══════════════════════════════════════════════════════════════════
Duration: 2.1 s

OK:       23
Failed:   0
Warnings: 0
Skipped:  0

The issues reported for `statisticalModleing` are already present on CRAN and not related to changes in `mosaic`.  I've informed the author of the issues reported. 

The note for `mdsr` is unrelated to `mosaic`.

