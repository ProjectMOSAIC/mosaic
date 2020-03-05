## Context

This release fixes a few minor bugs, improves some behavior, and updates some
of the documentation.


## Test environments

* local OS X install
  * R version 3.6.2 (2019-12-12)
  * Platform: x86_64-apple-darwin15.6.0 (64-bit)
  * Running under: macOS Catalina 10.15.3
  
* win-builder via devtools::check_win_release()
* win-builder via devtools::check_win_devel()


## R CMD check results

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

R CMD check succeeded

## Reverse dependencies

revdepcheck::revdep_check() report:

✔ abd 0.2-8                              ── E: 0     | W: 0     | N: 0                
✔ fastR 0.10.3                           ── E: 0     | W: 0     | N: 0                
✔ fastR2 1.2.1                           ── E: 0     | W: 0     | N: 0                
I FDRreg 0.1                             ── E: 1     | W: 0     | N: 0                
✔ ggformula 0.9.0                        ── E: 0     | W: 0     | N: 1                
✔ Lock5withR 1.2.2                       ── E: 0     | W: 0     | N: 0                
✔ mdsr 0.1.6                             ── E: 0     | W: 0     | N: 2                
✔ MMAC 0.1.2                             ── E: 0     | W: 0     | N: 0                
✔ mosaicCalc 0.5.0                       ── E: 0     | W: 0     | N: 0                
✔ mosaicCore 0.6.0                       ── E: 0     | W: 0     | N: 0                
✔ mosaicData 0.17.0                      ── E: 0     | W: 0     | N: 1                
✔ mosaicModel 0.3.0                      ── E: 0     | W: 0     | N: 1                
✔ NHANES 2.1.0                           ── E: 0     | W: 0     | N: 0                
✔ Sleuth2 2.0-4                          ── E: 0     | W: 0     | N: 0                
✔ Sleuth3 1.0-2                          ── E: 0     | W: 0     | N: 0                
✔ splinetree 0.1.1                       ── E: 0     | W: 0     | N: 0                
✔ supernova 1.1                          ── E: 0     | W: 0     | N: 0                
✔ tigerstats 0.3                         ── E: 0     | W: 0     | N: 0 

OK: 17                                                                              
BROKEN: 1

* FDRreg doesn't install in revdep_check() and hasn't been updated on CRAN since
2014-03-05.  I can install locally and the examples for the two functions in
the package work.


