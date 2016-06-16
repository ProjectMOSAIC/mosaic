## Test environments

  * local OS X install: 
    * R version 3.3.0 Patched (2016-06-02 r70706)
    * Platform: x86_64-apple-darwin13.4.0 (64-bit)
    * Running under: OS X 10.11.5 (El Capitan)
  
  * win-builder via devtools

## R CMD check results

There were no ERRORs or WARNINGs. 

## Downstream dependencies

FDRreg doesn't install in revdep_check() and hasn't been updated on CRAN since 2014-03-05, but my
locally installed version appears to work.  (The package only has two functions and the 
examples for each run with the new version of this package installed.)

Checking packages -------------------------------------------
Checked abd                : 0 errors | 0 warnings | 0 notes
Checked fastR              : 0 errors | 0 warnings | 0 notes
Checked FDRreg             : 1 error  | 0 warnings | 0 notes
Checked Lock5withR         : 0 errors | 0 warnings | 0 notes
Checked mosaicData         : 0 errors | 0 warnings | 0 notes
Checked NHANES             : 0 errors | 0 warnings | 0 notes
Checked Sleuth2            : 0 errors | 0 warnings | 0 notes
Checked Sleuth3            : 0 errors | 0 warnings | 0 notes
Checked tigerstats         : 0 errors | 0 warnings | 0 notes
Checked userfriendlyscience: 0 errors | 0 warnings | 0 notes

