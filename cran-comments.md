## Test environments

  * local OS X install: 
    * R version 3.4.1 Patched (2017-07-09 r72910)
    * System: x86_64, darwin15.6.0
  
  * win-builder via devtools

## R CMD check results

There were no ERRORs or WARNINGs locally or via Win Builder.

## Downstream dependencies

FDRreg doesn't install in revdep_check() and hasn't been updated on CRAN since 2014-03-05, but my
locally installed version appears to work.  (The package only has two functions and the 
examples for each run with the new version of this package installed.)

Statistical modeling is maintained by a co-author.  He will resubmit once the new mosaic is on CRAN.

Checking packages -----------------------------------------------------------------------------------------
Checking 14 packages: abd, fastR, FDRreg, ggformula, Lock5withR, mdsr, mosaicCore, mosaicData, NHANES, Sleuth2, Sleuth3, statisticalModeling, tigerstats, userfriendlyscience
Package library: /private/var/folders/py/txwd26jx5rq83f4nn0f5fmmm0000gn/T/RtmpC65B6B/revdep12c802a70100c, /private/var/folders/py/txwd26jx5rq83f4nn0f5fmmm0000gn/T/Rtmp5RTFvq/R-lib, /Library/Frameworks/R.framework/Versions/3.4/Resources/library
Checked abd                : 0 errors | 0 warnings | 0 notes
Checked fastR              : 0 errors | 1 warning  | 0 notes
Checked FDRreg             : 1 error  | 0 warnings | 0 notes
Checked ggformula          : 0 errors | 0 warnings | 0 notes
Checked Lock5withR         : 0 errors | 0 warnings | 0 notes
Checked mdsr               : 0 errors | 0 warnings | 1 note 
Checked mosaicCore         : 0 errors | 0 warnings | 0 notes
Checked mosaicData         : 0 errors | 0 warnings | 0 notes
Checked NHANES             : 0 errors | 0 warnings | 0 notes
Checked Sleuth2            : 0 errors | 0 warnings | 0 notes
Checked Sleuth3            : 0 errors | 0 warnings | 0 notes
Checked statisticalModeling: 1 error  | 1 warning  | 0 notes
Checked tigerstats         : 0 errors | 0 warnings | 0 notes
Checked userfriendlyscience: 0 errors | 0 warnings | 0 notes
