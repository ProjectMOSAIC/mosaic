## Test environments

* local OS X install: R version 3.2.2 Patched (2015-09-08 r69333)
* ubuntu 12.04 (on travis-ci), R 3.2.2
* win-builder (devel and release)

## R CMD check results

There were no ERRORs or WARNINGs. 
  
  * Sometimes win-builder throws an error on example(CIAdata) which accesses the CIA 
  world factbook.  I've submitted the same package multiple times and this failure 
  is intermittent.  It does not occur on my local machine.

## Downstream dependencies

I've informed all of the maintainers and rebuilt their packages locally from source, and
checked with devtools::revdep_check().

## Note

We will likely make another CRAN submission relatively soon after testing more thoroughly
and finishing some work we were prepping for a CRAN submission in the near future.  (Some
of these have been included in the current submission, so this is more than just a minimal 
fix of the immediate problems due to changes to ggdendro.)

