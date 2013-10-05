#! /bin/bash
export _R_CHECK_FORCE_SUGGESTS_=false
bin/roxy -p "." --clean
mv *_*.tar.gz builds/
#\cp sandbox/Vignettes/Resampling/Resampling.pdf inst/doc/Resampling.pdf
#\cp sandbox/Vignettes/Calculus/mosaic-calculus.pdf inst/doc/Calculus.pdf
#\cp sandbox/Vignettes/MinimalR/MinimalR.pdf inst/doc/MinimalR.pdf
#\cp sandbox/Vignettes/Links/Links.pdf inst/doc/Links.pdf
R-devel CMD build --resave-data .
bin/do2all "R-devel CMD check --as-cran %p" *_*.tar.gz
