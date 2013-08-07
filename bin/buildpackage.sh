#! /bin/bash
export _R_CHECK_FORCE_SUGGESTS_=false
bin/roxy -p "." --clean
mv *_*.tar.gz builds/
\cp vignettes/Resampling/Resampling.pdf inst/doc/vignettes/Resampling.pdf
\cp vignettes/Calculus/mosaic-calculus.pdf inst/doc/vignettes/Calculus.pdf
\cp vignettes/MinimalR/MinimalR.pdf inst/doc/vignettes/MinimalR.pdf
R CMD build --resave-data .
