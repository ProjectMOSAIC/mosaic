#! /bin/bash
export _R_CHECK_FORCE_SUGGESTS_=false
bin/roxy -p "." --clean
mv *_*.tar.gz builds/
\cp vignettes-source/Resampling/Resampling.pdf vignettes/Resampling.pdf
\cp vignettes-source/Calculus/mosaic-calculus.pdf vignettes/Calculus.pdf
\cp vignettes-source/MinimalR/MinimalR.pdf vignettes/MinimalR.pdf
\cp vignettes-source/Links/Links.pdf vignettes/Links.pdf
R CMD build --resave-data .
