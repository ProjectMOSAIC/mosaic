#! /bin/bash
export _R_CHECK_FORCE_SUGGESTS_=false
bin/roxy -p "." --clean
mv *_*.tar.gz builds/
R CMD build --resave-data .
