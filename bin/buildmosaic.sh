#! /bin/bash
export _R_CHECK_FORCE_SUGGESTS_=false
bin/roxy -p "." --clean
R CMD build .
