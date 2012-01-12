#! /bin/bash
export _R_CHECK_FORCE_SUGGESTS_=false
bin/roxy --path "." --clean
R CMD build .
