#! /bin/bash
export _R_CHECK_FORCE_SUGGESTS_=false
do2all "R CMD check %p" *_*.tar.gz
