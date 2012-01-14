#! /bin/bash
export _R_CHECK_FORCE_SUGGESTS_=false
R CMD check *_.tar.gz
