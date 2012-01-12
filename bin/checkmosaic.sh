#! /bin/bash
export _R_CHECK_FORCE_SUGGESTS_=false
mv mosaic_0*.gz builds/
bin/buildmosaic.sh
R CMD check mosaic_0.3*gz
