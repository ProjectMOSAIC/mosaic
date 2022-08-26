## Test environments

* local
    * R version 4.2.1 (2022-06-23)
    * macOS Monterey 12.5.1
    * x86_64, darwin17.0

* win-builder

## Mosaic suite udpates

This is a minor update mainly to 

* move a few tests from this package to mosaicCore (since they were testing 
functions defined there but re-exported from this package.)

* change guide = FALSE to guide = 'none' to avoid deprecation message from ggplot2

* change calls to expect_doppelganger to avoid deprecated path argument

It is being submitted in tandem with the mosaicCore package.