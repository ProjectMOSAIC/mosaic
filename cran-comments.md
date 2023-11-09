
## Submission notes

This is a relatively minor update to the package. The main issues addressed are

* removal of dependence on maptools and rgeos
* migration to |> from %>%
* migration away from some deprecated tidyverse/ggplot2 functions
* removal of ggplot2::aes_string()
* updating a few URLs
* fixing a few minor bugs
* migration from citEntry() to bibentry()
* update to edition 3 of testthat

We are submitting four packages roughly in parallel:

* mosaicCore [already on CRAN]
* mosaicData [already on CRAN]
* ggformula [already on CRAN]
* mosaic

## Test environments

* local
    * R version 4.2.1 (2022-06-23)
    * macOS Ventura 13.5
    * x86_64, darwin17.0

* win-builder

    * win builder complains about <https://www.tandfonline.com/doi/full/10.1080/00031305.2015.1094283> which is a redirect from <https://doi.org/10.1080/00031305.2015.1094283>, but both of these work for me.
    
## revdep check

*Wow, no problems at all. :)*
