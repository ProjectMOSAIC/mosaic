
## Submission notes

This is a relatively minor update to the package, primarily to aid the CRAN
submission of ggdendro. Changes to that package had breaking changes in some 
of our tests. We've updated our tests to match the github version of ggdendro.
All tests pass with that version on our test systems. We expect ggdendro to
be submitted to CRAN in the next day or two.

We made one additional change to repair a small bug.

## Test environments

* local
    * R version 4.2.1 (2022-06-23)
    * macOS Ventura 13.5
    * x86_64, darwin17.0

* win-builder

    * win builder complains about <https://www.tandfonline.com/doi/full/10.1080/00031305.2015.1094283> which is a redirect from <https://doi.org/10.1080/00031305.2015.1094283>, but both of these work for me.
