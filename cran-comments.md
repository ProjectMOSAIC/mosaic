
## Submission notes

The primary reason for this update is in response to the recent changes to rlang. 
Sorry this took longer than I originally hoped.

Also fixed a small bug in mosaic::var() where na.rm was not being passed along in some cases.

Maintainer email is changing from <rpruim@calvin.edu> to <rpruim@gmail.com>.

A previous submission failed automated CRAN checks.  I've udpated R and my packages locally, and I've checked with WinBuilder (devel and release) and I'm not seeing any of the problems that were reported at https://cran.r-project.org/web/checks/check_results_mosaic.html.

## Test environments

* local
  * R version 4.6.0
  * os: macOS Sonoma 14.4
  * system: aarch64, darwin23.6.0

* WinBuilder
  * `devtools::check_win_devel()`
  * `devtools::check_win_release()`

  * win builder has sometimes complained about <https://www.tandfonline.com/doi/full/10.1080/00031305.2015.1094283> which is a redirect from <https://doi.org/10.1080/00031305.2015.1094283>, but both of these work for me.
