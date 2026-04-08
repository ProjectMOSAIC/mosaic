
## Submission notes

The primary reason for this update is in response to the recent changes to rlang.

Maintainer email is changing from <rpruim@calvin.edu> to <rpruim@gmail.com>.

## Test environments

* local
  * R version 4.5.1 (2025-06-13)
  * os: macOS Sonoma 14.4
  * system: aarch64, darwin23.6.0

* WinBuilder
  * `devtools::check_win_devel()`
  * `devtools::check_win_release()`

  * win builder complains about <https://www.tandfonline.com/doi/full/10.1080/00031305.2015.1094283> which is a redirect from <https://doi.org/10.1080/00031305.2015.1094283>, but both of these work for me.
