# to satisfy CRAN checks about conditional use of suggested packages
# code suggested by Lionel Henry

wrapped_expect_doppelganger <- function(title, fig, path = NULL, ...) {
  testthat::skip_if_not_installed("vdiffr")
  wrapped_expect_doppelganger(title, fig, path = path, ...)
}
