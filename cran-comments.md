## Test environments

* local

  * R version 4.0.3 (2020-10-10)
  * Platform: x86_64-apple-darwin17.0 (64-bit)
  * Running under: macOS Catalina 10.15.6
  * devtools::check(env_vars = c(`_R_CHECK_DEPENDS_ONLY_` = "true"))

* win-builder isn't sending back emails the last couple days -- not sure why

## Mosaic suite udpates

This is a minor update to address the use of vdiffr and order() applied to a data frame, both per CRAN request.


