## Test environments

* local

  * R version 4.0.3 (2020-10-10)
  * Platform: x86_64-apple-darwin17.0 (64-bit)
  * Running under: macOS Catalina 10.15.6
  * devtools::check(env_vars = c(`_R_CHECK_DEPENDS_ONLY_` = "true"))

* win builder

## Mosaic suite udpates

This is a minor update mainly to address the use of vdiffr and order() applied to a
data frame, both per CRAN request. We've also dealt with the broken URLs.


