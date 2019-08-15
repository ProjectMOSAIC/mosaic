context("xpnorm")
require(manipulate)
testthat::test_that("xpnorm works", {
  vdiffr::expect_doppelganger("xpnorm1", xpnorm(650, 500, 100)) 
  vdiffr::expect_doppelganger("xpnorm2", xqnorm(.75, 500, 100))
  #vdiffr::expect_doppelganger("xpnorm3", xpnorm(-3:3, return = "plot", system = "gg") %>%
  #                              gf_labs(title = "My Plot", x = "") %>%
  #                              gf_theme(theme_bw()))



  #   manipulate(xpnorm(score, 500, 100, verbose = verbose),
  #     score = slider(200, 800),
  # 	   verbose = checkbox(TRUE, label = "Verbose Output")
  #   )
})