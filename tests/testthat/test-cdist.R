# testthat::test_that("cdist works", {
#   
#   testcase1 <- c(-1.95996398454005, 1.95996398454005)
#   testcase2 <- c(-4.03214298355523, -2.57058183563631, -2.01504837333302, 2.01504837333302, 
#                  2.57058183563631, 4.03214298355523)
#   testcase3 <- c(-2.67779327094084, -2.00855911210076, -1.6759050251631, 1.6759050251631, 
#                  2.00855911210076, 2.67779327094084)
#   testcase4 <- c(-3.18244630528371, 2.57058183563631, -2.22813885198627, 2.08596344726586
#   )
#   testcase5 <- c(304.003601545995, 695.996398454005)
#   testcase6 <- c(0.215795282623898, 0.351846317749271, 7.81472790325118, 9.34840360449614
#   )
#   testcase7 <- c(-2.07387306790403, 2.07387306790403)
#   testcase9 <- structure(list(`mean of dfexample` = 9.79992935127334, lower = 8.94278258043651, 
#                               upper = 10.6570761221102, level = 0.95), class = "data.frame", row.names = c(NA, 
#                                                                                                            -1L))
#   testcase10 <- c(-2.07387306790403, 2.07387306790403)
#   # expect_equivalent(testcase1, cdist( "norm", .95))
#   # expect_equivalent(testcase2, cdist( "t", c(.90, .95, .99), df=5))
#   # expect_equivalent(testcase3, cdist( "t", c(.90, .95, .99), df=50))
#   # expect_equivalent(testcase4, cdist( "t", .95, df=c(3,5,10,20), plot = FALSE))
#   # expect_equivalent(testcase5, cdist( "norm", .95, mean=500, sd=100 ))
#   # expect_equivalent(testcase6, cdist( "chisq", c(.90, .95), df=3 ))
#   # expect_equivalent(testcase7, cdist("t", p = 0.95, df=22))
#   # expect_equivalent(testcase9, confint(t.test(dfexample)))
#   # expect_equivalent(testcase10, cdist("t", p = 0.95, df=22, verbose = TRUE))
#   
#   #vdiffr::expect_doppelganger("cdist1", cdist( "norm", .95))
#   #vdiffr::expect_doppelganger("cdist2", cdist( "t", c(.90, .95, .99), df=5))
#   #vdiffr::expect_doppelganger("cdist3", cdist( "t", c(.90, .95, .99), df=50))
#   #vdiffr::expect_doppelganger("cdist5", cdist( "norm", .95, mean=500, sd=100 ))
#   #vdiffr::expect_doppelganger("cdist6", cdist( "chisq", c(.90, .95), df=3 ))
#   #vdiffr::expect_doppelganger("cdist7", cdist("t", p = 0.95, df=22))
#   #vdiffr::expect_doppelganger("cdist8", mean(dfexample) + cdist("t", p = 0.95, df=22) * sd(x) / sqrt(23))
#   #vdiffr::expect_doppelganger("cdist10", cdist("t", p = 0.95, df=22, verbose = TRUE))
#   
# })