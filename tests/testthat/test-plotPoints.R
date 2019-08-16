context("plotPoints()")
require(mosaicData)
testthat::test_that("plotPoints works", {
  
  vdiffr::expect_doppelganger("plotPoints1", plotPoints( width ~ length, data=KidsFeet, groups=sex, pch=20))
  
  f <- makeFun( lm( width ~ length * sex, data=KidsFeet))
  vdiffr::expect_doppelganger("plotPoints2", plotFun( f(length=length,sex="G")~length, add=TRUE, col="pink"))
  vdiffr::expect_doppelganger("plotPoints3", plotFun( f(length=length,sex="B")~length, add=TRUE))
  
})