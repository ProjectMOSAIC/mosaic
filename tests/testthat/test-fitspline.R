context("fitSpline()")

testthat::test_that("fitSpline works", {
  
  f <- fitSpline( weight ~ height, data=women, df=5 )
  g <- fitSpline( length ~ width, data = KidsFeet, type='natural', df=5 )
  h <- fitSpline( length ~ width, data = KidsFeet, type='linear', df=5 )
  
  vdiffr::expect_doppelganger("fitspline1", xyplot( weight ~ height, data=women ))
  vdiffr::expect_doppelganger("fitspline2", plotFun(f(height) ~ height, add=TRUE))
  
  xyplot( length ~ width, data = KidsFeet, col='gray70', pch=16)
  
  vdiffr::expect_doppelganger("fitspline3", plotFun(g, add=TRUE, col='navy'))
  vdiffr::expect_doppelganger("fitspline4", plotFun(h, add=TRUE, col='red'))
  
  
})







