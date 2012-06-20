

context('Finding Zeros')

test_that("zeros are found", {
  expect_equivalent( round(findZeros(x^2 - 2 ~x),4), round(c(-sqrt(2), sqrt(2)),4) )
  expect_equivalent( round(findZeros(x^2 - 3 ~x),4), round(c(-sqrt(3), sqrt(3)),4) )
})

test_that("zeros are within search interval", {
  expect_true( all( abs(findZeros(sin(1/x) ~ x, near=0, within=4)) < 4) )
})

test_that("Can find zeros in two variables",{
  Z = findZerosMult(a*x^2-v~a&x, v=8)
  expect_that(Z[1,]$a*Z[1,]$x^2-8, equals(0,tol=.001) )
  
  Z = findZerosMult(a^2+x^2-8~a&x, npts = 1000, sortBy='radial')
  expect_that(Z[46,]$a^2+Z[46,]$x^2-8, equals(0, tol=0.001))
})