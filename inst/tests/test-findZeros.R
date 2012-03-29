

context('Finding Zeros')

test_that("zeros are found", {
  expect_equivalent( round(findZeros(x^2 - 2 ~x),4), round(c(-sqrt(2), sqrt(2)),4) )
  expect_equivalent( round(findZeros(x^2 - 3 ~x),4), round(c(-sqrt(3), sqrt(3)),4) )
})

test_that("zeros are within search interval", {
  expect_true( all( abs(findZeros(sin(1/x) ~ x, near=0, within=4)) < 4) )
})
