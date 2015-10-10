
context('do()')

test_that("naming as expected", {
  expect_equivalent( names( do(2) * mean(~cesd, data=mosaicData::HELPrct)), "mean")
  expect_equivalent( names( do(2) * var(~cesd, data=mosaicData::HELPrct)), "var")
  expect_equivalent( names( do(2) * {var(~cesd, data=mosaicData::HELPrct)}), "result")
  expect_equivalent( names( do(2) * 5 + 3), "result")
  expect_equivalent( names( do(2) * lm(cesd ~ age + sex, data=mosaicData::HELPrct)), 
     c("Intercept", "age", "sexmale", "sigma", "r.squared", "F", "numdf", "dendf"))
})


test_that("dimension as expected", {
  expect_equivalent( dim( do(2) * mean(~cesd, data=mosaicData::HELPrct)), c(2,1))
  expect_equivalent( dim( do(2) * var(~cesd, data=mosaicData::HELPrct)), c(2,1))
  expect_equivalent( dim( do(2) * {var(~cesd, data=mosaicData::HELPrct)}), c(2,1))
  expect_equivalent( dim( do(2) * 5 + 3), c(2,1))
  expect_equivalent( dim( do(2) * lm(cesd ~ age + sex, data=mosaicData::HELPrct)), 
                      c(2,8))
  expect_equivalent( dim( do(2) * anova(lm(cesd ~ age + sex, data=mosaicData::HELPrct))), 
                      c(6,8))
})