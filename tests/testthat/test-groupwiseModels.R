context("Groupwise Models")

test_that("basic calculations occur properly", {
  mod <- gwm( width ~ 1, data=mosaicData::KidsFeet)
  expect_that(coef(mod)$model_value, is_equivalent_to(mean(mosaicData::KidsFeet$width)))
  mod2 <- gwm( width ~ sex, data=mosaicData::KidsFeet)
  mod3 <- lm( width ~ sex-1, data=mosaicData::KidsFeet)
  expect_that( coef(mod2)$model_value, is_equivalent_to(coef(mod3)))
})

test_that("coefficient names are correct.",{
  mod <- gwm( width ~ 1, data=mosaicData::KidsFeet)
  expect_that( names(coef(mod)), equals("model_value"))
  mod <- gwm( width ~ sex, data=mosaicData::KidsFeet)
  expect_true( all( names(coef(mod)) == c("sex", "model_value") ) )
  mod <- gwm( width ~ sex + domhand, data = mosaicData::KidsFeet)
  expect_true( all( names(coef(mod)) == c("sex", "domhand", "model_value") ) )
})

test_that("fitted and resids have the right relationship",{
  mod <- gwm( width ~ sex + domhand, data=mosaicData::KidsFeet)
  expect_that( resid(mod) + fitted(mod), equals( mosaicData::KidsFeet$width, tol=0.000001))
})

test_that("gwm works with do",{
  s <- do(10) * gwm(width ~ sex + domhand, data=resample(mosaicData::KidsFeet))
  expect_true( all(s$.index %in%  1:10) )
  expect_that(ncol(s), equals(5))
})

