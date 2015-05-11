
context('Environment Checks (with(), etc.)')

test_that("with() works in place of data = ",{
  expect_equivalent( mean(~length, data=mosaicData::KidsFeet), with(mosaicData::KidsFeet, mean(length)) )
  expect_equivalent( sd(~length, data=mosaicData::KidsFeet), with(mosaicData::KidsFeet, sd(length)) )
  expect_equivalent( diffmean(length ~ sex, data=mosaicData::KidsFeet), with(mosaicData::KidsFeet, diffmean(length ~ sex)) )
  expect_equivalent( diffprop(domhand ~ sex, data=mosaicData::KidsFeet), with(mosaicData::KidsFeet, diffprop(domhand ~ sex)) )
  expect_equivalent( t.test(length ~ sex, data=mosaicData::KidsFeet), with(mosaicData::KidsFeet, t.test(length ~ sex)) )
})
