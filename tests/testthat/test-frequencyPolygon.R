require(mosaic)
require(mosaicData)
# Examples
# freqpolygon(~age | substance, data=HELPrct, v=35)
# freqpolygon(~age, data=HELPrct, labels=TRUE, type='count')
# freqpolygon(~age | substance, data=HELPrct, groups=sex)
# freqpolygon(~age | substance, data=HELPrct, groups=sex, ylim=c(0,0.11))
testthat::test_that("Returns a trellis object",{
})