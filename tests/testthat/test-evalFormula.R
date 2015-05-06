

context('evalFormula')


test_that("subset works ", {
	expect_equivalent( 
		evalFormula( age ~ sex, data=mosaicData::HELPrct, age > 50 )[1:3],
		evalFormula( age ~ sex, data=subset(mosaicData::HELPrct, age > 50))[1:3] )
})

