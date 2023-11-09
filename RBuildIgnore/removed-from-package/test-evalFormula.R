

# context('evalFormula()')


test_that("subset works ", {
	expect_equal(ignore_attr = TRUE,  
		evalFormula( age ~ sex, data=mosaicData::HELPrct, age > 50 )[1:3],
		evalFormula( age ~ sex, data=subset(mosaicData::HELPrct, age > 50))[1:3] )
})

