

context('evalFormula')


test_that("subset works ", {
	expect_equivalent( 
		evalFormula( age ~ sex, data=HELPrct, age > 50 )[1:3],
		evalFormula( age ~ sex, data=subset(HELPrct, age > 50))[1:3] )
})

