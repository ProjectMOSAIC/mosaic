
context('Testing pdata(), qdata(), and rdata() ')


test_that("formula interface works.", {
	expect_equivalent( length(rdata( 10, ~age, mosaicData::HELPrct)), 10 )
	expect_equivalent( pdata(30, ~age, mosaicData::HELPrct), prop( ~ mosaicData::HELPrct$age <= 30) )
	expect_equivalent( qdata(.75, ~age, mosaicData::HELPrct)['quantile'], quantile(mosaicData::HELPrct$age, .75) )
	expect_equivalent( qdata(c(.25,.75), ~age, mosaicData::HELPrct)$quantile, quantile(mosaicData::HELPrct$age, c(.25,.75)) )
})

test_that("vector/data interface works.", {
	expect_equivalent( length(rdata( 10, age, mosaicData::HELPrct)), 10 )
	expect_equivalent( pdata(30, age, data=mosaicData::HELPrct), prop(~ mosaicData::HELPrct$age <= 30) )
	expect_equivalent( qdata(.75, age, data=mosaicData::HELPrct)['quantile'], quantile(mosaicData::HELPrct$age, .75) )
	expect_equivalent( qdata(c(.25,.75), age, mosaicData::HELPrct)$quantile, quantile(mosaicData::HELPrct$age, c(.25,.75)) )
})


test_that("vector (no data) interface works.", {
	expect_equivalent( length(rdata( 10, mosaicData::HELPrct$age)), 10 )
	expect_equivalent( pdata(30, mosaicData::HELPrct$age), prop( ~ mosaicData::HELPrct$age <= 30) )
	expect_equivalent( qdata(.75, mosaicData::HELPrct$age)['quantile'], quantile(mosaicData::HELPrct$age, .75) )
	expect_equivalent( qdata(c(.25,.75), mosaicData::HELPrct$age)$quantile, quantile(mosaicData::HELPrct$age, c(.25,.75)) )
})

#test_that("error messages generated", {
#	expect_error( pdata( 30, ~age | sex, mosaicData::HELPrct) )
#	expect_error( qdata( .75, ~age | sex, mosaicData::HELPrct) )
#	expect_error( rdata( 30, ~age | sex, mosaicData::HELPrct) )
#	expect_error( qdata( 30, ~age, mosaicData::HELPrct) )
#})

