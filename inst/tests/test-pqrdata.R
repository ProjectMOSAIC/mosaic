
context('Testing pdata(), qdata(), and rdata() ')


#test_that("formula interface works.", {
#	expect_equivalent( length(rdata( 10, ~age, HELPrct)), 10 )
#	expect_equivalent( pdata(30, ~age, HELPrct), prop(HELPrct$age <= 30) )
#	expect_equivalent( qdata(.75, ~age, HELPrct), quantile(HELPrct$age, .75) )
#	expect_equivalent( qdata(c(.25,.75), ~age, HELPrct), quantile(HELPrct$age, c(.25,.75)) )
#})

test_that("vector/data interface works.", {
	expect_equivalent( length(rdata( 10, age, HELPrct)), 10 )
	expect_equivalent( pdata(30, age, HELPrct), prop(HELPrct$age <= 30) )
	expect_equivalent( qdata(.75, age, HELPrct), quantile(HELPrct$age, .75) )
	expect_equivalent( qdata(c(.25,.75), age, HELPrct), quantile(HELPrct$age, c(.25,.75)) )
})


test_that("vector (no data) interface works.", {
	expect_equivalent( length(rdata( 10, HELPrct$age)), 10 )
	expect_equivalent( pdata(30, HELPrct$age), prop(HELPrct$age <= 30) )
	expect_equivalent( qdata(.75, HELPrct$age), quantile(HELPrct$age, .75) )
	expect_equivalent( qdata(c(.25,.75), HELPrct$age), quantile(HELPrct$age, c(.25,.75)) )
})

#test_that("error messages generated", {
#	expect_error( pdata( 30, ~age | sex, HELPrct) )
#	expect_error( qdata( .75, ~age | sex, HELPrct) )
#	expect_error( rdata( 30, ~age | sex, HELPrct) )
#	expect_error( qdata( 30, ~age, HELPrct) )
#})

