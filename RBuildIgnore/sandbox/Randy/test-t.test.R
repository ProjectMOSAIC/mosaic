

context('t.test')

test_that("get same results as in stats", {
	expect_equivalent( 
		t.test( ~ mother + father, data=Galton),
		stats:::t.test(Galton$mother, Galton$father) )
	expect_equivalent( 
		t.test( age ~ sex, data=HELPrct),
		stats:::t.test(age ~ sex, data=HELPrct) )
	expect_equivalent( 
		t.test( ~ age | sex, data=HELPrct),
		t.test(age ~ sex, data=HELPrct) )
	expect_equivalent( 
		t.test( ~ age, data=HELPrct),
		stats:::t.test(HELPrct$age) )
	expect_equivalent( 
		t.test( ~ age, data=HELPrct),
		t.test(HELPrct$age) )
})

test_that("warnings and errors generated", {
	expect_error( t.test( age ~ substance, HELPrct ) )
	expect_error( t.test( ~ age | substance, HELPrct ) )
})

