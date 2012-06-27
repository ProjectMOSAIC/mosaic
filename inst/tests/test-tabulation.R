
context('Tabulation')

test_that("dimensions are correct", {
  expect_equivalent( dim( tally( ~ sex & substance | homeless, HELPrct ) ), c( 3, 4, 2) )
  expect_equivalent( dim( tally( ~ sex & substance & homeless, HELPrct ) ), c( 3, 4, 3) )
  expect_equivalent( dim( tally( ~ sex | substance & homeless, HELPrct ) ), c( 3, 3, 2) )
  expect_equivalent( dim( tally( ~ sex + substance | homeless, HELPrct ) ), c( 3, 4, 2) )
  expect_equivalent( dim( tally( ~ sex + substance + homeless, HELPrct ) ), c( 3, 4, 3) )
  expect_equivalent( dim( tally( ~ sex | substance + homeless, HELPrct ) ), c( 3, 3, 2) )
})

test_that("Proportions/Counts/Percents selected correctly", {
  expect_true( all(tally( ~ sex & substance | homeless, HELPrct) <= 1) )
  expect_true( all(tally( ~ sex & substance & homeless, HELPrct) > 5) )
  expect_equivalent( max(tally( ~ sex & substance & homeless, HELPrct)) , nrow(HELPrct) )
  expect_true( all(tally( ~ sex & substance | homeless, format='percent', HELPrct) <= 100) )
  expect_equivalent( 100 * tally( ~ sex & substance | homeless, format='proportion', HELPrct),
                          tally( ~ sex & substance | homeless, format='percent', HELPrct))
  expect_true( all(tally( ~ sex + substance | homeless, HELPrct) <= 1) )
  expect_true( all(tally( ~ sex + substance + homeless, HELPrct) > 5) )
  expect_equivalent( max(tally( ~ sex + substance + homeless, HELPrct)) , nrow(HELPrct) )
  expect_true( all(tally( ~ sex + substance | homeless, format='percent', HELPrct) <= 100) )
  expect_equivalent( 100 * tally( ~ sex + substance | homeless, format='proportion', HELPrct),
                          tally( ~ sex + substance | homeless, format='percent', HELPrct))
})

test_that("Subsetting works", {
  expect_equivalent( 
	tally( ~ substance & homeless, HELPrct, subset=sex=='male' ) + 
	tally( ~ substance & homeless, HELPrct, subset=sex=='female' ),
	tally( ~ substance & homeless, HELPrct) 
  )
  expect_equivalent( 
	tally( ~ substance + homeless, HELPrct, subset=sex=='male' ) + 
	tally( ~ substance + homeless, HELPrct, subset=sex=='female' ),
	tally( ~ substance + homeless, HELPrct) 
  )
})

test_that("errors generated for bad formula types", {
  expect_error( prop(sex~homeless&substance, data=HELPrct))
  expect_error( count(sex~homeless&substance, data=HELPrct))
  expect_error( perc(sex~homeless&substance, data=HELPrct))
  expect_error( prop(sex~homeless+substance, data=HELPrct))
  expect_error( count(sex~homeless+substance, data=HELPrct))
  expect_error( perc(sex~homeless+substance, data=HELPrct))
})

test_that("count/perc/prop wrappers work", {
  expect_equivalent( count(~sex, data=HELPrct), sum(HELPrct$sex == 'female') )
  expect_equivalent( prop(~sex, data=HELPrct), sum(HELPrct$sex == 'female') / nrow(HELPrct))
  expect_equivalent( perc(~sex, data=HELPrct), 100 * sum(HELPrct$sex == 'female') / nrow(HELPrct))
})
