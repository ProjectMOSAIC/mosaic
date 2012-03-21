
context('Tabulation')

test_that("dimensions are correct", {
  expect_equivalent( dim( tally( ~ sex & substance | homeless, HELPrct ) ), c( 3, 4, 2) )
  expect_equivalent( dim( tally( ~ sex & substance & homeless, HELPrct ) ), c( 3, 4, 3) )
  expect_equivalent( dim( tally( ~ sex | substance & homeless, HELPrct ) ), c( 3, 3, 2) )
})

test_that("Proportions/Counts/Percents selected correctly", {
  expect_true( all(tally( ~ sex & substance | homeless, HELPrct) <= 1) )
  expect_true( all(tally( ~ sex & substance & homeless, HELPrct) > 5) )
  expect_equivalent( max(tally( ~ sex & substance & homeless, HELPrct)) , nrow(HELPrct) )
  expect_true( all(tally( ~ sex & substance | homeless, format='percent', HELPrct) <= 100) )
  expect_equivalent( 100 * tally( ~ sex & substance | homeless, format='proportion', HELPrct),
                          tally( ~ sex & substance | homeless, format='percent', HELPrct))
})

test_that("Subsetting works", {
  expect_equivalent( 
	tally( ~ substance & homeless, HELPrct, subset=sex=='male' ) + 
	tally( ~ substance & homeless, HELPrct, subset=sex=='female' ),
	tally( ~ substance & homeless, HELPrct) 
  )
})
