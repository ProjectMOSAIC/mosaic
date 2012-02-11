
context('Tabulation')

test_that("dimensions are correct", {
  expect_equivalent( dim( mtable( ~ sex & substance | homeless, HELPrct ) ), c( 3, 4, 2) )
  expect_equivalent( dim( mtable( ~ sex & substance & homeless, HELPrct ) ), c( 3, 4, 3) )
  expect_equivalent( dim( mtable( ~ sex | substance & homeless, HELPrct ) ), c( 3, 3, 2) )
})

test_that("Proportions/Counts/Percents selected correctly", {
  expect_true( all(mtable( ~ sex & substance | homeless, HELPrct) <= 1) )
  expect_true( all(mtable( ~ sex & substance & homeless, HELPrct) > 5) )
  expect_equivalent( max(mtable( ~ sex & substance & homeless, HELPrct)) , nrow(HELPrct) )
  expect_true( all(mtable( ~ sex & substance | homeless, format='percent', HELPrct) <= 100) )
  expect_equivalent( 100 * mtable( ~ sex & substance | homeless, format='proportion', HELPrct),
                          mtable( ~ sex & substance | homeless, format='percent', HELPrct))
})

test_that("Subsetting works", {
  expect_equivalent( 
	mtable( ~ substance & homeless, HELPrct, subset=sex=='male' ) + 
	mtable( ~ substance & homeless, HELPrct, subset=sex=='female' ),
	mtable( ~ substance & homeless, HELPrct) 
  )
})
