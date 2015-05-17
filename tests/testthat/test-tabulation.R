
context('Tabulation')

test_that("dimensions are correct", {
  expect_equivalent( dim( tally( ~ sex & substance | homeless, mosaicData::HELPrct, margins=TRUE ) ), c( 3, 4, 2) )
  expect_equivalent( dim( tally( ~ sex & substance & homeless, mosaicData::HELPrct, margins=TRUE ) ), c( 3, 4, 3) )
  expect_equivalent( dim( tally( ~ sex | substance & homeless, mosaicData::HELPrct, margins=TRUE ) ), c( 3, 3, 2) )
  expect_equivalent( dim( tally( ~ sex + substance | homeless, mosaicData::HELPrct, margins=TRUE ) ), c( 3, 4, 2) )
  expect_equivalent( dim( tally( ~ sex + substance + homeless, mosaicData::HELPrct, margins=TRUE ) ), c( 3, 4, 3) )
  expect_equivalent( dim( tally( ~ sex | substance + homeless, mosaicData::HELPrct, margins=TRUE ) ), c( 3, 3, 2) )
  expect_equivalent( dim( tally( ~ sex & substance | homeless, mosaicData::HELPrct) ), c( 2, 3, 2) )
  expect_equivalent( dim( tally( ~ sex & substance & homeless, mosaicData::HELPrct) ), c( 2, 3, 2) )
  expect_equivalent( dim( tally( ~ sex | substance & homeless, mosaicData::HELPrct) ), c( 2, 3, 2) )
  expect_equivalent( dim( tally( ~ sex + substance | homeless, mosaicData::HELPrct) ), c( 2, 3, 2) )
  expect_equivalent( dim( tally( ~ sex + substance + homeless, mosaicData::HELPrct) ), c( 2, 3, 2) )
  expect_equivalent( dim( tally( ~ sex | substance + homeless, mosaicData::HELPrct) ), c( 2, 3, 2) )
})

test_that("Proportions/Counts/Percents selected correctly", {
  expect_true( all(tally( ~ sex & substance | homeless, data=mosaicData::HELPrct) > 5) )
  expect_true( all(tally( ~ sex & substance & homeless, data=mosaicData::HELPrct) > 5) )
  expect_equivalent( max(tally( ~ sex & substance & homeless, data=mosaicData::HELPrct, margins=TRUE)) , nrow(mosaicData::HELPrct) )
  expect_true( all(tally( ~ sex & substance | homeless, format='percent', data=mosaicData::HELPrct) <= 100) )
  expect_equivalent( 100 * tally( ~ sex & substance | homeless, format='proportion', data=mosaicData::HELPrct),
                          tally( ~ sex & substance | homeless, format='percent', data=mosaicData::HELPrct))
  expect_true( all(tally( ~ sex + substance | homeless, data=mosaicData::HELPrct) > 5) )
  expect_true( all(tally( ~ sex + substance + homeless, data=mosaicData::HELPrct) > 5) )
  expect_equivalent( max(tally( ~ sex + substance + homeless, data=mosaicData::HELPrct, margins=TRUE)) , nrow(mosaicData::HELPrct) )
  expect_true( all(tally( ~ sex + substance | homeless, format='percent', data=mosaicData::HELPrct) <= 100) )
  expect_equivalent( 100 * tally( ~ sex + substance | homeless, format='proportion', data=mosaicData::HELPrct),
                          tally( ~ sex + substance | homeless, format='percent', data=mosaicData::HELPrct))
})

test_that("Subsetting works", {
  expect_equivalent( 
	tally( ~ substance & homeless, mosaicData::HELPrct, subset=sex=='male' ) + 
	tally( ~ substance & homeless, mosaicData::HELPrct, subset=sex=='female' ),
	tally( ~ substance & homeless, mosaicData::HELPrct) 
  )
  expect_equivalent( 
	tally( ~ substance + homeless, mosaicData::HELPrct, subset=sex=='male' ) + 
	tally( ~ substance + homeless, mosaicData::HELPrct, subset=sex=='female' ),
	tally( ~ substance + homeless, mosaicData::HELPrct) 
  )
})

test_that("errors generated for bad formula types", {
  expect_error( prop(sex~homeless&substance, data=mosaicData::HELPrct))
  expect_error( count(sex~homeless&substance, data=mosaicData::HELPrct))
  expect_error( perc(sex~homeless&substance, data=mosaicData::HELPrct))
  expect_error( prop(sex~homeless+substance, data=mosaicData::HELPrct))
  expect_error( count(sex~homeless+substance, data=mosaicData::HELPrct))
  expect_error( perc(sex~homeless+substance, data=mosaicData::HELPrct))
})

test_that("count/perc/prop wrappers work", {
  expect_equivalent( count(~sex, data=mosaicData::HELPrct), sum(mosaicData::HELPrct$sex == 'female') )
  expect_equivalent( prop(~sex, data=mosaicData::HELPrct), sum(mosaicData::HELPrct$sex == 'female') / nrow(mosaicData::HELPrct))
  expect_equivalent( perc(~sex, data=mosaicData::HELPrct), 100 * sum(mosaicData::HELPrct$sex == 'female') / nrow(mosaicData::HELPrct))
})

if(TRUE) { 
test_that("... passes through to table()", {
	x <- c(1,2,2,3,3,3,NA,NA)
	expect_equivalent( length( tally(~x) ), 4  )
	expect_equivalent( length(tally(~x, useNA='ifany') ), 4)
	expect_equivalent( length(tally(~x, useNA='no') ), 3)
	expect_equivalent( length(tally(~x[1:5], useNA='always') ), 4)
	expect_equivalent( length(tally(~x, useNA='ifany', margins=TRUE) ), 5 )
	expect_equivalent( length(tally(~x[1:6], useNA='always')), 4 )
})
}
