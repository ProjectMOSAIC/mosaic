
context('Aggregating Functions')

test_that("formula interface works", {
  expect_equivalent( mean(~cesd, data=HELPrct), mean(HELPrct$cesd))
  expect_equivalent( var(~cesd, data=HELPrct), var(HELPrct$cesd))
  expect_equivalent( sd(~cesd, data=HELPrct), sd(HELPrct$cesd))
  expect_equivalent( max(~cesd, data=HELPrct), max(HELPrct$cesd))
  expect_equivalent( min(~cesd, data=HELPrct), min(HELPrct$cesd))
  expect_equivalent( min(~cesd | sex, data=HELPrct), min(cesd ~ sex, data=HELPrct))
  expect_equivalent( max(~cesd | sex, data=HELPrct), max(cesd ~ sex, data=HELPrct))
  expect_equivalent( mean(~cesd | sex, data=HELPrct), mean(cesd ~ sex, data=HELPrct))
  expect_equivalent( sd(~cesd | sex, data=HELPrct), sd(cesd ~ sex, data=HELPrct))
  expect_equivalent( var(~cesd | sex, data=HELPrct), var(cesd ~ sex, data=HELPrct))
})

test_that("data frame interface works", {
  expect_equivalent( mean(cesd, data=HELPrct), mean(HELPrct$cesd))
  expect_equivalent( var(cesd, data=HELPrct), var(HELPrct$cesd))
  expect_equivalent( sd(cesd, data=HELPrct), sd(HELPrct$cesd))
  expect_equivalent( max(cesd, data=HELPrct), max(HELPrct$cesd))
  expect_equivalent( min(cesd, data=HELPrct), min(HELPrct$cesd))
})


test_that("formulas work without data", {
  age <<- HELPrct$age
  sex <<- HELPrct$sex
  expect_equivalent( min( age ), min( ~age ))
  expect_equivalent( min( ~ age ), min( ~age, data=HELPrct ))
  expect_equivalent( max( age ), max( ~age ))
  expect_equivalent( max( ~ age ), max( ~age, data=HELPrct ))
  expect_equivalent( sd( age ), sd( ~age ))
  expect_equivalent( sd( ~ age ), sd( ~age, data=HELPrct ))
  expect_equivalent( var( age ), var( ~age ))
  expect_equivalent( var( ~ age ), var( ~age, data=HELPrct ))
  # expect_equivalent( median( ~ age ), median( ~age, data=HELPrct ))
  # expect_equivalent( median( age ~ sex ), median( age ~ sex, data=HELPrct ))
  expect_equivalent( mean( age ), mean( ~age ))
  expect_equivalent( mean( ~ age ), mean( ~age, data=HELPrct ))
})

test_that("var grabs two vectors from data frame", {
  expect_equivalent( var( age, cesd, data=HELPrct ), var( HELPrct$age, HELPrct$cesd) )
})

test_that("na.rm works", {
  x <- 1:6; y <- c(1,2,5,6)
  x[3] <- NA
  x[4] <- NA
  expect_equivalent( mean( x, na.rm=TRUE ), mean(y))
  expect_equivalent( sd( x, na.rm=TRUE ), sd(y))
  expect_equivalent( var( x, na.rm=TRUE ), var(y))
  expect_equivalent( var( x, x, na.rm=TRUE ), var(x,x,na.rm=TRUE))
  expect_equivalent( var( x, 1:6, na.rm=TRUE ), stats::var(x,1:6,na.rm=TRUE))
  expect_equivalent( median( x, na.rm=TRUE ), median(y))
  expect_equivalent( max( x, na.rm=TRUE ), max(y))
})

