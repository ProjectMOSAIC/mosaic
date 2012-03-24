
context('Aggregating Functions')

test_that("formula interface works", {
  expect_equivalent( mean(~cesd, data=HELPrct), mean(HELPrct$cesd))
  expect_equivalent( var(~cesd, data=HELPrct), var(HELPrct$cesd))
  expect_equivalent( sd(~cesd, data=HELPrct), sd(HELPrct$cesd))
  expect_equivalent( max(~cesd, data=HELPrct), max(HELPrct$cesd))
  expect_equivalent( min(~cesd, data=HELPrct), min(HELPrct$cesd))
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
  expect_equivalent( min( ~ age ), min( ~age, HELPrct ))
  expect_equivalent( max( age ), max( ~age ))
  expect_equivalent( max( ~ age ), max( ~age, HELPrct ))
  expect_equivalent( sd( age ), sd( ~age ))
  expect_equivalent( sd( ~ age ), sd( ~age, HELPrct ))
  expect_equivalent( var( age ), var( ~age ))
  expect_equivalent( var( ~ age ), var( ~age, HELPrct ))
  expect_equivalent( median( ~ age ), median( ~age, HELPrct ))
  expect_equivalent( median( age ~ sex ), median( age ~ sex, data=HELPrct ))
  expect_equivalent( mean( age ), mean( ~age ))
  expect_equivalent( mean( ~ age ), mean( ~age, HELPrct ))
})

