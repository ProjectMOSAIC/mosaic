
context('Aggregating Functions')

test_that("formula interface works", {
  expect_equivalent( count(~sex, data=HELPrct), sum(HELPrct$sex == 'female') )
  expect_equivalent( prop(~sex, data=HELPrct), sum(HELPrct$sex == 'female') / nrow(HELPrct))
  expect_equivalent( mean(~cesd, data=HELPrct), mean(HELPrct$cesd))
  expect_equivalent( var(~cesd, data=HELPrct), var(HELPrct$cesd))
  expect_equivalent( sd(~cesd, data=HELPrct), sd(HELPrct$cesd))
  expect_equivalent( max(~cesd, data=HELPrct), max(HELPrct$cesd))
  expect_equivalent( min(~cesd, data=HELPrct), min(HELPrct$cesd))
})

test_that("data frame interface works", {
  expect_equivalent( count(sex, data=HELPrct), sum(HELPrct$sex == 'female') )
  expect_equivalent( prop(sex, data=HELPrct), sum(HELPrct$sex == 'female') / nrow(HELPrct))
  expect_equivalent( mean(cesd, data=HELPrct), mean(HELPrct$cesd))
  expect_equivalent( var(cesd, data=HELPrct), var(HELPrct$cesd))
  expect_equivalent( sd(cesd, data=HELPrct), sd(HELPrct$cesd))
  expect_equivalent( max(cesd, data=HELPrct), max(HELPrct$cesd))
  expect_equivalent( min(cesd, data=HELPrct), min(HELPrct$cesd))
})

test_that("errors generated for bad formula types", {
  expect_error( count(sex~homeless, data=HELPrct))
  expect_error( prop(sex~homeless, data=HELPrct))
})
