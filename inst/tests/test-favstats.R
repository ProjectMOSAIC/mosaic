
context('favstats')

test_that("favstats works for vectors ", {
  expect_equivalent( favstats(1:10)$mean,  mean(1:10) )
  expect_equivalent( favstats(1:10)$sd,  sd(1:10) )
  expect_equivalent( favstats(1:10)$min,  min(1:10) )
  expect_equivalent( favstats(1:10)$max,  max(1:10) )
  expect_equivalent( favstats(1:10)$Q1,  quantile(1:10)[2] )
  expect_equivalent( favstats(1:10)$Q3,  quantile(1:10)[4] )
})

test_that("data interface works", {
  expect_equivalent( favstats(HELPrct$age), favstats(age, data=HELPrct) )
})

test_that("formula interface works", {
  expect_equivalent( favstats(HELPrct$age), favstats(~age, data=HELPrct) )
})

test_that("formulas work without data", {
  expect_equivalent( favstats(1:10), favstats(~1:10) )
})


test_that("missing data handled correctly", {
  myHELP <- HELPrct
  myHELP$age[1] <- NA
  expect_equivalent( favstats(myHELP$age)$missing, 1 ) 
  expect_equivalent( favstats(myHELP$age)$mean, mean(HELPrct$age[-1]) )
  expect_equivalent( favstats(myHELP$age)$sd, sd(HELPrct$age[-1]) )
})


