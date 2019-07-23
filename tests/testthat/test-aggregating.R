
context('Aggregating Functions')

test_that("formula interface works", {
  expect_equivalent( mean(~cesd, data=mosaicData::HELPrct), mean(mosaicData::HELPrct$cesd))
  expect_equivalent( var(~cesd, data=mosaicData::HELPrct), var(mosaicData::HELPrct$cesd))
  expect_equivalent( sd(~cesd, data=mosaicData::HELPrct), sd(mosaicData::HELPrct$cesd))
  expect_equivalent( max(~cesd, data=mosaicData::HELPrct), max(mosaicData::HELPrct$cesd))
  expect_equivalent( min(~cesd, data=mosaicData::HELPrct), min(mosaicData::HELPrct$cesd))
  expect_equivalent( min(~cesd | sex, data=mosaicData::HELPrct), min(cesd ~ sex, data=mosaicData::HELPrct))
  expect_equivalent( max(~cesd | sex, data=mosaicData::HELPrct), max(cesd ~ sex, data=mosaicData::HELPrct))
  expect_equivalent( mean(~cesd | sex, data=mosaicData::HELPrct), mean(cesd ~ sex, data=mosaicData::HELPrct))
  expect_equivalent( sd(~cesd | sex, data=mosaicData::HELPrct), sd(cesd ~ sex, data=mosaicData::HELPrct))
  expect_equivalent( var(~cesd | sex, data=mosaicData::HELPrct), var(cesd ~ sex, data=mosaicData::HELPrct))
})

# no longer supporting this
# test_that("data frame interface works", {
#   expect_equivalent( mean(cesd, data=mosaicData::HELPrct), mean(mosaicData::HELPrct$cesd))
#   expect_equivalent( var(cesd, data=mosaicData::HELPrct), var(mosaicData::HELPrct$cesd))
#   expect_equivalent( sd(cesd, data=mosaicData::HELPrct), sd(mosaicData::HELPrct$cesd))
#   expect_equivalent( max(cesd, data=mosaicData::HELPrct), max(mosaicData::HELPrct$cesd))
#   expect_equivalent( min(cesd, data=mosaicData::HELPrct), min(mosaicData::HELPrct$cesd))
# })


test_that("formulas work without data", {
  age <<- mosaicData::HELPrct$age
  sex <<- mosaicData::HELPrct$sex
  expect_equivalent( min( age ), min( ~age ))
  expect_equivalent( min( ~ age ), min( ~age, data=mosaicData::HELPrct ))
  expect_equivalent( max( age ), max( ~age ))
  expect_equivalent( max( ~ age ), max( ~age, data=mosaicData::HELPrct ))
  expect_equivalent( sd( age ), sd( ~age ))
  expect_equivalent( sd( ~ age ), sd( ~age, data=mosaicData::HELPrct ))
  expect_equivalent( var( age ), var( ~age ))
  expect_equivalent( var( ~ age ), var( ~age, data=mosaicData::HELPrct ))
  expect_equivalent( median( ~ age ), median( ~age, data=mosaicData::HELPrct ))
  expect_equivalent( median( age ~ sex ), median( age ~ sex, data=mosaicData::HELPrct ))
  expect_equivalent( mean( age ), mean( ~age ))
  expect_equivalent( mean( ~ age ), mean( ~age, data=mosaicData::HELPrct ))
})

test_that("var grabs two vectors from data frame", {
  expect_equivalent(var( ~ age, ~ cesd, data = mosaicData::HELPrct), 
                    var(mosaicData::HELPrct$age, mosaicData::HELPrct$cesd))
})

test_that("na.rm works", {
  x <<- 1:6; y <<- c(1,2,5,6)
  x[3] <- NA
  x[4] <- NA
  expect_equivalent(   mean(x, na.rm=TRUE), mean(y))
  expect_equivalent(     sd(x, na.rm=TRUE), sd(y))
  expect_equivalent(    var(x, na.rm=TRUE), var(y))
  expect_equivalent( median(x, na.rm=TRUE), median(y))
  expect_equivalent(    max(x, na.rm=TRUE), max(y))
  expect_equivalent(    var(x, x, na.rm=TRUE), var(x, x, na.rm=TRUE))
  expect_equivalent(    var(x, 1:6, na.rm=TRUE), stats::var(x, 1:6, na.rm=TRUE))
})

test_that("sum( a + x ) works, etc.", {
  x <<- 1:6; a <<- 11:16
  expect_equivalent( sum( a + x) , base::sum( a + x) )
  expect_equivalent( mean( a + x) , base::mean( a + x) )
  expect_equivalent( median( a + x) , stats::median( a + x) )
})

test_that("cov() and cor() work.", {
  expect_equivalent(cor( age ~ i1, data = HELPrct), cor(HELPrct$age, HELPrct$i1))
  expect_equivalent(cor( ~ age, ~ i1, data = HELPrct), cor(HELPrct$age, HELPrct$i1))
  expect_equivalent(cor( ~ age | i1, data = HELPrct), cor(HELPrct$age, HELPrct$i1))
  expect_equivalent(cov( age ~ i1, data = HELPrct), cov(HELPrct$age, HELPrct$i1))
  expect_equivalent(cov( ~ age, ~ i1, data = HELPrct), cov(HELPrct$age, HELPrct$i1))
  expect_equivalent(cov( ~ age | i1, data = HELPrct), cov(HELPrct$age, HELPrct$i1))
})

test_that("var() works.", {
  expect_equivalent(var( age ~ sex, data = HELPrct), 
                    aggregate(HELPrct$age, by = list(HELPrct$sex), FUN = var)$x)
  expect_equivalent(var( ~ age, ~ i1, data = HELPrct), var(HELPrct$age, HELPrct$i1))
  expect_equivalent(var( ~ age | sex, data = HELPrct), var(age ~ sex, data = HELPrct))
})

# no longer supporting this
# test_that("bare names work", {
#   expect_equivalent( mean(   age, data = HELPrct), 
#                      mean( ~ age, data = HELPrct) )
#   expect_equivalent( cor( age,  cesd, data = HELPrct), 
#                      cor( age ~ cesd, data = HELPrct) )
#   expect_equivalent( var(  age, data = HELPrct), 
#                      var( ~ age, data = HELPrct) )
#   expect_equivalent( favstats(   age, data = HELPrct), 
#                      favstats( ~ age, data = HELPrct) )
# })

