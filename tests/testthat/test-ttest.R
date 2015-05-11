
context("t.test")

Boys <- filter(mosaicData::Galton, sex=="M")

test_that("2-sample tests give same results as stats::t.test", {
  
  expect_equivalent( 
    interval(stats::t.test(height ~ sex, data=mosaicData::Galton)),
    interval(t.test(height ~ sex, data=mosaicData::Galton))
  )
  
  expect_equivalent( 
    interval(stats::t.test(Boys$father, Boys$height)),
    interval(t.test(Boys$father, Boys$height))
  )
  
  expect_equivalent( 
    interval(stats::t.test(Boys$father, Boys$height)),
    interval(with(Boys, t.test(father, height)))
  )
  
  expect_equivalent( 
    interval(stats::t.test(Boys$father, Boys$height)),
    interval(t.test(father, height, data=Boys))
  )
})

test_that("paired tests give same results as stats::t.test", {
  
  expect_equivalent( 
    interval(stats::t.test(Boys$father, Boys$height, paired=TRUE)),
    interval(t.test(father, height, data=Boys, paired=TRUE))
  )
  
  expect_equivalent( 
    interval(stats::t.test(Boys$father, Boys$height, paired=TRUE)),
    interval(t.test(Boys$father, Boys$height, paired=TRUE))
  )
  
  expect_equivalent( 
    interval(stats::t.test(Boys$father, Boys$height, paired=TRUE)),
    interval(t.test(~ (father - height), data=Boys))
  )
  
  expect_equivalent( 
    interval(stats::t.test(Boys$father, Boys$height, paired=TRUE)),
    interval(with(Boys, t.test(father, height, paired=TRUE)))
  )
})

test_that("1-sample tests give same results as stats::t.test", {
  
  expect_equivalent(
    interval(stats::t.test(Boys$height)),
    interval(t.test(~ height, data=Boys))
  )
  
  expect_equivalent(
    interval(stats::t.test(Boys$height)),
    interval(t.test(height, data=Boys))
  )
  
  expect_equivalent(
    interval(stats::t.test(Boys$height)),
    interval(t.test(Boys$height))
  )
  
  expect_equivalent(
    interval(stats::t.test(Boys$height)),
    interval(with(Boys, t.test(height)))
  )
})