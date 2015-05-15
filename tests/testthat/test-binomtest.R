
context("binom.test()")

  TestData <- data.frame( a = factor(rep(letters[1:3], length.out=100)),
                          b = rep(letters[1:3], length.out=100), 
                          stringsAsFactors = FALSE
  )
  
test_that("Formulas work", {
 
  expect_equivalent( 
    interval(stats::binom.test( 34, 100)),
    interval(binom.test(~ a, data=TestData))
  )
  
  expect_equivalent( 
    interval(stats::binom.test( 34, 100)),
    interval(binom.test(~ b, data=TestData))
  )
})

test_that("success = works", {
  expect_equivalent( 
    interval(stats::binom.test( 33, 100)),
    interval(binom.test(~ a, data=TestData, success="b"))
  )
  
  expect_equivalent( 
    interval(stats::binom.test( 33, 100)),
    interval(binom.test(~ b, data=TestData, success="b"))
  )
  
})


test_that("bare vars work", {
  expect_equivalent( 
    interval(stats::binom.test( 33, 100)),
    interval(binom.test(a, data=TestData, success="b"))
  )
  
  expect_equivalent( 
    interval(stats::binom.test( 33, 100)),
    interval(binom.test(b, data=TestData, success="b"))
  )
  
})
