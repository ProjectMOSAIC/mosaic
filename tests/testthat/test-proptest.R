
context("prop.test()")

TestData <- data.frame( a = factor(rep(letters[1:3], length.out=100)),
                        b = rep(letters[1:3], length.out=100), 
                        c = rep(c(TRUE, FALSE, FALSE), length.out=100), 
                        stringsAsFactors = FALSE
)

test_that("formulas work", {
  
  expect_equivalent( 
    interval(stats::prop.test(34, 100)),
    interval(prop.test(~ a, data=TestData))
  )
  
  expect_equivalent( 
    interval(stats::prop.test(34, 100)),
    interval(prop.test(~ b, data=TestData))
  )
  
  expect_equivalent( 
    interval(stats::prop.test(34, 100)),
    interval(prop.test(~ c, data=TestData))
  )
})

test_that("formulas work with unnamed second arg", {
  
  expect_equivalent( 
    interval(stats::prop.test(34, 100)),
    interval(prop.test(~ a, TestData))
  )
  
  expect_equivalent( 
    interval(stats::prop.test(34, 100)),
    interval(prop.test(~ b, TestData))
  )
  
  expect_equivalent( 
    interval(stats::prop.test(34, 100)),
    interval(prop.test(~ c, TestData))
  )
})
test_that("success = works", {
  expect_equivalent( 
    interval(stats::prop.test(33, 100)),
    interval(prop.test(~ a, data=TestData, success="b"))
  )
  
  expect_equivalent( 
    interval(stats::prop.test(33, 100)),
    interval(prop.test(~ b, data=TestData, success="b"))
  )
  
  expect_equivalent( 
    interval(stats::prop.test(66, 100)),
    interval(prop.test(~ c, data=TestData, success=FALSE))
  )
  
})


test_that("bare vars work", {
  expect_equivalent( 
    interval(stats::prop.test(33, 100)),
    interval(prop.test(a, data=TestData, success="b"))
  )
  
  expect_equivalent( 
    interval(stats::prop.test(33, 100)),
    interval(prop.test(b, data=TestData, success="b"))
  )
  
  expect_equivalent( 
    interval(stats::prop.test(34, 100)),
    interval(prop.test(c, data=TestData))
  )
})

test_that("numbers work", {
  expect_equivalent( 
    interval(stats::prop.test(33, 100)),
    interval(prop.test(33,100))
  )
  
})
